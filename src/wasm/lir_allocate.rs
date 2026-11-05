//! Liveness-based physical storage assignment for VMU LIR.
//!
//! Function-owned values with disjoint live ranges share physical slots, with
//! copy-related values preferred for coalescing. Program-owned and
//! address-taken objects retain stable locations. Bit values are packed, and
//! byte values spill to VMU work RAM when application RAM is full.
//!
//! Function-owned WRAM is currently assigned statically. The LIR verifier
//! rejects reentrant call graphs so this temporary representation cannot
//! silently overwrite a live invocation.

use std::{collections::BTreeSet, error::Error, fmt};

use super::{
    lir::{
        AllocatedProgram, BitIndex, PhysicalPlacement, Program, Storage, StorageId, StorageKind,
        StorageOwner, StorageRegion, VirtualPlacement, VirtualProgram, WorkRamOffset,
    },
    lir_analysis::{InterferenceGraph, interference_graph},
    target::{APPLICATION_RAM_BYTES, FIRST_ALLOCATABLE_APPLICATION_BYTE, WORK_RAM_BYTES},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AllocationError {
    ApplicationRamExhausted { bytes: usize },
    WorkRamExhausted { bytes: usize },
    NoAllowedPlacement { storage: usize },
    InvalidFixedPlacement { storage: usize },
    FixedPlacementConflict { storage: usize },
    InvalidProgram(String),
}

impl fmt::Display for AllocationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ApplicationRamExhausted { bytes } => {
                write!(
                    f,
                    "VMU application RAM cannot fit a {bytes}-byte direct object"
                )
            }
            Self::WorkRamExhausted { bytes } => {
                write!(f, "VMU work RAM cannot fit a {bytes}-byte object")
            }
            Self::NoAllowedPlacement { storage } => {
                write!(f, "LIR storage %s{storage} has no allocatable VMU region")
            }
            Self::InvalidFixedPlacement { storage } => {
                write!(
                    f,
                    "fixed placement for LIR storage %s{storage} does not match its kind"
                )
            }
            Self::FixedPlacementConflict { storage } => {
                write!(
                    f,
                    "fixed placement for LIR storage %s{storage} overlaps another object"
                )
            }
            Self::InvalidProgram(message) => write!(f, "invalid VMU LIR: {message}"),
        }
    }
}

impl Error for AllocationError {}

pub fn allocate(program: &VirtualProgram) -> Result<AllocatedProgram, AllocationError> {
    super::lir_validate::validate_virtual(program)
        .map_err(|error| AllocationError::InvalidProgram(error.to_string()))?;
    let program = Allocator::new(program).allocate(program)?;
    super::lir_validate::validate_allocated(&program)
        .map_err(|error| AllocationError::InvalidProgram(error.to_string()))?;
    Ok(program)
}

struct Allocator {
    application: [u8; APPLICATION_RAM_BYTES],
    work: [u8; WORK_RAM_BYTES],
    interference: InterferenceGraph,
    copies: BTreeSet<(StorageId, StorageId)>,
    slots: Vec<AllocationSlot>,
}

struct AllocationSlot {
    owner: StorageOwner,
    kind: StorageKind,
    placement: PhysicalPlacement,
    members: Vec<StorageId>,
}

impl Allocator {
    fn new(program: &VirtualProgram) -> Self {
        let mut application = [0; APPLICATION_RAM_BYTES];
        application[..FIRST_ALLOCATABLE_APPLICATION_BYTE].fill(u8::MAX);
        let (interference, copies) = interference_graph(program);
        Self {
            application,
            work: [0; WORK_RAM_BYTES],
            interference,
            copies,
            slots: Vec::new(),
        }
    }

    fn allocate(mut self, program: &VirtualProgram) -> Result<AllocatedProgram, AllocationError> {
        let mut storages = Vec::with_capacity(program.storages.len());
        for storage in &program.storages {
            let placement = if let Some(slot) = self.reusable_slot(storage) {
                self.slots[slot].members.push(storage.id);
                self.slots[slot].placement
            } else {
                let placement = self.allocate_storage(storage)?;
                self.record_slot(storage, placement);
                placement
            };
            storages.push(Storage {
                id: storage.id,
                name: storage.name.clone(),
                owner: storage.owner.clone(),
                kind: storage.kind,
                placement,
            });
        }

        Ok(Program {
            main: program.main.clone(),
            interrupt_handlers: program.interrupt_handlers.clone(),
            functions: program.functions.clone(),
            statics: program.statics.clone(),
            storages,
        })
    }

    fn reusable_slot(&self, storage: &Storage<VirtualPlacement>) -> Option<usize> {
        if storage.placement.fixed.is_some()
            || !matches!(storage.owner, StorageOwner::Function(_))
            || storage.kind == StorageKind::Carry
        {
            return None;
        }

        let fits = |slot: &AllocationSlot| {
            slot.owner == storage.owner
                && slot.kind == storage.kind
                && placement_allowed(slot.placement, &storage.placement)
                && slot.members.iter().all(|member| {
                    !self
                        .interference
                        .get(&storage.id)
                        .is_some_and(|neighbors| neighbors.contains(member))
                })
        };

        self.slots
            .iter()
            .enumerate()
            .find(|(_, slot)| {
                fits(slot)
                    && slot
                        .members
                        .iter()
                        .any(|member| self.copies.contains(&ordered_pair(storage.id, *member)))
            })
            .or_else(|| self.slots.iter().enumerate().find(|(_, slot)| fits(slot)))
            .map(|(index, _)| index)
    }

    fn record_slot(&mut self, storage: &Storage<VirtualPlacement>, placement: PhysicalPlacement) {
        if storage.placement.fixed.is_none()
            && matches!(storage.owner, StorageOwner::Function(_))
            && storage.kind != StorageKind::Carry
        {
            self.slots.push(AllocationSlot {
                owner: storage.owner.clone(),
                kind: storage.kind,
                placement,
                members: vec![storage.id],
            });
        }
    }

    fn allocate_storage(
        &mut self,
        storage: &Storage<VirtualPlacement>,
    ) -> Result<PhysicalPlacement, AllocationError> {
        if let Some(fixed) = storage.placement.fixed {
            self.reserve_fixed(storage, fixed)?;
            return Ok(fixed);
        }

        if storage.kind == StorageKind::Carry
            && storage
                .placement
                .allowed_regions
                .contains(&StorageRegion::CarryFlag)
        {
            return Ok(PhysicalPlacement::CarryFlag);
        }

        match storage.kind {
            StorageKind::Bit | StorageKind::Carry => self.allocate_bit(storage),
            StorageKind::Byte => self.allocate_bytes(storage, 1),
            StorageKind::Bytes { length } => self.allocate_bytes(storage, length.get()),
        }
    }

    fn allocate_bit(
        &mut self,
        storage: &Storage<VirtualPlacement>,
    ) -> Result<PhysicalPlacement, AllocationError> {
        let regions = &storage.placement.allowed_regions;
        let application_bit = regions
            .contains(&StorageRegion::ApplicationRam)
            .then(|| allocate_bit(&mut self.application))
            .flatten();
        if let Some((offset, bit)) = application_bit {
            return Ok(PhysicalPlacement::ApplicationBit {
                offset: offset as u8,
                bit: BitIndex::new(bit).unwrap(),
            });
        }

        let shadow = regions.contains(&StorageRegion::ShadowStack);
        if shadow || regions.contains(&StorageRegion::WorkRam) {
            if let Some((offset, bit)) = allocate_bit(&mut self.work) {
                let frame_offset = WorkRamOffset::new(offset as u16).unwrap();
                return Ok(if shadow {
                    PhysicalPlacement::ShadowStackBit {
                        frame_offset,
                        bit: BitIndex::new(bit).unwrap(),
                    }
                } else {
                    PhysicalPlacement::WorkRamBit {
                        offset: frame_offset,
                        bit: BitIndex::new(bit).unwrap(),
                    }
                });
            }
            return Err(AllocationError::WorkRamExhausted { bytes: 1 });
        }

        Err(AllocationError::NoAllowedPlacement {
            storage: storage.id.0,
        })
    }

    fn allocate_bytes(
        &mut self,
        storage: &Storage<VirtualPlacement>,
        length: usize,
    ) -> Result<PhysicalPlacement, AllocationError> {
        let regions = &storage.placement.allowed_regions;
        if regions.contains(&StorageRegion::ApplicationRam) {
            if let Some(offset) = allocate_contiguous(&mut self.application, length) {
                return Ok(PhysicalPlacement::ApplicationRam {
                    offset: offset as u8,
                });
            }
            if storage.placement.must_be_direct {
                return Err(AllocationError::ApplicationRamExhausted { bytes: length });
            }
        }

        if regions.contains(&StorageRegion::Flash) {
            return Ok(PhysicalPlacement::Flash);
        }

        let shadow = regions.contains(&StorageRegion::ShadowStack);
        if shadow || regions.contains(&StorageRegion::WorkRam) {
            let Some(offset) = allocate_contiguous(&mut self.work, length) else {
                return Err(AllocationError::WorkRamExhausted { bytes: length });
            };
            let offset = WorkRamOffset::new(offset as u16).unwrap();
            return Ok(if shadow {
                PhysicalPlacement::ShadowStack {
                    frame_offset: offset,
                }
            } else {
                PhysicalPlacement::WorkRam { offset }
            });
        }

        Err(AllocationError::NoAllowedPlacement {
            storage: storage.id.0,
        })
    }

    fn reserve_fixed(
        &mut self,
        storage: &Storage<VirtualPlacement>,
        placement: PhysicalPlacement,
    ) -> Result<(), AllocationError> {
        if !fixed_kind_matches(storage.kind, placement) {
            return Err(AllocationError::InvalidFixedPlacement {
                storage: storage.id.0,
            });
        }
        let length = storage_length(storage.kind);
        let result = match placement {
            PhysicalPlacement::ApplicationRam { offset } => {
                reserve_bytes(&mut self.application, offset as usize, length)
            }
            PhysicalPlacement::ApplicationBit { offset, bit } => {
                reserve_bit(&mut self.application, offset as usize, bit.get())
            }
            PhysicalPlacement::ShadowStack { frame_offset } => {
                reserve_bytes(&mut self.work, frame_offset.get() as usize, length)
            }
            PhysicalPlacement::ShadowStackBit { frame_offset, bit } => {
                reserve_bit(&mut self.work, frame_offset.get() as usize, bit.get())
            }
            PhysicalPlacement::WorkRam { offset } => {
                reserve_bytes(&mut self.work, offset.get() as usize, length)
            }
            PhysicalPlacement::WorkRamBit { offset, bit } => {
                reserve_bit(&mut self.work, offset.get() as usize, bit.get())
            }
            PhysicalPlacement::Flash => true,
            PhysicalPlacement::Accumulator
            | PhysicalPlacement::CarryFlag
            | PhysicalPlacement::SpecialRegister(_)
            | PhysicalPlacement::IndirectRegister { .. }
            | PhysicalPlacement::XRam { .. } => length == 1,
        };

        result
            .then_some(())
            .ok_or(AllocationError::FixedPlacementConflict {
                storage: storage.id.0,
            })
    }
}

fn ordered_pair(left: StorageId, right: StorageId) -> (StorageId, StorageId) {
    if left <= right {
        (left, right)
    } else {
        (right, left)
    }
}

fn placement_allowed(placement: PhysicalPlacement, requested: &VirtualPlacement) -> bool {
    let region = match placement {
        PhysicalPlacement::Accumulator => StorageRegion::Accumulator,
        PhysicalPlacement::CarryFlag => StorageRegion::CarryFlag,
        PhysicalPlacement::ApplicationRam { .. } | PhysicalPlacement::ApplicationBit { .. } => {
            StorageRegion::ApplicationRam
        }
        PhysicalPlacement::ShadowStack { .. } | PhysicalPlacement::ShadowStackBit { .. } => {
            StorageRegion::ShadowStack
        }
        PhysicalPlacement::WorkRam { .. } | PhysicalPlacement::WorkRamBit { .. } => {
            StorageRegion::WorkRam
        }
        PhysicalPlacement::SpecialRegister(_) => StorageRegion::SpecialRegister,
        PhysicalPlacement::IndirectRegister { .. } => StorageRegion::IndirectRegister,
        PhysicalPlacement::Flash => StorageRegion::Flash,
        PhysicalPlacement::XRam { .. } => StorageRegion::XRam,
    };
    requested.allowed_regions.contains(&region)
        && (!requested.must_be_direct
            || matches!(
                placement,
                PhysicalPlacement::Accumulator
                    | PhysicalPlacement::CarryFlag
                    | PhysicalPlacement::ApplicationRam { .. }
                    | PhysicalPlacement::ApplicationBit { .. }
                    | PhysicalPlacement::SpecialRegister(_)
                    | PhysicalPlacement::IndirectRegister { .. }
            ))
}

fn storage_length(kind: StorageKind) -> usize {
    match kind {
        StorageKind::Bit | StorageKind::Byte | StorageKind::Carry => 1,
        StorageKind::Bytes { length } => length.get(),
    }
}

fn fixed_kind_matches(kind: StorageKind, placement: PhysicalPlacement) -> bool {
    match kind {
        StorageKind::Bit | StorageKind::Carry => matches!(
            placement,
            PhysicalPlacement::ApplicationBit { .. }
                | PhysicalPlacement::ShadowStackBit { .. }
                | PhysicalPlacement::WorkRamBit { .. }
                | PhysicalPlacement::CarryFlag
        ),
        StorageKind::Byte => matches!(
            placement,
            PhysicalPlacement::Accumulator
                | PhysicalPlacement::ApplicationRam { .. }
                | PhysicalPlacement::ShadowStack { .. }
                | PhysicalPlacement::SpecialRegister(_)
                | PhysicalPlacement::IndirectRegister { .. }
                | PhysicalPlacement::WorkRam { .. }
                | PhysicalPlacement::Flash
                | PhysicalPlacement::XRam { .. }
        ),
        StorageKind::Bytes { length } => {
            length.get() == 1
                || matches!(
                    placement,
                    PhysicalPlacement::ApplicationRam { .. }
                        | PhysicalPlacement::ShadowStack { .. }
                        | PhysicalPlacement::WorkRam { .. }
                        | PhysicalPlacement::Flash
                        | PhysicalPlacement::XRam { .. }
                )
        }
    }
}

fn allocate_contiguous(bytes: &mut [u8], length: usize) -> Option<usize> {
    if length == 0 || length > bytes.len() {
        return None;
    }
    let offset = bytes
        .windows(length)
        .position(|window| window.iter().all(|byte| *byte == 0))?;
    bytes[offset..offset + length].fill(u8::MAX);
    Some(offset)
}

fn allocate_bit(bytes: &mut [u8]) -> Option<(usize, u8)> {
    for (offset, byte) in bytes.iter_mut().enumerate().rev() {
        if *byte == u8::MAX {
            continue;
        }
        let bit = (!*byte).trailing_zeros() as u8;
        *byte |= 1 << bit;
        return Some((offset, bit));
    }
    None
}

fn reserve_bytes(bytes: &mut [u8], offset: usize, length: usize) -> bool {
    let Some(range) = bytes.get_mut(offset..offset.saturating_add(length)) else {
        return false;
    };
    if range.iter().any(|byte| *byte != 0) {
        return false;
    }
    range.fill(u8::MAX);
    true
}

fn reserve_bit(bytes: &mut [u8], offset: usize, bit: u8) -> bool {
    let Some(byte) = bytes.get_mut(offset) else {
        return false;
    };
    let mask = 1 << bit;
    if *byte & mask != 0 {
        return false;
    }
    *byte |= mask;
    true
}

#[cfg(test)]
mod tests {
    use std::num::NonZeroUsize;

    use super::allocate;
    use crate::wasm::intrinsics::{Intrinsic, Sfr};
    use crate::wasm::lir::{
        Block, BlockId, ByteOperand, ByteStorage, Function, FunctionAbi, FunctionKind, Instruction,
        InstructionKind, InterruptHandlers, PhysicalPlacement, Program, ShiftOp, Static,
        StaticByte, Storage, StorageId, StorageKind, StorageOwner, StorageRegion, Terminator,
        TerminatorKind, ValueOperand, VirtualPlacement,
    };

    #[test]
    fn packs_bits_and_spills_large_objects_to_work_ram() {
        let function = Function {
            name: "main".into(),
            kind: FunctionKind::Normal,
            abi: FunctionAbi::default(),
            blocks: vec![Block {
                id: BlockId(0),
                name: None,
                instructions: Vec::new(),
                terminator: Terminator {
                    kind: TerminatorKind::Return,
                },
            }],
            entry: BlockId(0),
        };
        let placement = VirtualPlacement {
            allowed_regions: vec![StorageRegion::ApplicationRam, StorageRegion::WorkRam],
            must_be_direct: false,
            fixed: None,
        };
        let program = Program {
            main: function,
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
            storages: vec![
                Storage {
                    id: StorageId(0),
                    name: None,
                    owner: StorageOwner::Program,
                    kind: StorageKind::Bit,
                    placement: placement.clone(),
                },
                Storage {
                    id: StorageId(1),
                    name: None,
                    owner: StorageOwner::Program,
                    kind: StorageKind::Bit,
                    placement: placement.clone(),
                },
                Storage {
                    id: StorageId(2),
                    name: None,
                    owner: StorageOwner::Program,
                    kind: StorageKind::Bytes {
                        length: NonZeroUsize::new(225).unwrap(),
                    },
                    placement,
                },
            ],
        };

        let allocated = allocate(&program).unwrap();
        let PhysicalPlacement::ApplicationBit {
            offset: first_offset,
            bit: first_bit,
        } = allocated.storages[0].placement
        else {
            panic!("first boolean was not packed in application RAM")
        };
        let PhysicalPlacement::ApplicationBit {
            offset: second_offset,
            bit: second_bit,
        } = allocated.storages[1].placement
        else {
            panic!("second boolean was not packed in application RAM")
        };
        assert_eq!(first_offset, second_offset);
        assert_ne!(first_bit, second_bit);
        assert!(matches!(
            allocated.storages[2].placement,
            PhysicalPlacement::WorkRam { .. }
        ));
    }

    #[test]
    fn places_read_only_program_objects_in_flash() {
        let function = Function {
            name: "main".into(),
            kind: FunctionKind::Normal,
            abi: FunctionAbi::default(),
            blocks: vec![Block {
                id: BlockId(0),
                name: None,
                instructions: Vec::new(),
                terminator: Terminator {
                    kind: TerminatorKind::Return,
                },
            }],
            entry: BlockId(0),
        };
        let program = Program {
            main: function,
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: vec![Static {
                name: "message".into(),
                storage: StorageId(0),
                initializer: vec![StaticByte::Value(0); 200],
                mutable: false,
            }],
            storages: vec![Storage {
                id: StorageId(0),
                name: Some("message".into()),
                owner: StorageOwner::Program,
                kind: StorageKind::Bytes {
                    length: NonZeroUsize::new(200).unwrap(),
                },
                placement: VirtualPlacement {
                    allowed_regions: vec![StorageRegion::Flash, StorageRegion::WorkRam],
                    must_be_direct: false,
                    fixed: None,
                },
            }],
        };

        let allocated = allocate(&program).unwrap();

        assert_eq!(allocated.storages[0].placement, PhysicalPlacement::Flash);
    }

    #[test]
    fn reuses_a_function_slot_for_noninterfering_values() {
        let function = Function {
            name: "main".into(),
            kind: FunctionKind::Normal,
            abi: FunctionAbi::default(),
            blocks: vec![Block {
                id: BlockId(0),
                name: None,
                instructions: vec![
                    Instruction {
                        kind: InstructionKind::CopyByte {
                            destination: ByteStorage {
                                storage: StorageId(0),
                                offset: 0,
                            },
                            source: ByteOperand::Immediate(1),
                        },
                    },
                    Instruction {
                        kind: InstructionKind::CopyByte {
                            destination: ByteStorage {
                                storage: StorageId(1),
                                offset: 0,
                            },
                            source: ByteOperand::Immediate(2),
                        },
                    },
                ],
                terminator: Terminator {
                    kind: TerminatorKind::Return,
                },
            }],
            entry: BlockId(0),
        };
        let placement = VirtualPlacement {
            allowed_regions: vec![StorageRegion::ApplicationRam],
            must_be_direct: false,
            fixed: None,
        };
        let program = Program {
            main: function,
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
            storages: (0..2)
                .map(|id| Storage {
                    id: StorageId(id),
                    name: Some(format!("temporary{id}")),
                    owner: StorageOwner::Function("main".into()),
                    kind: StorageKind::Byte,
                    placement: placement.clone(),
                })
                .collect(),
        };

        let allocated = allocate(&program).unwrap();
        assert_eq!(
            allocated.storages[0].placement,
            allocated.storages[1].placement
        );
    }

    #[test]
    fn keeps_shift_inputs_separate_from_the_output() {
        let byte = |storage| ByteStorage {
            storage: StorageId(storage),
            offset: 0,
        };
        let function = Function {
            name: "main".into(),
            kind: FunctionKind::Normal,
            abi: FunctionAbi::default(),
            blocks: vec![Block {
                id: BlockId(0),
                name: None,
                instructions: vec![
                    Instruction {
                        kind: InstructionKind::CopyByte {
                            destination: byte(0),
                            source: ByteOperand::Immediate(0xff),
                        },
                    },
                    Instruction {
                        kind: InstructionKind::CopyByte {
                            destination: byte(1),
                            source: ByteOperand::Immediate(3),
                        },
                    },
                    Instruction {
                        kind: InstructionKind::ShiftBytes {
                            op: ShiftOp::RightUnsigned,
                            destinations: vec![byte(2)],
                            value: vec![ByteOperand::Storage(byte(0))],
                            amount: vec![ByteOperand::Storage(byte(1))],
                            width: 8,
                        },
                    },
                    Instruction {
                        kind: InstructionKind::Intrinsic {
                            operation: Intrinsic::SfrWrite(Sfr::LcdContrastControl),
                            inputs: vec![ValueOperand::Byte(ByteOperand::Storage(byte(2)))],
                            outputs: Vec::new(),
                        },
                    },
                ],
                terminator: Terminator {
                    kind: TerminatorKind::Return,
                },
            }],
            entry: BlockId(0),
        };
        let placement = VirtualPlacement {
            allowed_regions: vec![StorageRegion::ApplicationRam],
            must_be_direct: false,
            fixed: None,
        };
        let program = Program {
            main: function,
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
            storages: (0..3)
                .map(|id| Storage {
                    id: StorageId(id),
                    name: Some(format!("value{id}")),
                    owner: StorageOwner::Function("main".into()),
                    kind: StorageKind::Byte,
                    placement: placement.clone(),
                })
                .collect(),
        };

        let allocated = allocate(&program).expect("allocation succeeds");
        let value = allocated.storages[0].placement;
        let amount = allocated.storages[1].placement;
        let output = allocated.storages[2].placement;

        assert_ne!(output, value);
        assert_ne!(output, amount);
    }
}
