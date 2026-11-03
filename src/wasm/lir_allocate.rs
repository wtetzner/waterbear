//! Deterministic physical storage assignment for VMU LIR.
//!
//! This is intentionally a simple first allocator. It gives every live LIR
//! object a stable location for the whole program, packs bit values, and spills
//! byte values to the VMU work RAM when application RAM is full. A later
//! allocator can reuse locations based on liveness without changing assembly
//! instruction selection.

use std::{error::Error, fmt};

use super::lir::{
    AllocatedProgram, BitIndex, PhysicalPlacement, Program, Storage, StorageKind, StorageRegion,
    VirtualPlacement, VirtualProgram, WorkRamOffset,
};

const APPLICATION_RAM_BYTES: usize = 0x100;
const WORK_RAM_BYTES: usize = 0x200;

// 00h-0fh contain the four banks of indirect registers. The assembly backend
// uses bank zero's R2 and R3. 10h-1fh are backend scratch bytes.
const FIRST_APPLICATION_BYTE: usize = 0x20;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AllocationError {
    ApplicationRamExhausted { bytes: usize },
    WorkRamExhausted { bytes: usize },
    NoAllowedPlacement { storage: usize },
    InvalidFixedPlacement { storage: usize },
    FixedPlacementConflict { storage: usize },
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
        }
    }
}

impl Error for AllocationError {}

pub fn allocate(program: &VirtualProgram) -> Result<AllocatedProgram, AllocationError> {
    Allocator::new().allocate(program)
}

struct Allocator {
    application: [u8; APPLICATION_RAM_BYTES],
    work: [u8; WORK_RAM_BYTES],
}

impl Allocator {
    fn new() -> Self {
        let mut application = [0; APPLICATION_RAM_BYTES];
        application[..FIRST_APPLICATION_BYTE].fill(u8::MAX);
        Self {
            application,
            work: [0; WORK_RAM_BYTES],
        }
    }

    fn allocate(mut self, program: &VirtualProgram) -> Result<AllocatedProgram, AllocationError> {
        let mut storages = Vec::with_capacity(program.storages.len());
        for storage in &program.storages {
            let placement = self.allocate_storage(storage)?;
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
            PhysicalPlacement::Accumulator
            | PhysicalPlacement::CarryFlag
            | PhysicalPlacement::SpecialRegister(_)
            | PhysicalPlacement::IndirectRegister { .. }
            | PhysicalPlacement::Flash { .. }
            | PhysicalPlacement::XRam { .. } => length == 1,
        };

        result
            .then_some(())
            .ok_or(AllocationError::FixedPlacementConflict {
                storage: storage.id.0,
            })
    }
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
                | PhysicalPlacement::Flash { .. }
                | PhysicalPlacement::XRam { .. }
        ),
        StorageKind::Bytes { length } => {
            length.get() == 1
                || matches!(
                    placement,
                    PhysicalPlacement::ApplicationRam { .. }
                        | PhysicalPlacement::ShadowStack { .. }
                        | PhysicalPlacement::WorkRam { .. }
                        | PhysicalPlacement::Flash { .. }
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
    use crate::wasm::lir::{
        Block, BlockId, Function, FunctionAbi, FunctionKind, InterruptHandlers, PhysicalPlacement,
        Program, Storage, StorageId, StorageKind, StorageOwner, StorageRegion, Terminator,
        TerminatorKind, VirtualPlacement,
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
}
