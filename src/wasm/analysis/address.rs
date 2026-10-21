use std::convert::Infallible;

use walrus::{
    FunctionId, GlobalId, GlobalKind, Module, ValType,
    ir::{BinaryOp, Value},
};

use super::{
    domain::AbstractDomain,
    forward::{ForwardAnalysis, RootKind, TransferContext},
    index::ValueProducer,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum AddressBase {
    ShadowStack(FunctionId),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AddressFact {
    Bottom(Option<ValType>),
    Constant {
        value: i64,
        ty: ValType,
    },
    Affine {
        base: AddressBase,
        ty: ValType,
        minimum_offset: i64,
        maximum_offset: i64,
    },
    Unknown(Option<ValType>),
}

impl AddressFact {
    pub fn stack_range(&self, function: FunctionId) -> Option<(i64, i64)> {
        match self {
            Self::Affine {
                base: AddressBase::ShadowStack(owner),
                ty: _,
                minimum_offset,
                maximum_offset,
            } if *owner == function => Some((*minimum_offset, *maximum_offset)),
            _ => None,
        }
    }
}

impl AbstractDomain for AddressFact {
    fn bottom(ty: Option<ValType>) -> Self {
        Self::Bottom(ty)
    }

    fn top(ty: ValType) -> Self {
        Self::Unknown(Some(ty))
    }

    fn is_bottom(&self) -> bool {
        matches!(self, Self::Bottom(_))
    }

    fn join_assign(&mut self, other: &Self) -> bool {
        let previous = self.clone();
        match (&mut *self, other) {
            (left @ Self::Bottom(_), right) => *left = right.clone(),
            (_, Self::Bottom(_)) => {}
            (
                Self::Constant {
                    value: left_value,
                    ty: left_ty,
                },
                Self::Constant {
                    value: right_value,
                    ty: right_ty,
                },
            ) if left_value == right_value && left_ty == right_ty => {}
            (
                Self::Affine {
                    base: left_base,
                    ty: left_ty,
                    minimum_offset: left_minimum,
                    maximum_offset: left_maximum,
                },
                Self::Affine {
                    base: right_base,
                    ty: right_ty,
                    minimum_offset: right_minimum,
                    maximum_offset: right_maximum,
                },
            ) if left_base == right_base && left_ty == right_ty => {
                *left_minimum = (*left_minimum).min(*right_minimum);
                *left_maximum = (*left_maximum).max(*right_maximum);
            }
            (Self::Unknown(_), _) => {}
            (left, right) => *left = Self::Unknown(fact_type(left).or_else(|| fact_type(right))),
        }
        *self != previous
    }

    fn widen_assign(&mut self, other: &Self) -> bool {
        let previous = self.clone();
        match (&*self, other) {
            (
                Self::Affine {
                    base: left_base,
                    ty: left_ty,
                    minimum_offset: left_minimum,
                    maximum_offset: left_maximum,
                },
                Self::Affine {
                    base: right_base,
                    ty: right_ty,
                    minimum_offset: right_minimum,
                    maximum_offset: right_maximum,
                },
            ) if left_base == right_base
                && left_ty == right_ty
                && (left_minimum != right_minimum || left_maximum != right_maximum) =>
            {
                *self = Self::Unknown(Some(*left_ty));
            }
            _ => {
                self.join_assign(other);
            }
        }
        *self != previous
    }
}

#[derive(Clone, Copy, Debug)]
pub struct AffineAddressAnalysis {
    stack_pointer: Option<GlobalId>,
}

impl AffineAddressAnalysis {
    pub fn new(module: &Module) -> Self {
        Self {
            stack_pointer: find_stack_pointer(module),
        }
    }
}

impl ForwardAnalysis for AffineAddressAnalysis {
    type Fact = AddressFact;
    type Error = Infallible;

    fn root(
        &mut self,
        cx: &TransferContext<'_>,
        kind: RootKind,
        value: super::index::ValueId,
        default: Self::Fact,
    ) -> Result<Self::Fact, Self::Error> {
        let is_stack_pointer = matches!(kind, RootKind::GlobalInitial)
            && matches!(
                cx.index.values[value.0].producer,
                ValueProducer::GlobalInitial(global) if Some(global) == self.stack_pointer
            );
        Ok(if is_stack_pointer {
            AddressFact::Affine {
                base: AddressBase::ShadowStack(cx.function),
                ty: cx
                    .module
                    .globals
                    .get(self.stack_pointer.expect("identified stack pointer"))
                    .ty,
                minimum_offset: 0,
                maximum_offset: 0,
            }
        } else {
            default
        })
    }

    fn constant(
        &mut self,
        _cx: &TransferContext<'_>,
        value: Value,
    ) -> Result<Self::Fact, Self::Error> {
        Ok(match value {
            Value::I32(value) => AddressFact::Constant {
                value: i64::from(value),
                ty: ValType::I32,
            },
            Value::I64(value) => AddressFact::Constant {
                value,
                ty: ValType::I64,
            },
            Value::F32(_) => AddressFact::Unknown(Some(ValType::F32)),
            Value::F64(_) => AddressFact::Unknown(Some(ValType::F64)),
            Value::V128(_) => AddressFact::Unknown(Some(ValType::V128)),
        })
    }

    fn binary(
        &mut self,
        _cx: &TransferContext<'_>,
        op: BinaryOp,
        left: &Self::Fact,
        right: &Self::Fact,
        output: ValType,
    ) -> Result<Self::Fact, Self::Error> {
        Ok(match op {
            BinaryOp::I32Add | BinaryOp::I64Add => add(left, right, output),
            BinaryOp::I32Sub | BinaryOp::I64Sub => subtract(left, right, output),
            _ => AddressFact::top(output),
        })
    }
}

pub fn find_stack_pointer(module: &Module) -> Option<GlobalId> {
    module.globals.iter().find_map(|global| {
        let local_name = global.name.as_deref();
        let import_name = match global.kind {
            GlobalKind::Import(import) => Some(module.imports.get(import).name.as_str()),
            GlobalKind::Local(_) => None,
        };
        [local_name, import_name]
            .into_iter()
            .flatten()
            .any(is_stack_pointer_name)
            .then_some(global.id())
    })
}

fn is_stack_pointer_name(name: &str) -> bool {
    matches!(name, "__stack_pointer" | "stack_pointer")
}

fn add(left: &AddressFact, right: &AddressFact, output: ValType) -> AddressFact {
    match (left, right) {
        (AddressFact::Constant { value: left, .. }, AddressFact::Constant { value: right, .. }) => {
            constant_result(left.wrapping_add(*right), output)
        }
        (affine @ AddressFact::Affine { .. }, AddressFact::Constant { value: offset, .. })
        | (AddressFact::Constant { value: offset, .. }, affine @ AddressFact::Affine { .. }) => {
            offset_affine(affine, *offset)
        }
        _ => AddressFact::top(output),
    }
}

fn subtract(left: &AddressFact, right: &AddressFact, output: ValType) -> AddressFact {
    match (left, right) {
        (AddressFact::Constant { value: left, .. }, AddressFact::Constant { value: right, .. }) => {
            constant_result(left.wrapping_sub(*right), output)
        }
        (affine @ AddressFact::Affine { .. }, AddressFact::Constant { value: offset, .. }) => {
            offset_affine(affine, offset.wrapping_neg())
        }
        _ => AddressFact::top(output),
    }
}

fn offset_affine(fact: &AddressFact, amount: i64) -> AddressFact {
    let AddressFact::Affine {
        base,
        ty,
        minimum_offset,
        maximum_offset,
    } = fact
    else {
        unreachable!()
    };
    AddressFact::Affine {
        base: *base,
        ty: *ty,
        minimum_offset: minimum_offset.saturating_add(amount),
        maximum_offset: maximum_offset.saturating_add(amount),
    }
}

fn constant_result(value: i64, ty: ValType) -> AddressFact {
    let value = match ty {
        ValType::I32 => i64::from(value as i32),
        ValType::I64 => value,
        _ => return AddressFact::top(ty),
    };
    AddressFact::Constant { value, ty }
}

fn fact_type(fact: &AddressFact) -> Option<ValType> {
    match fact {
        AddressFact::Bottom(ty) | AddressFact::Unknown(ty) => *ty,
        AddressFact::Constant { ty, .. } => Some(*ty),
        AddressFact::Affine { ty, .. } => Some(*ty),
    }
}
