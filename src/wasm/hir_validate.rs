//! Structural and type validation for VMU HIR.
//!
//! HIR is intentionally easy to construct, which makes an explicit verifier
//! essential. Every pass consuming HIR may rely on the invariants checked
//! here instead of rediscovering them with indexing, `unwrap`, or accidental
//! backend behavior.

use std::{
    collections::{BTreeMap, BTreeSet},
    error::Error,
    fmt,
};

use super::{
    hir::*,
    intrinsics::{AbiType, IntrinsicDefinition},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ValidationError {
    context: String,
    message: String,
}

impl ValidationError {
    fn new(context: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            context: context.into(),
            message: message.into(),
        }
    }
}

impl fmt::Display for ValidationError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}: {}", self.context, self.message)
    }
}

impl Error for ValidationError {}

#[derive(Clone)]
struct FunctionSignature {
    parameters: Vec<Type>,
    result: Type,
}

#[derive(Clone)]
struct Binding {
    ty: Type,
    mutable: bool,
}

#[derive(Clone)]
struct PlaceInfo {
    ty: Type,
    mutable: bool,
}

#[derive(Clone)]
struct ControlTarget {
    label: Option<String>,
    continuable: bool,
}

pub fn validate(program: &Program) -> Result<(), ValidationError> {
    ProgramValidator::new(program)?.validate(program)
}

struct ProgramValidator {
    functions: BTreeMap<String, FunctionSignature>,
    statics: BTreeMap<String, Binding>,
}

impl ProgramValidator {
    fn new(program: &Program) -> Result<Self, ValidationError> {
        let mut functions = BTreeMap::new();
        for function in program.all_functions() {
            if function.name.is_empty() {
                return Err(ValidationError::new(
                    "HIR program",
                    "function name is empty",
                ));
            }
            let signature = FunctionSignature {
                parameters: function
                    .parameters
                    .iter()
                    .map(|parameter| parameter.ty.clone())
                    .collect(),
                result: function.result.clone(),
            };
            if functions.insert(function.name.clone(), signature).is_some() {
                return Err(ValidationError::new(
                    "HIR program",
                    format!("duplicate function name {:?}", function.name),
                ));
            }
        }

        let mut statics = BTreeMap::new();
        for static_ in &program.statics {
            if static_.name.is_empty() {
                return Err(ValidationError::new("HIR program", "static name is empty"));
            }
            let binding = Binding {
                ty: static_.ty.clone(),
                mutable: static_.mutable,
            };
            if statics.insert(static_.name.clone(), binding).is_some() {
                return Err(ValidationError::new(
                    "HIR program",
                    format!("duplicate static name {:?}", static_.name),
                ));
            }
        }

        Ok(Self { functions, statics })
    }

    fn validate(self, program: &Program) -> Result<(), ValidationError> {
        for static_ in &program.statics {
            let context = format!("static {:?}", static_.name);
            validate_stored_type(&static_.ty, &context)?;
            validate_initializer(&static_.initializer, &static_.ty, &self, &context)?;
        }

        validate_function(&program.main, false, &self)?;
        for function in program.interrupt_handlers.iter() {
            if !function.parameters.is_empty() || function.result != Type::Unit {
                return Err(ValidationError::new(
                    format!("interrupt handler {:?}", function.name),
                    "interrupt handlers must have signature () -> unit",
                ));
            }
            validate_function(function, true, &self)?;
        }
        for function in &program.functions {
            validate_function(function, false, &self)?;
        }
        Ok(())
    }
}

fn validate_function(
    function: &Function,
    interrupt: bool,
    program: &ProgramValidator,
) -> Result<(), ValidationError> {
    let context = if interrupt {
        format!("interrupt handler {:?}", function.name)
    } else {
        format!("function {:?}", function.name)
    };
    validate_type(&function.result, true, &context)?;

    let mut validator = FunctionValidator {
        function,
        program,
        scopes: vec![BTreeMap::new()],
        names: BTreeSet::new(),
        controls: Vec::new(),
    };
    for parameter in &function.parameters {
        validate_stored_type(&parameter.ty, &context)?;
        validator.declare(
            &parameter.name,
            Binding {
                ty: parameter.ty.clone(),
                mutable: true,
            },
            &context,
        )?;
    }
    validator.validate_statements(&function.body.statements, &context)
}

struct FunctionValidator<'a> {
    function: &'a Function,
    program: &'a ProgramValidator,
    scopes: Vec<BTreeMap<String, Binding>>,
    /// HIR names are unique within a function even though visibility is
    /// lexical. This keeps name-based optimization passes unambiguous.
    names: BTreeSet<String>,
    controls: Vec<ControlTarget>,
}

impl FunctionValidator<'_> {
    fn validate_statements(
        &mut self,
        statements: &[Statement],
        context: &str,
    ) -> Result<(), ValidationError> {
        for (index, statement) in statements.iter().enumerate() {
            let statement_context = format!("{context}, statement {index}");
            self.validate_statement(statement, &statement_context)?;
            if is_terminal(statement) && index + 1 != statements.len() {
                return Err(ValidationError::new(
                    statement_context,
                    "unconditionally terminating statement is not last in its block",
                ));
            }
        }
        Ok(())
    }

    fn validate_statement(
        &mut self,
        statement: &Statement,
        context: &str,
    ) -> Result<(), ValidationError> {
        match statement {
            Statement::Declare {
                name,
                ty,
                mutable,
                initializer,
            } => {
                validate_stored_type(ty, context)?;
                if let Some(initializer) = initializer {
                    self.validate_expression_as(initializer, ty, context)?;
                }
                self.declare(
                    name,
                    Binding {
                        ty: ty.clone(),
                        mutable: *mutable,
                    },
                    context,
                )
            }
            Statement::Assign { destination, value } => {
                self.validate_expression(value, context)?;
                self.validate_place_as(destination, &value.ty, true, context)
            }
            Statement::Load {
                destination,
                address,
                ty,
                ..
            } => {
                validate_stored_type(ty, context)?;
                self.validate_expression_as(address, &Type::Pointer, context)?;
                if let Some(destination) = destination {
                    self.validate_place_as(destination, ty, true, context)?;
                }
                Ok(())
            }
            Statement::Store {
                address, value, ty, ..
            } => {
                validate_stored_type(ty, context)?;
                self.validate_expression_as(address, &Type::Pointer, context)?;
                self.validate_expression_as(value, ty, context)
            }
            Statement::Call {
                destination,
                function,
                arguments,
            } => self.validate_call(destination.as_ref(), function, arguments, context),
            Statement::CallIndirect {
                destination,
                target,
                signature,
                arguments,
            } => {
                self.validate_expression_as(target, &Type::Pointer, context)?;
                validate_signature(signature, context)?;
                self.validate_arguments(arguments, &signature.parameters, context)?;
                self.validate_destination(destination.as_ref(), &signature.result, context)
            }
            Statement::TailCall {
                function,
                arguments,
            } => {
                let signature = self.program.functions.get(function).ok_or_else(|| {
                    ValidationError::new(context, format!("unknown function {function:?}"))
                })?;
                self.validate_arguments(arguments, &signature.parameters, context)?;
                require_type(&signature.result, &self.function.result, context)
            }
            Statement::TailCallIndirect {
                target,
                signature,
                arguments,
            } => {
                self.validate_expression_as(target, &Type::Pointer, context)?;
                validate_signature(signature, context)?;
                self.validate_arguments(arguments, &signature.parameters, context)?;
                require_type(&signature.result, &self.function.result, context)
            }
            Statement::Intrinsic {
                destination,
                operation,
                arguments,
            } => {
                let definition = operation.definition().ok_or_else(|| {
                    ValidationError::new(
                        context,
                        format!("intrinsic {operation:?} is not registered"),
                    )
                })?;
                self.validate_intrinsic(destination.as_ref(), arguments, &definition, context)
            }
            Statement::Block { label, body } => {
                self.push_control(label, false, context)?;
                let result = self.validate_nested_block(body, context);
                self.controls.pop();
                result
            }
            Statement::If {
                condition,
                then_body,
                else_body,
            } => {
                self.validate_expression_as(condition, &Type::Bool, context)?;
                self.validate_nested_block(then_body, &format!("{context}, then branch"))?;
                if let Some(else_body) = else_body {
                    self.validate_nested_block(else_body, &format!("{context}, else branch"))?;
                }
                Ok(())
            }
            Statement::Loop { label, body } => {
                self.push_control(label, true, context)?;
                let result = self.validate_nested_block(body, context);
                self.controls.pop();
                result
            }
            Statement::Switch {
                value,
                cases,
                default,
            } => {
                self.validate_expression(value, context)?;
                let Type::Bits(width) = value.ty else {
                    return Err(ValidationError::new(
                        context,
                        format!("switch value has non-integer type {:?}", value.ty),
                    ));
                };
                let mut values = BTreeSet::new();
                for (index, case) in cases.iter().enumerate() {
                    if case.value.width().get() > width.get() {
                        return Err(ValidationError::new(
                            context,
                            format!("case {} does not fit switch width", case.value.bits()),
                        ));
                    }
                    if !values.insert(case.value.bits()) {
                        return Err(ValidationError::new(
                            context,
                            format!("duplicate switch case {}", case.value.bits()),
                        ));
                    }
                    self.validate_nested_block(&case.body, &format!("{context}, case {index}"))?;
                }
                self.validate_nested_block(default, &format!("{context}, default case"))
            }
            Statement::Break { label } => self.validate_control(label.as_deref(), false, context),
            Statement::Continue { label } => self.validate_control(label.as_deref(), true, context),
            Statement::Return(value) => match value {
                Some(value) => self.validate_expression_as(value, &self.function.result, context),
                None if self.function.result == Type::Unit => Ok(()),
                None => Err(ValidationError::new(
                    context,
                    format!(
                        "return has no value, but function returns {:?}",
                        self.function.result
                    ),
                )),
            },
            Statement::Trap | Statement::Unreachable => Ok(()),
        }
    }

    fn validate_nested_block(
        &mut self,
        block: &Block,
        context: &str,
    ) -> Result<(), ValidationError> {
        self.scopes.push(BTreeMap::new());
        let result = self.validate_statements(&block.statements, context);
        self.scopes.pop();
        result
    }

    fn validate_call(
        &mut self,
        destination: Option<&Place>,
        function: &str,
        arguments: &[Expression],
        context: &str,
    ) -> Result<(), ValidationError> {
        let signature = self.program.functions.get(function).ok_or_else(|| {
            ValidationError::new(context, format!("unknown function {function:?}"))
        })?;
        self.validate_arguments(arguments, &signature.parameters, context)?;
        self.validate_destination(destination, &signature.result, context)
    }

    fn validate_intrinsic(
        &mut self,
        destination: Option<&Place>,
        arguments: &[Expression],
        definition: &IntrinsicDefinition,
        context: &str,
    ) -> Result<(), ValidationError> {
        let parameters = definition
            .parameters
            .iter()
            .map(|parameter| intrinsic_type(parameter.ty))
            .collect::<Vec<_>>();
        self.validate_arguments(arguments, &parameters, context)?;
        validate_intrinsic_constants(definition.operation, arguments, context)?;
        let result = definition.result.map(intrinsic_type).unwrap_or(Type::Unit);
        self.validate_destination(destination, &result, context)
    }

    fn validate_arguments(
        &mut self,
        arguments: &[Expression],
        parameters: &[Type],
        context: &str,
    ) -> Result<(), ValidationError> {
        if arguments.len() != parameters.len() {
            return Err(ValidationError::new(
                context,
                format!(
                    "call has {} arguments for {} parameters",
                    arguments.len(),
                    parameters.len()
                ),
            ));
        }
        for (index, (argument, parameter)) in arguments.iter().zip(parameters).enumerate() {
            self.validate_expression_as(
                argument,
                parameter,
                &format!("{context}, argument {index}"),
            )?;
        }
        Ok(())
    }

    fn validate_destination(
        &mut self,
        destination: Option<&Place>,
        result: &Type,
        context: &str,
    ) -> Result<(), ValidationError> {
        match (destination, result) {
            (Some(_), Type::Unit) => Err(ValidationError::new(
                context,
                "unit-returning operation has a destination",
            )),
            (Some(destination), result) => {
                self.validate_place_as(destination, result, true, context)
            }
            (None, _) => Ok(()),
        }
    }

    fn validate_expression_as(
        &mut self,
        expression: &Expression,
        expected: &Type,
        context: &str,
    ) -> Result<(), ValidationError> {
        self.validate_expression(expression, context)?;
        require_type(&expression.ty, expected, context)
    }

    fn validate_expression(
        &mut self,
        expression: &Expression,
        context: &str,
    ) -> Result<(), ValidationError> {
        validate_type(&expression.ty, false, context)?;
        match &expression.kind {
            ExpressionKind::Constant(constant) => {
                validate_constant(constant, &expression.ty, self.program, context)
            }
            ExpressionKind::Read(place) => {
                self.validate_place_as(place, &expression.ty, false, context)
            }
            ExpressionKind::IntegerUnary { op, operand } => {
                self.validate_expression(operand, context)?;
                require_integer(&operand.ty, context)?;
                require_integer(&expression.ty, context)?;
                if matches!(op, IntegerUnaryOp::Negate | IntegerUnaryOp::Not) {
                    require_type(&operand.ty, &expression.ty, context)?;
                }
                Ok(())
            }
            ExpressionKind::IntegerBinary { left, right, .. } => {
                self.validate_expression(left, context)?;
                self.validate_expression(right, context)?;
                require_integer(&expression.ty, context)?;
                require_type(&left.ty, &expression.ty, context)?;
                require_type(&right.ty, &expression.ty, context)
            }
            ExpressionKind::FloatUnary { operand, .. } => {
                self.validate_expression(operand, context)?;
                require_float(&expression.ty, context)?;
                require_type(&operand.ty, &expression.ty, context)
            }
            ExpressionKind::FloatBinary { left, right, .. } => {
                self.validate_expression(left, context)?;
                self.validate_expression(right, context)?;
                require_float(&expression.ty, context)?;
                require_type(&left.ty, &expression.ty, context)?;
                require_type(&right.ty, &expression.ty, context)
            }
            ExpressionKind::Compare { op, left, right } => {
                self.validate_expression(left, context)?;
                self.validate_expression(right, context)?;
                require_type(&expression.ty, &Type::Bool, context)?;
                if matches!(
                    op,
                    CompareOp::FloatEqual
                        | CompareOp::FloatNotEqual
                        | CompareOp::FloatLessThan
                        | CompareOp::FloatLessThanOrEqual
                        | CompareOp::FloatGreaterThan
                        | CompareOp::FloatGreaterThanOrEqual
                ) {
                    require_float(&left.ty, context)?;
                    require_type(&left.ty, &right.ty, context)
                } else {
                    require_integer(&left.ty, context)?;
                    require_integer(&right.ty, context)
                }
            }
            ExpressionKind::Convert { op, operand } => {
                self.validate_expression(operand, context)?;
                validate_conversion(*op, &operand.ty, &expression.ty, context)
            }
            ExpressionKind::Select {
                when_true,
                when_false,
                condition,
            } => {
                self.validate_expression_as(when_true, &expression.ty, context)?;
                self.validate_expression_as(when_false, &expression.ty, context)?;
                self.validate_expression_as(condition, &Type::Bool, context)
            }
            ExpressionKind::AddressOf(place) => {
                require_type(&expression.ty, &Type::Pointer, context)?;
                self.place_info(place, context).map(|_| ())
            }
            ExpressionKind::PointerOffset { base, offset } => {
                require_type(&expression.ty, &Type::Pointer, context)?;
                self.validate_expression_as(base, &Type::Pointer, context)?;
                self.validate_expression(offset, context)?;
                require_integer(&offset.ty, context)
            }
            ExpressionKind::Aggregate(elements) => {
                let expected = aggregate_elements(&expression.ty).ok_or_else(|| {
                    ValidationError::new(
                        context,
                        format!("aggregate expression has scalar type {:?}", expression.ty),
                    )
                })?;
                if elements.len() != expected.len() {
                    return Err(ValidationError::new(
                        context,
                        format!(
                            "aggregate has {} elements for {} fields",
                            elements.len(),
                            expected.len()
                        ),
                    ));
                }
                for (element, expected) in elements.iter().zip(&expected) {
                    self.validate_expression_as(element, expected, context)?;
                }
                Ok(())
            }
            ExpressionKind::ExtractElement { aggregate, index } => {
                self.validate_expression(aggregate, context)?;
                let element = aggregate_element(&aggregate.ty, *index, context)?;
                require_type(&expression.ty, &element, context)
            }
            ExpressionKind::InsertElement {
                aggregate,
                index,
                value,
            } => {
                self.validate_expression_as(aggregate, &expression.ty, context)?;
                let element = aggregate_element(&aggregate.ty, *index, context)?;
                self.validate_expression_as(value, &element, context)
            }
        }
    }

    fn validate_place_as(
        &mut self,
        place: &Place,
        expected: &Type,
        write: bool,
        context: &str,
    ) -> Result<(), ValidationError> {
        let info = self.place_info(place, context)?;
        if write && !info.mutable {
            return Err(ValidationError::new(context, "write to immutable place"));
        }
        if matches!(place, Place::ByteOffset { .. }) {
            let available = type_size(&info.ty, context)?;
            let required = type_size(expected, context)?;
            if required > available {
                return Err(ValidationError::new(
                    context,
                    format!("{required}-byte access exceeds {available}-byte place"),
                ));
            }
            Ok(())
        } else {
            require_type(&info.ty, expected, context)
        }
    }

    fn place_info(&mut self, place: &Place, context: &str) -> Result<PlaceInfo, ValidationError> {
        match place {
            Place::Variable(name) => self
                .scopes
                .iter()
                .rev()
                .find_map(|scope| scope.get(name))
                .cloned()
                .map(|binding| PlaceInfo {
                    ty: binding.ty,
                    mutable: binding.mutable,
                })
                .ok_or_else(|| ValidationError::new(context, format!("unknown variable {name:?}"))),
            Place::Static(name) => self
                .program
                .statics
                .get(name)
                .cloned()
                .map(|binding| PlaceInfo {
                    ty: binding.ty,
                    mutable: binding.mutable,
                })
                .ok_or_else(|| ValidationError::new(context, format!("unknown static {name:?}"))),
            Place::Field { base, index } => {
                let base = self.place_info(base, context)?;
                let ty = aggregate_element(&base.ty, *index, context)?;
                Ok(PlaceInfo {
                    ty,
                    mutable: base.mutable,
                })
            }
            Place::Element { base, index } => {
                let base = self.place_info(base, context)?;
                let Type::Array { element, length } = base.ty else {
                    return Err(ValidationError::new(
                        context,
                        "element place base is not an array",
                    ));
                };
                self.validate_expression(index, context)?;
                require_integer(&index.ty, context)?;
                let index = constant_index(index).ok_or_else(|| {
                    ValidationError::new(context, "VMU HIR place index is not a constant integer")
                })?;
                if index >= length {
                    return Err(ValidationError::new(
                        context,
                        format!("array index {index} exceeds length {length}"),
                    ));
                }
                Ok(PlaceInfo {
                    ty: *element,
                    mutable: base.mutable,
                })
            }
            Place::ByteOffset { base, offset } => {
                let base = self.place_info(base, context)?;
                self.validate_expression(offset, context)?;
                require_integer(&offset.ty, context)?;
                let offset = constant_index(offset).ok_or_else(|| {
                    ValidationError::new(context, "VMU HIR byte offset is not a constant integer")
                })?;
                let size = type_size(&base.ty, context)?;
                if offset > size {
                    return Err(ValidationError::new(
                        context,
                        format!("byte offset {offset} exceeds {size}-byte place"),
                    ));
                }
                Ok(PlaceInfo {
                    ty: Type::OpaqueBytes(size - offset),
                    mutable: base.mutable,
                })
            }
        }
    }

    fn declare(
        &mut self,
        name: &str,
        binding: Binding,
        context: &str,
    ) -> Result<(), ValidationError> {
        if name.is_empty() {
            return Err(ValidationError::new(context, "variable name is empty"));
        }
        if !self.names.insert(name.to_owned()) {
            return Err(ValidationError::new(
                context,
                format!("duplicate variable name {name:?}"),
            ));
        }
        self.scopes
            .last_mut()
            .expect("function validator always has a scope")
            .insert(name.to_owned(), binding);
        Ok(())
    }

    fn push_control(
        &mut self,
        label: &Option<String>,
        continuable: bool,
        context: &str,
    ) -> Result<(), ValidationError> {
        if let Some(label) = label
            && self
                .controls
                .iter()
                .any(|control| control.label.as_ref() == Some(label))
        {
            return Err(ValidationError::new(
                context,
                format!("duplicate active control label {label:?}"),
            ));
        }
        self.controls.push(ControlTarget {
            label: label.clone(),
            continuable,
        });
        Ok(())
    }

    fn validate_control(
        &self,
        label: Option<&str>,
        continuing: bool,
        context: &str,
    ) -> Result<(), ValidationError> {
        let found = self.controls.iter().rev().any(|control| {
            (label.is_none() || control.label.as_deref() == label)
                && (!continuing || control.continuable)
        });
        if found {
            Ok(())
        } else {
            Err(ValidationError::new(
                context,
                match (continuing, label) {
                    (true, Some(label)) => format!("no active loop named {label:?}"),
                    (true, None) => "continue has no enclosing loop".into(),
                    (false, Some(label)) => format!("no active control target named {label:?}"),
                    (false, None) => "break has no enclosing control target".into(),
                },
            ))
        }
    }
}

fn validate_signature(signature: &FunctionType, context: &str) -> Result<(), ValidationError> {
    for parameter in &signature.parameters {
        validate_stored_type(parameter, context)?;
    }
    validate_type(&signature.result, true, context)
}

fn validate_initializer(
    initializer: &StaticInitializer,
    ty: &Type,
    program: &ProgramValidator,
    context: &str,
) -> Result<(), ValidationError> {
    match initializer {
        StaticInitializer::Zeroed => Ok(()),
        StaticInitializer::Constant(constant) => validate_constant(constant, ty, program, context),
        StaticInitializer::Bytes(bytes) => {
            let expected = type_size(ty, context)?;
            if bytes.len() == expected {
                Ok(())
            } else {
                Err(ValidationError::new(
                    context,
                    format!("initializer has {} bytes, expected {expected}", bytes.len()),
                ))
            }
        }
        StaticInitializer::Aggregate(elements) => {
            let expected = aggregate_elements(ty).ok_or_else(|| {
                ValidationError::new(context, format!("aggregate initializer for {ty:?}"))
            })?;
            if elements.len() != expected.len() {
                return Err(ValidationError::new(
                    context,
                    format!(
                        "initializer has {} elements, expected {}",
                        elements.len(),
                        expected.len()
                    ),
                ));
            }
            for (element, ty) in elements.iter().zip(&expected) {
                validate_initializer(element, ty, program, context)?;
            }
            Ok(())
        }
    }
}

fn validate_constant(
    constant: &Constant,
    ty: &Type,
    program: &ProgramValidator,
    context: &str,
) -> Result<(), ValidationError> {
    match constant {
        Constant::Bool(_) => require_type(ty, &Type::Bool, context),
        Constant::Integer(value) => require_type(ty, &Type::Bits(value.width()), context),
        Constant::Float32(_) => require_type(ty, &Type::Float(FloatWidth::Bits32), context),
        Constant::Float64(_) => require_type(ty, &Type::Float(FloatWidth::Bits64), context),
        Constant::NullPointer => require_type(ty, &Type::Pointer, context),
        Constant::StaticAddress { name, offset } => {
            require_type(ty, &Type::Pointer, context)?;
            let static_ = program.statics.get(name).ok_or_else(|| {
                ValidationError::new(context, format!("address of unknown static {name:?}"))
            })?;
            let size = type_size(&static_.ty, context)?;
            if *offset <= size {
                Ok(())
            } else {
                Err(ValidationError::new(
                    context,
                    format!("static address offset {offset} exceeds {size}-byte object"),
                ))
            }
        }
        Constant::FunctionAddress(function) => {
            require_type(ty, &Type::Pointer, context)?;
            if program.functions.contains_key(function) {
                Ok(())
            } else {
                Err(ValidationError::new(
                    context,
                    format!("address of unknown function {function:?}"),
                ))
            }
        }
    }
}

fn validate_conversion(
    operation: ConversionOp,
    source: &Type,
    destination: &Type,
    context: &str,
) -> Result<(), ValidationError> {
    use ConversionOp::*;
    match operation {
        Truncate => compare_integer_widths(source, destination, |from, to| to <= from, context),
        ZeroExtend | SignExtend => {
            compare_integer_widths(source, destination, |from, to| to >= from, context)
        }
        IntegerToFloat(_) => {
            require_integer(source, context)?;
            require_float(destination, context)
        }
        FloatToInteger { .. } => {
            require_float(source, context)?;
            require_integer(destination, context)
        }
        PromoteFloat => {
            require_type(source, &Type::Float(FloatWidth::Bits32), context)?;
            require_type(destination, &Type::Float(FloatWidth::Bits64), context)
        }
        DemoteFloat => {
            require_type(source, &Type::Float(FloatWidth::Bits64), context)?;
            require_type(destination, &Type::Float(FloatWidth::Bits32), context)
        }
        Reinterpret => {
            let source_size = type_size(source, context)?;
            let destination_size = type_size(destination, context)?;
            if source_size == destination_size {
                Ok(())
            } else {
                Err(ValidationError::new(
                    context,
                    format!("reinterpret changes size from {source_size} to {destination_size}"),
                ))
            }
        }
        BooleanToInteger => {
            require_type(source, &Type::Bool, context)?;
            require_integer(destination, context)
        }
        IntegerToBoolean => {
            require_integer(source, context)?;
            require_type(destination, &Type::Bool, context)
        }
    }
}

fn compare_integer_widths(
    source: &Type,
    destination: &Type,
    valid: impl FnOnce(u8, u8) -> bool,
    context: &str,
) -> Result<(), ValidationError> {
    let Type::Bits(source) = source else {
        return require_integer(source, context);
    };
    let Type::Bits(destination) = destination else {
        return require_integer(destination, context);
    };
    if valid(source.get(), destination.get()) {
        Ok(())
    } else {
        Err(ValidationError::new(
            context,
            format!(
                "invalid integer conversion from {} to {} bits",
                source.get(),
                destination.get()
            ),
        ))
    }
}

fn validate_stored_type(ty: &Type, context: &str) -> Result<(), ValidationError> {
    validate_type(ty, false, context)?;
    let size = type_size(ty, context)?;
    if size == 0 {
        Err(ValidationError::new(context, "stored type has zero size"))
    } else {
        Ok(())
    }
}

fn validate_type(ty: &Type, allow_unit: bool, context: &str) -> Result<(), ValidationError> {
    match ty {
        Type::Unit if allow_unit => Ok(()),
        Type::Unit => Err(ValidationError::new(context, "unit is not a value type")),
        Type::Bool | Type::Bits(_) | Type::Float(_) | Type::Pointer => Ok(()),
        Type::Array { element, length } => {
            if *length == 0 {
                return Err(ValidationError::new(
                    context,
                    "zero-length array is unsupported",
                ));
            }
            validate_stored_type(element, context)
        }
        Type::Struct(fields) | Type::Tuple(fields) => {
            if fields.is_empty() {
                return Err(ValidationError::new(
                    context,
                    "empty aggregate is unsupported",
                ));
            }
            for field in fields {
                validate_stored_type(field, context)?;
            }
            Ok(())
        }
        Type::OpaqueBytes(0) => Err(ValidationError::new(
            context,
            "zero-length opaque object is unsupported",
        )),
        Type::OpaqueBytes(_) => Ok(()),
    }
}

fn type_size(ty: &Type, context: &str) -> Result<usize, ValidationError> {
    match ty {
        Type::Unit => Ok(0),
        Type::Bool => Ok(1),
        Type::Bits(width) => Ok(usize::from(width.get()).div_ceil(8)),
        Type::Float(FloatWidth::Bits32) => Ok(4),
        Type::Float(FloatWidth::Bits64) => Ok(8),
        Type::Pointer => Ok(2),
        Type::Array { element, length } => type_size(element, context)?
            .checked_mul(*length)
            .ok_or_else(|| ValidationError::new(context, "array size overflows usize")),
        Type::Struct(fields) | Type::Tuple(fields) => {
            fields.iter().try_fold(0usize, |size, field| {
                size.checked_add(type_size(field, context)?)
                    .ok_or_else(|| ValidationError::new(context, "aggregate size overflows usize"))
            })
        }
        Type::OpaqueBytes(length) => Ok(*length),
    }
}

fn aggregate_elements(ty: &Type) -> Option<Vec<&Type>> {
    match ty {
        Type::Array { element, length } => Some(vec![element.as_ref(); *length]),
        Type::Struct(fields) | Type::Tuple(fields) => Some(fields.iter().collect()),
        _ => None,
    }
}

fn aggregate_element(ty: &Type, index: usize, context: &str) -> Result<Type, ValidationError> {
    match ty {
        Type::Array { element, length } if index < *length => Ok(element.as_ref().clone()),
        Type::Struct(fields) | Type::Tuple(fields) => fields.get(index).cloned().ok_or_else(|| {
            ValidationError::new(context, format!("aggregate has no field {index}"))
        }),
        _ => Err(ValidationError::new(
            context,
            format!("cannot access field {index} of {ty:?}"),
        )),
    }
}

fn require_integer(ty: &Type, context: &str) -> Result<(), ValidationError> {
    if matches!(ty, Type::Bits(_)) {
        Ok(())
    } else {
        Err(ValidationError::new(
            context,
            format!("expected integer type, found {ty:?}"),
        ))
    }
}

fn require_float(ty: &Type, context: &str) -> Result<(), ValidationError> {
    if matches!(ty, Type::Float(_)) {
        Ok(())
    } else {
        Err(ValidationError::new(
            context,
            format!("expected float type, found {ty:?}"),
        ))
    }
}

fn require_type(actual: &Type, expected: &Type, context: &str) -> Result<(), ValidationError> {
    if actual == expected {
        Ok(())
    } else {
        Err(ValidationError::new(
            context,
            format!("expected type {expected:?}, found {actual:?}"),
        ))
    }
}

fn constant_index(expression: &Expression) -> Option<usize> {
    let ExpressionKind::Constant(Constant::Integer(value)) = expression.kind else {
        return None;
    };
    usize::try_from(value.bits()).ok()
}

fn intrinsic_type(ty: AbiType) -> Type {
    match ty {
        AbiType::U8 => Type::Bits(BitWidth::new(8).expect("8 is a valid HIR bit width")),
        AbiType::ConstU8Pointer | AbiType::MutU8Pointer => Type::Pointer,
    }
}

fn validate_intrinsic_constants(
    operation: super::intrinsics::Intrinsic,
    arguments: &[Expression],
    context: &str,
) -> Result<(), ValidationError> {
    use super::intrinsics::Intrinsic;

    match operation {
        Intrinsic::SfrBitRead(_) | Intrinsic::SfrBitWrite(_) | Intrinsic::SfrBitToggle(_) => {
            validate_bit_index(arguments.first(), context)
        }
        Intrinsic::XramRead | Intrinsic::XramWrite => {
            validate_xram_access(arguments.first(), arguments.get(1), None, context)
        }
        Intrinsic::XramCopyFromMain => validate_xram_access(
            arguments.first(),
            arguments.get(1),
            arguments.get(3),
            context,
        ),
        Intrinsic::XramCopyToMain => validate_xram_access(
            arguments.get(1),
            arguments.get(2),
            arguments.get(3),
            context,
        ),
        Intrinsic::SfrRead(_) | Intrinsic::SfrWrite(_) => Ok(()),
    }
}

fn validate_bit_index(argument: Option<&Expression>, context: &str) -> Result<(), ValidationError> {
    if let Some(bit) = argument.and_then(constant_integer)
        && bit >= 8
    {
        return Err(ValidationError::new(
            context,
            format!("constant SFR bit index {bit} is outside 0 through 7"),
        ));
    }
    Ok(())
}

fn validate_xram_access(
    bank: Option<&Expression>,
    offset: Option<&Expression>,
    length: Option<&Expression>,
    context: &str,
) -> Result<(), ValidationError> {
    if let Some(bank) = bank.and_then(constant_integer)
        && bank > 2
    {
        return Err(ValidationError::new(
            context,
            format!("constant XRAM bank {bank} is outside 0 through 2"),
        ));
    }

    let offset = offset
        .and_then(constant_integer)
        .and_then(|value| u8::try_from(value).ok());
    if let Some(offset) = offset
        && !super::target::xram_offset_is_addressable(offset)
    {
        return Err(ValidationError::new(
            context,
            format!("constant XRAM offset {offset:#04x} is not mapped"),
        ));
    }

    if let (Some(offset), Some(length)) = (offset, length.and_then(constant_integer)) {
        let end = u128::from(offset).checked_add(length);
        if !end.is_some_and(|end| end <= u128::from(super::target::XRAM_BANK_LAST_OFFSET) + 1) {
            return Err(ValidationError::new(
                context,
                format!(
                    "constant XRAM copy from {offset:#04x} with length {length} exceeds the bank"
                ),
            ));
        }
    }
    Ok(())
}

fn constant_integer(expression: &Expression) -> Option<u128> {
    match &expression.kind {
        ExpressionKind::Constant(Constant::Integer(value)) => Some(value.bits()),
        ExpressionKind::Convert {
            op: ConversionOp::ZeroExtend,
            operand,
        } => constant_integer(operand),
        ExpressionKind::Convert {
            op: ConversionOp::Truncate,
            operand,
        } => {
            let Type::Bits(width) = expression.ty else {
                return None;
            };
            let mask = if width.get() == 128 {
                u128::MAX
            } else {
                (1_u128 << width.get()) - 1
            };
            constant_integer(operand).map(|value| value & mask)
        }
        _ => None,
    }
}

fn is_terminal(statement: &Statement) -> bool {
    matches!(
        statement,
        Statement::TailCall { .. }
            | Statement::TailCallIndirect { .. }
            | Statement::Break { .. }
            | Statement::Continue { .. }
            | Statement::Return(_)
            | Statement::Trap
            | Statement::Unreachable
    )
}

#[cfg(test)]
mod tests {
    use super::validate;
    use crate::wasm::hir::{
        BitWidth, Block, Constant, Expression, ExpressionKind, Function, IntegerConstant,
        InterruptHandlers, Parameter, Place, Program, Statement, Type,
    };
    use crate::wasm::intrinsics::Intrinsic;

    fn program(body: Vec<Statement>) -> Program {
        Program {
            main: Function {
                name: "main".into(),
                parameters: Vec::new(),
                result: Type::Unit,
                body: Block { statements: body },
            },
            interrupt_handlers: InterruptHandlers::default(),
            functions: Vec::new(),
            statics: Vec::new(),
        }
    }

    fn byte(value: u128) -> Expression {
        let width = BitWidth::new(8).unwrap();
        Expression {
            ty: Type::Bits(width),
            kind: ExpressionKind::Constant(Constant::Integer(
                IntegerConstant::new(width, value).unwrap(),
            )),
        }
    }

    #[test]
    fn rejects_unknown_calls() {
        let error = validate(&program(vec![Statement::Call {
            destination: None,
            function: "missing".into(),
            arguments: Vec::new(),
        }]))
        .unwrap_err();
        assert!(error.to_string().contains("unknown function"));
    }

    #[test]
    fn rejects_statements_after_return() {
        let error = validate(&program(vec![
            Statement::Return(None),
            Statement::Return(None),
        ]))
        .unwrap_err();
        assert!(error.to_string().contains("not last"));
    }

    #[test]
    fn parameters_are_mutable_local_places() {
        let width = BitWidth::new(8).unwrap();
        let ty = Type::Bits(width);
        let mut program = program(Vec::new());
        program.main.parameters.push(Parameter {
            name: "value".into(),
            ty: ty.clone(),
        });
        program.main.body.statements = vec![
            Statement::Assign {
                destination: Place::Variable("value".into()),
                value: Expression {
                    ty,
                    kind: ExpressionKind::Constant(Constant::Integer(
                        IntegerConstant::new(width, 1).unwrap(),
                    )),
                },
            },
            Statement::Return(None),
        ];

        validate(&program).unwrap();
    }

    #[test]
    fn rejects_constant_access_past_the_xram_bank() {
        let program = program(vec![
            Statement::Intrinsic {
                destination: None,
                operation: Intrinsic::XramWrite,
                arguments: vec![byte(0), byte(0x7c), byte(0)],
            },
            Statement::Return(None),
        ]);

        let error = validate(&program).unwrap_err();
        assert!(error.to_string().contains("XRAM offset 0x7c is not mapped"));
    }
}
