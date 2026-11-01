use std::collections::HashMap;

use crate::asm::{AssemblyError, assemble, compute_names};
use crate::ast::{ByteValue, Directive, IncludeType, Statement, Statements};
use crate::env::Names;
use crate::expression::{Arg, Expr, IndirectionMode, InternalLabelId};
use crate::instruction::Instr;
use crate::location::Positioned;

pub(crate) fn optimize_jumps(statements: &Statements) -> Result<Statements, AssemblyError> {
    assemble(statements, false)?;
    let mut current = anchor_numeric_targets(statements)?;

    loop {
        let layout = ProgramLayout::new(&current)?;
        if let Some(next) = fold_first_branch_pattern(&current) {
            current = next;
            continue;
        }
        if let Some(next) = shrink_first_transfer(&current, &layout) {
            current = next;
            continue;
        }
        let materialized = materialize_internal_labels(&current)?;
        assemble(&materialized, false)?;
        return Ok(materialized);
    }
}

fn anchor_numeric_targets(statements: &Statements) -> Result<Statements, AssemblyError> {
    let layout = ProgramLayout::new(statements)?;
    let mut labels = HashMap::<ByteOrigin, InternalLabelId>::new();
    let mut rewritten = statements.as_slice().to_vec();

    for (index, statement) in statements.iter().enumerate() {
        let Statement::Instr(span, instruction) = statement else {
            continue;
        };
        if is_change_jump(statements, index, &layout) {
            continue;
        }
        let Some(target) = control_flow_target(instruction) else {
            continue;
        };
        if expression_contains_name(target) {
            continue;
        }
        let Some(address) = layout.eval(index, target) else {
            continue;
        };
        let Some(origin) = layout.origin(address) else {
            continue;
        };

        let next_id = labels.len();
        let id = *labels
            .entry(origin)
            .or_insert_with(|| InternalLabelId::new(next_id));
        let target = Expr::InternalName(target.span(), id);
        let instruction = with_control_flow_target(instruction, target)
            .expect("control-flow instruction must accept a replacement target");
        rewritten[index] = Statement::Instr(span.clone(), instruction);
    }

    let mut labels_by_statement = vec![vec![]; rewritten.len()];
    for (origin, id) in labels {
        labels_by_statement[origin.statement].push((origin.offset, id));
    }

    let mut anchored = Vec::with_capacity(rewritten.len() + labels_by_statement.len());
    for (index, statement) in rewritten.into_iter().enumerate() {
        labels_by_statement[index].sort_by_key(|(offset, id)| (*offset, *id));
        for (offset, id) in labels_by_statement[index].drain(..) {
            anchored.push(Statement::InternalLabel(statement.span(), id, offset));
        }
        anchored.push(statement);
    }

    let anchored = statements.with_statements(anchored);
    assert!(validate_internal_labels(&anchored));
    assemble(&anchored, false)?;
    Ok(anchored)
}

fn materialize_internal_labels(statements: &Statements) -> Result<Statements, AssemblyError> {
    let layout = ProgramLayout::new(statements)?;
    let mut materialized = vec![];

    for (index, statement) in statements.iter().enumerate() {
        match statement {
            Statement::InternalLabel(..) => {}
            Statement::Instr(span, instruction) => {
                let Some(target) = control_flow_target(instruction) else {
                    materialized.push(statement.clone());
                    continue;
                };
                let Expr::InternalName(target_span, _) = target else {
                    materialized.push(statement.clone());
                    continue;
                };
                let address = layout
                    .eval(index, target)
                    .expect("internal label must resolve before materialization");
                let target = Expr::num(address as i32).with_span(target_span.clone());
                let instruction = with_control_flow_target(instruction, target)
                    .expect("control-flow instruction must accept a replacement target");
                materialized.push(Statement::Instr(span.clone(), instruction));
            }
            _ => materialized.push(statement.clone()),
        }
    }

    Ok(statements.with_statements(materialized))
}

fn fold_first_branch_pattern(statements: &Statements) -> Option<Statements> {
    let index = ProgramIndex::new(statements);
    for pattern in branch_patterns() {
        for position in 0..index.significant.len() {
            let Some(rewrite) = pattern.match_at(statements, &index, position) else {
                continue;
            };
            if has_interior_internal_label(statements, rewrite.replacement_index) {
                continue;
            }
            let candidate = apply_rewrite(statements, rewrite);
            if let Some(candidate) = candidate.into_safe() {
                return Some(candidate);
            }
        }
    }
    None
}

fn shrink_first_transfer(statements: &Statements, layout: &ProgramLayout) -> Option<Statements> {
    for (index, statement) in statements.iter().enumerate() {
        let Statement::Instr(span, instruction) = statement else {
            continue;
        };

        let alternatives = match instruction {
            Instr::Jmpf(target)
                if !is_change_jump(statements, index, layout)
                    && !has_interior_internal_label(statements, index) =>
            {
                vec![Instr::Br(target.clone()), Instr::Jmp(target.clone())]
            }
            Instr::Callf(target) if !has_interior_internal_label(statements, index) => {
                vec![Instr::Call(target.clone())]
            }
            _ => continue,
        };

        for alternative in alternatives {
            let mut rewritten = statements.as_slice().to_vec();
            rewritten[index] = Statement::Instr(span.clone(), alternative);
            let candidate = Candidate {
                statements: statements.with_statements(rewritten),
            };
            if let Some(candidate) = candidate.into_safe() {
                return Some(candidate);
            }
        }
    }
    None
}

fn apply_rewrite(statements: &Statements, rewrite: Rewrite) -> Candidate {
    let mut rewritten = statements.as_slice().to_vec();
    let span = rewritten[rewrite.replacement_index].span();
    rewritten[rewrite.replacement_index] = Statement::Instr(span, rewrite.replacement);

    let mut remove = rewrite.remove;
    remove.sort_unstable_by(|left, right| right.cmp(left));
    for index in remove {
        rewritten.remove(index);
    }
    Candidate {
        statements: statements.with_statements(rewritten),
    }
}

struct Candidate {
    statements: Statements,
}

impl Candidate {
    fn into_safe(self) -> Option<Statements> {
        if !validate_internal_labels(&self.statements) {
            return None;
        }
        assemble(&self.statements, false).ok()?;
        Some(self.statements)
    }
}

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
struct ByteOrigin {
    statement: usize,
    offset: usize,
}

struct ProgramLayout {
    names: Names,
    contexts: Vec<String>,
    positions: Vec<usize>,
    byte_origins: Vec<Option<ByteOrigin>>,
}

impl ProgramLayout {
    fn new(statements: &Statements) -> Result<Self, AssemblyError> {
        let (_, names) = compute_names(statements)?;
        let mut contexts = vec![];
        let mut positions = vec![];
        let mut byte_origins = vec![];
        let mut current_global = String::new();
        let mut position = 0usize;

        for (index, statement) in statements.iter().enumerate() {
            if let Statement::Label(_, name) = statement
                && !name.starts_with('.')
            {
                current_global = name.clone();
            }
            contexts.push(current_global.clone());

            let size = match statement {
                Statement::Directive(Directive::Org(_, location)) => {
                    position = *location;
                    0
                }
                Statement::Directive(directive) => directive.size(position as i32)? as usize,
                Statement::Instr(_, instruction) => instruction.size(),
                Statement::Label(..)
                | Statement::InternalLabel(..)
                | Statement::Variable(..)
                | Statement::Alias(..)
                | Statement::Comment(..) => 0,
                Statement::MacroCall(..) => {
                    panic!("Shouldn't be any macro calls left while optimizing")
                }
            };

            positions.push(position);
            if byte_origins.len() < position + size {
                byte_origins.resize(position + size, None);
            }
            for offset in 0..size {
                byte_origins[position + offset] = Some(ByteOrigin {
                    statement: index,
                    offset,
                });
            }
            position += size;
        }

        Ok(Self {
            names,
            contexts,
            positions,
            byte_origins,
        })
    }

    fn eval(&self, statement: usize, expression: &Expr) -> Option<usize> {
        let environment = self
            .names
            .as_env("Name", self.contexts.get(statement)?.as_str());
        let address = expression.eval(&environment).ok()?;
        usize::try_from(address).ok()
    }

    fn origin(&self, address: usize) -> Option<ByteOrigin> {
        self.byte_origins.get(address).copied().flatten()
    }
}

fn validate_internal_labels(statements: &Statements) -> bool {
    let Ok(layout) = ProgramLayout::new(statements) else {
        return false;
    };
    let mut pending_offsets = vec![];

    for (index, statement) in statements.iter().enumerate() {
        if let Statement::InternalLabel(_, _, offset) = statement {
            pending_offsets.push(*offset);
            continue;
        }
        if pending_offsets.is_empty() {
            continue;
        }

        let owner_position = layout.positions[index];
        let owner_size = match statement {
            Statement::Directive(Directive::Org(..)) => 0,
            Statement::Directive(directive) => {
                let Ok(size) = directive.size(owner_position as i32) else {
                    return false;
                };
                let Ok(size) = usize::try_from(size) else {
                    return false;
                };
                size
            }
            Statement::Instr(_, instruction) => instruction.size(),
            _ => 0,
        };
        if pending_offsets.iter().any(|offset| *offset >= owner_size) {
            return false;
        }
        pending_offsets.clear();
    }
    pending_offsets.is_empty()
}

fn has_interior_internal_label(statements: &Statements, owner_index: usize) -> bool {
    statements.as_slice()[..owner_index]
        .iter()
        .rev()
        .take_while(|statement| matches!(statement, Statement::InternalLabel(..)))
        .any(|statement| matches!(statement, Statement::InternalLabel(_, _, offset) if *offset > 0))
}

fn is_change_jump(statements: &Statements, jump_index: usize, layout: &ProgramLayout) -> bool {
    if !matches!(
        statements.as_slice().get(jump_index),
        Some(Statement::Instr(_, Instr::Jmpf(_)))
    ) {
        return false;
    }

    let Some(jump_position) = layout.positions.get(jump_index).copied() else {
        return false;
    };
    for previous_index in (0..jump_index).rev() {
        match &statements.as_slice()[previous_index] {
            Statement::Label(..)
            | Statement::InternalLabel(..)
            | Statement::Variable(..)
            | Statement::Alias(..)
            | Statement::Comment(..) => continue,
            Statement::Instr(_, instruction @ Instr::Not1(direct, bit)) => {
                return layout.positions[previous_index] + instruction.size() == jump_position
                    && layout.eval(previous_index, direct) == Some(0x10d)
                    && layout.eval(previous_index, bit) == Some(0);
            }
            _ => return false,
        }
    }
    false
}

fn control_flow_target(instruction: &Instr<Expr, IndirectionMode>) -> Option<&Expr> {
    match instruction {
        Instr::Jmp(target)
        | Instr::Jmpf(target)
        | Instr::Br(target)
        | Instr::Brf(target)
        | Instr::Bz(target)
        | Instr::Bnz(target)
        | Instr::Dbnz_d9(_, target)
        | Instr::Dbnz_Ri(_, target)
        | Instr::Be_i8(_, target)
        | Instr::Be_d9(_, target)
        | Instr::Be_Rj(_, _, target)
        | Instr::Bne_i8(_, target)
        | Instr::Bne_d9(_, target)
        | Instr::Bne_Rj(_, _, target)
        | Instr::Bp(_, _, target)
        | Instr::Bpc(_, _, target)
        | Instr::Bn(_, _, target)
        | Instr::Call(target)
        | Instr::Callf(target)
        | Instr::Callr(target) => Some(target),
        _ => None,
    }
}

fn with_control_flow_target(
    instruction: &Instr<Expr, IndirectionMode>,
    target: Expr,
) -> Option<Instr<Expr, IndirectionMode>> {
    Some(match instruction {
        Instr::Jmp(_) => Instr::Jmp(target),
        Instr::Jmpf(_) => Instr::Jmpf(target),
        Instr::Br(_) => Instr::Br(target),
        Instr::Brf(_) => Instr::Brf(target),
        Instr::Bz(_) => Instr::Bz(target),
        Instr::Bnz(_) => Instr::Bnz(target),
        Instr::Dbnz_d9(value, _) => Instr::Dbnz_d9(value.clone(), target),
        Instr::Dbnz_Ri(register, _) => Instr::Dbnz_Ri(*register, target),
        Instr::Be_i8(value, _) => Instr::Be_i8(value.clone(), target),
        Instr::Be_d9(value, _) => Instr::Be_d9(value.clone(), target),
        Instr::Be_Rj(register, value, _) => Instr::Be_Rj(*register, value.clone(), target),
        Instr::Bne_i8(value, _) => Instr::Bne_i8(value.clone(), target),
        Instr::Bne_d9(value, _) => Instr::Bne_d9(value.clone(), target),
        Instr::Bne_Rj(register, value, _) => Instr::Bne_Rj(*register, value.clone(), target),
        Instr::Bp(value, bit, _) => Instr::Bp(value.clone(), bit.clone(), target),
        Instr::Bpc(value, bit, _) => Instr::Bpc(value.clone(), bit.clone(), target),
        Instr::Bn(value, bit, _) => Instr::Bn(value.clone(), bit.clone(), target),
        Instr::Call(_) => Instr::Call(target),
        Instr::Callf(_) => Instr::Callf(target),
        Instr::Callr(_) => Instr::Callr(target),
        _ => return None,
    })
}

fn expression_contains_name(expression: &Expr) -> bool {
    match expression {
        Expr::Name(..) | Expr::InternalName(..) => true,
        Expr::Plus(_, left, right)
        | Expr::Minus(_, left, right)
        | Expr::Times(_, left, right)
        | Expr::Divide(_, left, right)
        | Expr::BitwiseXor(_, left, right)
        | Expr::BitwiseAnd(_, left, right)
        | Expr::BitwiseOr(_, left, right) => {
            expression_contains_name(left) || expression_contains_name(right)
        }
        Expr::UnaryMinus(_, expression)
        | Expr::UpperByte(_, expression)
        | Expr::LowerByte(_, expression) => expression_contains_name(expression),
        Expr::Number(..) => false,
        Expr::MacroLabel(..) | Expr::MacroArg(..) => {
            panic!("Shouldn't be any macro expressions left while optimizing")
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum BranchKind {
    Bz,
    Bnz,
    Bp,
    Bpc,
    Bn,
    DbnzDirect,
    DbnzIndirect,
    BeImmediate,
    BeDirect,
    BeIndirect,
    BneImmediate,
    BneDirect,
    BneIndirect,
}

impl BranchKind {
    fn of(instruction: &Instr<Expr, IndirectionMode>) -> Option<Self> {
        match instruction {
            Instr::Bz(_) => Some(Self::Bz),
            Instr::Bnz(_) => Some(Self::Bnz),
            Instr::Bp(..) => Some(Self::Bp),
            Instr::Bpc(..) => Some(Self::Bpc),
            Instr::Bn(..) => Some(Self::Bn),
            Instr::Dbnz_d9(..) => Some(Self::DbnzDirect),
            Instr::Dbnz_Ri(..) => Some(Self::DbnzIndirect),
            Instr::Be_i8(..) => Some(Self::BeImmediate),
            Instr::Be_d9(..) => Some(Self::BeDirect),
            Instr::Be_Rj(..) => Some(Self::BeIndirect),
            Instr::Bne_i8(..) => Some(Self::BneImmediate),
            Instr::Bne_d9(..) => Some(Self::BneDirect),
            Instr::Bne_Rj(..) => Some(Self::BneIndirect),
            _ => None,
        }
    }
}

fn branch_target(instruction: &Instr<Expr, IndirectionMode>) -> Option<&Expr> {
    match instruction {
        Instr::Bz(target)
        | Instr::Bnz(target)
        | Instr::Dbnz_d9(_, target)
        | Instr::Dbnz_Ri(_, target)
        | Instr::Be_i8(_, target)
        | Instr::Be_d9(_, target)
        | Instr::Be_Rj(_, _, target)
        | Instr::Bne_i8(_, target)
        | Instr::Bne_d9(_, target)
        | Instr::Bne_Rj(_, _, target)
        | Instr::Bp(_, _, target)
        | Instr::Bpc(_, _, target)
        | Instr::Bn(_, _, target) => Some(target),
        _ => None,
    }
}

fn retarget_branch(
    instruction: &Instr<Expr, IndirectionMode>,
    kind: BranchKind,
    target: Expr,
) -> Option<Instr<Expr, IndirectionMode>> {
    match (instruction, kind) {
        (Instr::Bz(_) | Instr::Bnz(_), BranchKind::Bz) => Some(Instr::Bz(target)),
        (Instr::Bz(_) | Instr::Bnz(_), BranchKind::Bnz) => Some(Instr::Bnz(target)),
        (Instr::Bp(direct, bit, _), BranchKind::Bp) => {
            Some(Instr::Bp(direct.clone(), bit.clone(), target))
        }
        (Instr::Bpc(direct, bit, _), BranchKind::Bpc) => {
            Some(Instr::Bpc(direct.clone(), bit.clone(), target))
        }
        (Instr::Bn(direct, bit, _), BranchKind::Bn) => {
            Some(Instr::Bn(direct.clone(), bit.clone(), target))
        }
        (Instr::Dbnz_d9(value, _), BranchKind::DbnzDirect) => {
            Some(Instr::Dbnz_d9(value.clone(), target))
        }
        (Instr::Dbnz_Ri(register, _), BranchKind::DbnzIndirect) => {
            Some(Instr::Dbnz_Ri(*register, target))
        }
        (Instr::Be_i8(value, _) | Instr::Bne_i8(value, _), BranchKind::BeImmediate) => {
            Some(Instr::Be_i8(value.clone(), target))
        }
        (Instr::Be_i8(value, _) | Instr::Bne_i8(value, _), BranchKind::BneImmediate) => {
            Some(Instr::Bne_i8(value.clone(), target))
        }
        (Instr::Be_d9(value, _) | Instr::Bne_d9(value, _), BranchKind::BeDirect) => {
            Some(Instr::Be_d9(value.clone(), target))
        }
        (Instr::Be_d9(value, _) | Instr::Bne_d9(value, _), BranchKind::BneDirect) => {
            Some(Instr::Bne_d9(value.clone(), target))
        }
        (
            Instr::Be_Rj(register, value, _) | Instr::Bne_Rj(register, value, _),
            BranchKind::BeIndirect,
        ) => Some(Instr::Be_Rj(*register, value.clone(), target)),
        (
            Instr::Be_Rj(register, value, _) | Instr::Bne_Rj(register, value, _),
            BranchKind::BneIndirect,
        ) => Some(Instr::Bne_Rj(*register, value.clone(), target)),
        _ => None,
    }
}

fn unconditional_target(instruction: &Instr<Expr, IndirectionMode>) -> Option<&Expr> {
    match instruction {
        Instr::Br(target) | Instr::Jmp(target) | Instr::Jmpf(target) => Some(target),
        _ => None,
    }
}

#[derive(Clone, Copy)]
enum PatternShape {
    Inverted,
    Trampoline,
}

struct BranchPattern {
    source: BranchKind,
    replacement: BranchKind,
    shape: PatternShape,
}

impl BranchPattern {
    fn match_at(
        &self,
        statements: &Statements,
        index: &ProgramIndex,
        position: usize,
    ) -> Option<Rewrite> {
        match self.shape {
            PatternShape::Inverted => self.match_inverted(statements, index, position),
            PatternShape::Trampoline => self.match_trampoline(statements, index, position),
        }
    }

    fn match_inverted(
        &self,
        statements: &Statements,
        index: &ProgramIndex,
        position: usize,
    ) -> Option<Rewrite> {
        let positions = index.significant.get(position..position + 3)?;
        let branch_index = positions[0];
        let jump_index = positions[1];
        let skip_index = positions[2];

        let branch = instruction_at(statements, branch_index)?;
        if BranchKind::of(branch)? != self.source {
            return None;
        }
        let target = unconditional_target(instruction_at(statements, jump_index)?)?;
        let skip = local_label_at(statements, skip_index)?;
        if !targets_local_label(
            branch_target(branch)?,
            &index.contexts[branch_index],
            skip,
            &index.contexts[skip_index],
        ) {
            return None;
        }

        Some(Rewrite {
            replacement_index: branch_index,
            replacement: retarget_branch(branch, self.replacement, target.clone())?,
            remove: vec![jump_index],
        })
    }

    fn match_trampoline(
        &self,
        statements: &Statements,
        index: &ProgramIndex,
        position: usize,
    ) -> Option<Rewrite> {
        let positions = index.significant.get(position..position + 5)?;
        let branch_index = positions[0];
        let skip_jump_index = positions[1];
        let true_index = positions[2];
        let target_jump_index = positions[3];
        let skip_index = positions[4];

        let branch = instruction_at(statements, branch_index)?;
        if BranchKind::of(branch)? != self.source {
            return None;
        }
        let true_label = local_label_at(statements, true_index)?;
        let skip_label = local_label_at(statements, skip_index)?;
        if !targets_local_label(
            branch_target(branch)?,
            &index.contexts[branch_index],
            true_label,
            &index.contexts[true_index],
        ) || !targets_local_label(
            unconditional_target(instruction_at(statements, skip_jump_index)?)?,
            &index.contexts[skip_jump_index],
            skip_label,
            &index.contexts[skip_index],
        ) {
            return None;
        }

        let true_key = LabelKey::local(&index.contexts[true_index], true_label);
        if index.references.get(&true_key).copied() != Some(1) {
            return None;
        }

        let target = unconditional_target(instruction_at(statements, target_jump_index)?)?;
        Some(Rewrite {
            replacement_index: branch_index,
            replacement: retarget_branch(branch, self.replacement, target.clone())?,
            remove: vec![skip_jump_index, true_index, target_jump_index],
        })
    }
}

struct BranchPatternBuilder {
    patterns: Vec<BranchPattern>,
}

impl BranchPatternBuilder {
    fn new() -> Self {
        Self { patterns: vec![] }
    }

    fn inverse_pair(mut self, first: BranchKind, second: BranchKind) -> Self {
        self.patterns.push(BranchPattern {
            source: first,
            replacement: second,
            shape: PatternShape::Inverted,
        });
        self.patterns.push(BranchPattern {
            source: second,
            replacement: first,
            shape: PatternShape::Inverted,
        });
        self
    }

    fn trampoline(mut self, kind: BranchKind) -> Self {
        self.patterns.push(BranchPattern {
            source: kind,
            replacement: kind,
            shape: PatternShape::Trampoline,
        });
        self
    }

    fn build(self) -> Vec<BranchPattern> {
        self.patterns
    }
}

fn branch_patterns() -> Vec<BranchPattern> {
    use BranchKind::*;

    BranchPatternBuilder::new()
        // BZO and BNZO.
        .inverse_pair(Bz, Bnz)
        // BEO and BNEO, for each operand form.
        .inverse_pair(BeImmediate, BneImmediate)
        .inverse_pair(BeDirect, BneDirect)
        .inverse_pair(BeIndirect, BneIndirect)
        // BPO, BPCO, BNO, and both DBNZO operand forms.
        .trampoline(Bp)
        .trampoline(Bpc)
        .trampoline(Bn)
        .trampoline(DbnzDirect)
        .trampoline(DbnzIndirect)
        .build()
}

struct Rewrite {
    replacement_index: usize,
    replacement: Instr<Expr, IndirectionMode>,
    remove: Vec<usize>,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
struct LabelKey {
    global: String,
    local: String,
}

impl LabelKey {
    fn local(global: &str, local: &str) -> Self {
        Self {
            global: global.to_owned(),
            local: local.to_owned(),
        }
    }
}

struct ProgramIndex {
    significant: Vec<usize>,
    contexts: Vec<String>,
    references: HashMap<LabelKey, usize>,
}

impl ProgramIndex {
    fn new(statements: &Statements) -> Self {
        let mut significant = vec![];
        let mut contexts = vec![];
        let mut references = HashMap::new();
        let mut current_global = String::new();

        for (index, statement) in statements.iter().enumerate() {
            if let Statement::Label(_, name) = statement
                && !name.starts_with('.')
            {
                current_global = name.clone();
            }
            contexts.push(current_global.clone());
            if !matches!(statement, Statement::Comment(_)) {
                significant.push(index);
            }
            count_statement_references(statement, &current_global, &mut references);
        }

        Self {
            significant,
            contexts,
            references,
        }
    }
}

fn instruction_at(statements: &Statements, index: usize) -> Option<&Instr<Expr, IndirectionMode>> {
    match &statements.as_slice()[index] {
        Statement::Instr(_, instruction) => Some(instruction),
        _ => None,
    }
}

fn local_label_at(statements: &Statements, index: usize) -> Option<&str> {
    match &statements.as_slice()[index] {
        Statement::Label(_, name) if name.starts_with('.') => Some(name),
        _ => None,
    }
}

fn targets_local_label(
    expression: &Expr,
    expression_context: &str,
    label: &str,
    label_context: &str,
) -> bool {
    matches!(expression, Expr::Name(_, name) if name == label)
        && expression_context == label_context
}

fn count_statement_references(
    statement: &Statement,
    current_global: &str,
    references: &mut HashMap<LabelKey, usize>,
) {
    match statement {
        Statement::Instr(_, instruction) => {
            for expression in instruction.expression_args() {
                count_expression_references(expression, current_global, references);
            }
        }
        Statement::Variable(_, _, expression) | Statement::Alias(_, _, expression) => {
            count_expression_references(expression, current_global, references);
        }
        Statement::Directive(directive) => {
            count_directive_references(directive, current_global, references);
        }
        Statement::MacroCall(_, _, arguments) => {
            for argument in arguments {
                match argument {
                    Arg::Imm(expression) | Arg::Ex(expression) => {
                        count_expression_references(expression, current_global, references);
                    }
                    Arg::IM(..) | Arg::MacroArg(..) => {}
                }
            }
        }
        Statement::Label(..) | Statement::InternalLabel(..) | Statement::Comment(_) => {}
    }
}

fn count_directive_references(
    directive: &Directive,
    current_global: &str,
    references: &mut HashMap<LabelKey, usize>,
) {
    match directive {
        Directive::Byte(_, values) => {
            for value in values {
                if let ByteValue::Expr(expression) = value {
                    count_expression_references(expression, current_global, references);
                }
            }
        }
        Directive::Word(_, expressions) => {
            for expression in expressions {
                count_expression_references(expression, current_global, references);
            }
        }
        Directive::Cnop(_, first, second) => {
            count_expression_references(first, current_global, references);
            count_expression_references(second, current_global, references);
        }
        Directive::Include(_, IncludeType::Icon(speed, eyecatch), _) => {
            if let Some(expression) = speed {
                count_expression_references(expression, current_global, references);
            }
            if let Some(ByteValue::Expr(expression)) = eyecatch {
                count_expression_references(expression, current_global, references);
            }
        }
        Directive::Org(..)
        | Directive::Text(..)
        | Directive::String(..)
        | Directive::Include(..) => {}
    }
}

fn count_expression_references(
    expression: &Expr,
    current_global: &str,
    references: &mut HashMap<LabelKey, usize>,
) {
    match expression {
        Expr::Name(_, name) if name.starts_with('.') => {
            *references
                .entry(LabelKey::local(current_global, name))
                .or_default() += 1;
        }
        Expr::Plus(_, left, right)
        | Expr::Minus(_, left, right)
        | Expr::Times(_, left, right)
        | Expr::Divide(_, left, right)
        | Expr::BitwiseXor(_, left, right)
        | Expr::BitwiseAnd(_, left, right)
        | Expr::BitwiseOr(_, left, right) => {
            count_expression_references(left, current_global, references);
            count_expression_references(right, current_global, references);
        }
        Expr::UnaryMinus(_, expression)
        | Expr::UpperByte(_, expression)
        | Expr::LowerByte(_, expression) => {
            count_expression_references(expression, current_global, references);
        }
        Expr::Name(..)
        | Expr::InternalName(..)
        | Expr::Number(..)
        | Expr::MacroLabel(..)
        | Expr::MacroArg(..) => {}
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::{BranchKind, BranchPatternBuilder, optimize_jumps};
    use crate::ast::{Statement, Statements};
    use crate::expression::{Expr, IndirectionMode};
    use crate::instruction::Instr;

    #[test]
    fn pattern_builder_declares_both_directions_of_an_inverse_pair() {
        let patterns = BranchPatternBuilder::new()
            .inverse_pair(BranchKind::Bz, BranchKind::Bnz)
            .build();

        assert_eq!(patterns.len(), 2);
        assert_eq!(patterns[0].source, BranchKind::Bz);
        assert_eq!(patterns[0].replacement, BranchKind::Bnz);
        assert_eq!(patterns[1].source, BranchKind::Bnz);
        assert_eq!(patterns[1].replacement, BranchKind::Bz);
    }

    #[test]
    fn optimized_ast_materializes_internal_labels_as_numbers() {
        let statements = Statements::new(
            HashMap::new(),
            vec![
                Statement::instr(Instr::<Expr, IndirectionMode>::Jmpf(Expr::num(3))),
                Statement::instr(Instr::Nop),
            ],
        );

        let optimized = optimize_jumps(&statements).unwrap();

        assert!(
            optimized
                .iter()
                .all(|statement| !matches!(statement, Statement::InternalLabel(..)))
        );
        assert!(matches!(
            &optimized.as_slice()[0],
            Statement::Instr(_, Instr::Br(Expr::Number(_, 2, _)))
        ));
    }
}
