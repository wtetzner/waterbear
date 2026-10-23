use std::collections::{BTreeMap, BTreeSet};

use walrus::{
    DataId, ElementId, FunctionId, GlobalId, GlobalKind, LocalId, MemoryId, Module, TableId, TypeId,
};

pub(super) struct Names {
    functions: BTreeMap<FunctionId, String>,
    globals: BTreeMap<GlobalId, String>,
    locals: BTreeMap<LocalId, String>,
    memories: BTreeMap<MemoryId, String>,
    tables: BTreeMap<TableId, String>,
    types: BTreeMap<TypeId, String>,
    data: BTreeMap<DataId, String>,
    elements: BTreeMap<ElementId, String>,
}

impl Names {
    pub(super) fn new(module: &Module) -> Self {
        let mut used = BTreeSet::new();
        let mut functions = BTreeMap::new();
        for function in module.funcs.iter() {
            let preferred = function
                .name
                .clone()
                .or_else(|| {
                    module
                        .imports
                        .get_imported_func(function.id())
                        .map(|import| format!("{}.{}", import.module, import.name))
                })
                .or_else(|| {
                    module
                        .exports
                        .get_exported_func(function.id())
                        .map(|export| export.name.clone())
                })
                .unwrap_or_else(|| format!("func{}", function.id().index()));
            functions.insert(function.id(), symbol(&preferred, &mut used));
        }

        let mut used = BTreeSet::new();
        let mut globals = BTreeMap::new();
        for global in module.globals.iter() {
            let preferred = global
                .name
                .clone()
                .or_else(|| match global.kind {
                    GlobalKind::Import(import) => {
                        let import = module.imports.get(import);
                        Some(format!("{}.{}", import.module, import.name))
                    }
                    GlobalKind::Local(_) => None,
                })
                .or_else(|| {
                    module
                        .exports
                        .get_exported_global(global.id())
                        .map(|export| export.name.clone())
                })
                .unwrap_or_else(|| format!("global{}", global.id().index()));
            globals.insert(global.id(), symbol(&preferred, &mut used));
        }

        let mut used = BTreeSet::new();
        let locals = module
            .locals
            .iter()
            .map(|local| {
                let preferred = local
                    .name
                    .clone()
                    .unwrap_or_else(|| format!("local{}", local.id().index()));
                (local.id(), symbol(&preferred, &mut used))
            })
            .collect();

        let mut used = BTreeSet::new();
        let memories = module
            .memories
            .iter()
            .map(|memory| {
                let preferred = memory
                    .name
                    .clone()
                    .or_else(|| {
                        memory.import.map(|import| {
                            let import = module.imports.get(import);
                            format!("{}.{}", import.module, import.name)
                        })
                    })
                    .or_else(|| {
                        module
                            .exports
                            .get_exported_memory(memory.id())
                            .map(|export| export.name.clone())
                    })
                    .unwrap_or_else(|| format!("memory{}", memory.id().index()));
                (memory.id(), symbol(&preferred, &mut used))
            })
            .collect();

        let mut used = BTreeSet::new();
        let tables = module
            .tables
            .iter()
            .map(|table| {
                let preferred = table
                    .name
                    .clone()
                    .or_else(|| {
                        table.import.map(|import| {
                            let import = module.imports.get(import);
                            format!("{}.{}", import.module, import.name)
                        })
                    })
                    .or_else(|| {
                        module
                            .exports
                            .get_exported_table(table.id())
                            .map(|export| export.name.clone())
                    })
                    .unwrap_or_else(|| format!("table{}", table.id().index()));
                (table.id(), symbol(&preferred, &mut used))
            })
            .collect();

        let mut used = BTreeSet::new();
        let types = module
            .types
            .iter()
            .map(|ty| {
                let preferred = ty
                    .name
                    .clone()
                    .unwrap_or_else(|| format!("type{}", ty.id().index()));
                (ty.id(), symbol(&preferred, &mut used))
            })
            .collect();

        let mut used = BTreeSet::new();
        let data = module
            .data
            .iter()
            .map(|data| {
                let preferred = data
                    .name
                    .clone()
                    .unwrap_or_else(|| format!("data{}", data.id().index()));
                (data.id(), symbol(&preferred, &mut used))
            })
            .collect();

        let mut used = BTreeSet::new();
        let elements = module
            .elements
            .iter()
            .map(|element| {
                let preferred = element
                    .name
                    .clone()
                    .unwrap_or_else(|| format!("elem{}", element.id().index()));
                (element.id(), symbol(&preferred, &mut used))
            })
            .collect();

        Self {
            functions,
            globals,
            locals,
            memories,
            tables,
            types,
            data,
            elements,
        }
    }

    pub(super) fn function(&self, id: FunctionId) -> String {
        self.functions[&id].clone()
    }

    pub(super) fn global(&self, id: GlobalId) -> String {
        self.globals[&id].clone()
    }

    pub(super) fn local(&self, id: LocalId) -> String {
        self.locals[&id].clone()
    }

    pub(super) fn memory(&self, id: MemoryId) -> String {
        self.memories[&id].clone()
    }

    pub(super) fn table(&self, id: TableId) -> String {
        self.tables[&id].clone()
    }

    pub(super) fn ty(&self, id: TypeId) -> String {
        self.types[&id].clone()
    }

    pub(super) fn data(&self, id: DataId) -> String {
        self.data[&id].clone()
    }

    pub(super) fn element(&self, id: ElementId) -> String {
        self.elements[&id].clone()
    }
}
fn symbol(preferred: &str, used: &mut BTreeSet<String>) -> String {
    let base = identifier(preferred);
    let mut candidate = format!("${base}");
    let mut suffix = 2;
    while !used.insert(candidate.clone()) {
        candidate = format!("${base}_{suffix}");
        suffix += 1;
    }
    candidate
}

pub(super) fn identifier(value: &str) -> String {
    let value = value.trim_start_matches('$');
    let value: String = value
        .chars()
        .map(|character| {
            if character.is_ascii_alphanumeric() || matches!(character, '_' | '-' | '.') {
                character
            } else {
                '_'
            }
        })
        .collect();
    if value.is_empty() {
        "unnamed".to_owned()
    } else {
        value
    }
}
