use std::path::Path;

use walrus::Module;
pub mod cli;
mod ir;

#[derive(Debug)]
pub enum WasmError {
    Anyhow(anyhow::Error),
}

pub fn build<O: AsRef<Path>>(path: impl AsRef<Path>, output: Option<O>) -> Result<(), WasmError> {
    let path = path.as_ref();
    let module = load_module(path)?;
    println!("module: {module:#?}");

    for func in module.functions() {
        match &func.kind {
            walrus::FunctionKind::Import(function_import) => todo!(),
            walrus::FunctionKind::Local(local_function) => {
                let block = local_function.block(local_function.entry_block());
                for (instr, id) in block.iter() {
                    match instr {
                        walrus::ir::Instr::Block(_) => todo!(),
                        walrus::ir::Instr::Loop(_) => todo!(),
                        walrus::ir::Instr::Call(_) => todo!(),
                        walrus::ir::Instr::CallIndirect(_) => todo!(),
                        walrus::ir::Instr::LocalGet(_) => todo!(),
                        walrus::ir::Instr::LocalSet(_) => todo!(),
                        walrus::ir::Instr::LocalTee(_) => todo!(),
                        walrus::ir::Instr::GlobalGet(_) => todo!(),
                        walrus::ir::Instr::GlobalSet(_) => todo!(),
                        walrus::ir::Instr::Const(_) => todo!(),
                        walrus::ir::Instr::Binop(_) => todo!(),
                        walrus::ir::Instr::Unop(_) => todo!(),
                        walrus::ir::Instr::Select(_) => todo!(),
                        walrus::ir::Instr::Unreachable(_) => todo!(),
                        walrus::ir::Instr::Br(_) => todo!(),
                        walrus::ir::Instr::BrIf(_) => todo!(),
                        walrus::ir::Instr::IfElse(_) => todo!(),
                        walrus::ir::Instr::BrTable(_) => todo!(),
                        walrus::ir::Instr::Drop(_) => todo!(),
                        walrus::ir::Instr::Return(_) => todo!(),
                        walrus::ir::Instr::MemorySize(_) => todo!(),
                        walrus::ir::Instr::MemoryGrow(_) => todo!(),
                        walrus::ir::Instr::MemoryInit(_) => todo!(),
                        walrus::ir::Instr::DataDrop(_) => todo!(),
                        walrus::ir::Instr::MemoryCopy(_) => todo!(),
                        walrus::ir::Instr::MemoryFill(_) => todo!(),
                        walrus::ir::Instr::Load(_) => todo!(),
                        walrus::ir::Instr::Store(_) => todo!(),
                        walrus::ir::Instr::AtomicRmw(_) => todo!(),
                        walrus::ir::Instr::Cmpxchg(_) => todo!(),
                        walrus::ir::Instr::AtomicNotify(_) => todo!(),
                        walrus::ir::Instr::AtomicWait(_) => todo!(),
                        walrus::ir::Instr::AtomicFence(_) => todo!(),
                        walrus::ir::Instr::TableGet(_) => todo!(),
                        walrus::ir::Instr::TableSet(_) => todo!(),
                        walrus::ir::Instr::TableGrow(_) => todo!(),
                        walrus::ir::Instr::TableSize(_) => todo!(),
                        walrus::ir::Instr::TableFill(_) => todo!(),
                        walrus::ir::Instr::RefNull(_) => todo!(),
                        walrus::ir::Instr::RefIsNull(_) => todo!(),
                        walrus::ir::Instr::RefFunc(_) => todo!(),
                        walrus::ir::Instr::V128Bitselect(_) => todo!(),
                        walrus::ir::Instr::I8x16Swizzle(_) => todo!(),
                        walrus::ir::Instr::I8x16Shuffle(_) => todo!(),
                        walrus::ir::Instr::LoadSimd(_) => todo!(),
                        walrus::ir::Instr::TableInit(_) => todo!(),
                        walrus::ir::Instr::ElemDrop(_) => todo!(),
                        walrus::ir::Instr::TableCopy(_) => todo!(),
                        walrus::ir::Instr::ReturnCall(_) => todo!(),
                        walrus::ir::Instr::ReturnCallIndirect(_) => todo!(),
                    }
                    
                }
            },
            walrus::FunctionKind::Uninitialized(_) => todo!(),
        }
    }
    
    Ok(())
}

pub fn load_module(path: impl AsRef<Path>) -> Result<Module, WasmError> {
    let path = path.as_ref();
    let module = walrus::Module::from_file(path)?;
    Ok(module)
}

impl From<anyhow::Error> for WasmError {
    fn from(value: anyhow::Error) -> Self {
        WasmError::Anyhow(value)
    }
}
