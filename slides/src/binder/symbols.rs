use crate::instruction_converter::Program;

use super::{FunctionDeclarationBody, typing::FunctionType};

#[derive(Debug)]
pub struct Library {
    pub program: Program,
    pub functions: Vec<FunctionSymbol>,
}

impl Library {
    pub fn error() -> Self {
        Self {
            program: Program::error(),
            functions: vec![],
        }
    }
}

#[derive(Debug)]
pub struct FunctionSymbol {
    pub name: String,
    pub function_type: FunctionType,
    pub label_index: u64,
}

impl From<FunctionDeclarationBody<'_>> for FunctionSymbol {
    fn from(it: FunctionDeclarationBody) -> Self {
        Self {
            name: it.function_name.into(),
            function_type: it.function_type,
            label_index: it.function_id,
        }
    }
}