use std::{
    convert::TryFrom,
    path::{Path, PathBuf},
};

use crate::instruction_converter::{
    instruction::{op_codes::OpCode, Instruction},
    InstructionOrLabelReference, LabelReference, Program,
};

use super::{
    typing::{FunctionType, Type},
    BoundStructFieldSymbol, BoundStructSymbol, FunctionDeclarationBody,
};

#[derive(Debug, Clone)]
pub struct Library {
    pub name: String,
    pub instructions: Vec<InstructionOrLabelReference>,
    pub startup: Vec<InstructionOrLabelReference>,
    pub program: Program,
    pub functions: Vec<FunctionSymbol>,
    pub structs: Vec<StructSymbol>,
    pub has_errors: bool,
    pub path: PathBuf,
    pub referenced_libraries: Vec<Library>,
}

impl Library {
    pub fn error() -> Self {
        Self {
            name: "error".into(),
            instructions: vec![],
            startup: vec![],
            program: Program::error(),
            functions: vec![],
            structs: vec![],
            has_errors: true,
            path: PathBuf::new(),
            referenced_libraries: vec![],
        }
    }

    pub fn find_imported_library_by_path(&self, path: &Path) -> Option<(String, Library)> {
        if self.path == path {
            return Some((self.name.clone(), self.clone()));
        }
        for lib in &self.referenced_libraries {
            if let Some((rel_path, lib)) = lib.find_imported_library_by_path(path) {
                return Some((format!("{}.{}", self.name, rel_path), lib));
            }
        }
        None
    }

    pub fn look_up_function_by_name(&self, name: &str) -> Option<&FunctionSymbol> {
        self.functions.iter().find(|f| f.name == name)
    }

    pub fn relocate_labels(&mut self, label_offset: usize) {
        for function in self.functions.iter_mut() {
            function.label_index += label_offset as u64;
        }
        for inst in self.instructions.iter_mut().chain(self.startup.iter_mut()) {
            match inst {
                InstructionOrLabelReference::Instruction(Instruction {
                    arg,
                    op_code: OpCode::Jump | OpCode::JumpIfFalse | OpCode::JumpIfTrue | OpCode::Label,
                    ..
                }) => {
                    *arg += label_offset as u64;
                }
                InstructionOrLabelReference::LabelReference(LabelReference {
                    label_reference,
                    ..
                }) => {
                    *label_reference += label_offset;
                }
                _ => {}
            }
        }
    }

    pub fn relocate_structs(&mut self, struct_offset: usize) {
        for function in self.functions.iter_mut() {
            for parameter in function.function_type.parameter_types.iter_mut() {
                if let Type::StructReference(index) = parameter {
                    *index += struct_offset as u64;
                }
            }
        }
        for strct in self.structs.iter_mut() {
            for field in strct.fields.iter_mut() {
                if let Type::StructReference(index) = &mut field.type_ {
                    *index += struct_offset as u64;
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSymbol {
    pub name: String,
    pub function_type: FunctionType,
    pub label_index: u64,
    pub is_member_function: bool,
}

impl From<FunctionDeclarationBody<'_>> for FunctionSymbol {
    fn from(it: FunctionDeclarationBody) -> Self {
        Self {
            name: it.function_name.into(),
            function_type: it.function_type,
            label_index: it.function_id,
            is_member_function: it.is_struct_function,
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructSymbol {
    pub name: String,
    pub fields: Vec<StructFieldSymbol>,
    pub function_table: StructFunctionTable,
}

impl StructSymbol {
    pub fn field(&self, name: &str) -> Option<&StructFieldSymbol> {
        self.fields.iter().find(|f| f.name == name)
    }
}

impl From<BoundStructSymbol<'_>> for StructSymbol {
    fn from(it: BoundStructSymbol<'_>) -> Self {
        Self {
            name: it.name.into(),
            fields: it.fields.into_iter().map(Into::into).collect(),
            function_table: it.function_table,
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructFieldSymbol {
    pub name: String,
    pub type_: Type,
    pub offset: u64,
    pub is_read_only: bool,
}

impl From<BoundStructFieldSymbol<'_>> for StructFieldSymbol {
    fn from(it: BoundStructFieldSymbol<'_>) -> Self {
        Self {
            name: it.name.into(),
            type_: it.type_,
            offset: it.offset,
            is_read_only: it.is_read_only,
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct StructFunctionTable {
    pub constructor_function: Option<FunctionSymbol>,
}

impl StructFunctionTable {
    pub fn set(&mut self, kind: StructFunctionKind, function: FunctionSymbol) {
        match kind {
            StructFunctionKind::Constructor => self.constructor_function = Some(function),
        }
    }

    fn relocate_labels(&mut self, label_offset: usize) {
        self.constructor_function
            .iter_mut()
            .for_each(|f| f.label_index += label_offset as u64);
    }

    fn relocate_structs(&mut self, struct_offset: usize) {
        self.constructor_function.iter_mut().for_each(|f| {
            f.function_type
                .parameter_types
                .iter_mut()
                .filter_map(|p| {
                    if let Type::StructReference(id) = p {
                        Some(id)
                    } else {
                        None
                    }
                })
                .for_each(|id| *id += struct_offset as u64)
        });
    }
}

pub enum StructFunctionKind {
    Constructor,
}

impl<'a> TryFrom<&'a str> for StructFunctionKind {
    type Error = &'a str;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        match value {
            "$constructor" => Ok(Self::Constructor),
            _ => Err(value),
        }
    }
}
