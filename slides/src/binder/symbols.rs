use crate::instruction_converter::{InstructionOrLabelReference, LabelReference, Program, instruction::{Instruction, op_codes::OpCode}};

use super::{BoundStructFieldSymbol, BoundStructSymbol, FunctionDeclarationBody, typing::{FunctionType, Type}};

#[derive(Debug)]
pub struct Library {
    pub instructions: Vec<InstructionOrLabelReference>,
    pub startup: Vec<InstructionOrLabelReference>,
    pub program: Program,
    pub functions: Vec<FunctionSymbol>,
    pub structs: Vec<StructSymbol>,
    pub has_errors: bool,
}

impl Library {
    pub fn error() -> Self {
        Self {
            instructions: vec![],
            startup: vec![],
            program: Program::error(),
            functions: vec![],
            structs: vec![],
            has_errors: true,
        }
    }

    pub fn look_up_function_by_name(&self, name: &str) -> Option<&FunctionSymbol> {
        self.functions.iter().find(|f| f.name == name)
    }

    pub fn relocate_static_memory(&mut self, label_offset: usize) {
        for inst in self.instructions.iter_mut().chain(self.startup.iter_mut()) {
            match inst {
                InstructionOrLabelReference::Instruction(Instruction {
                    arg,
                    op_code: OpCode::Jump | OpCode::JumpIfFalse | OpCode::JumpIfTrue | OpCode::Label,
                    ..
                }) => {
                    *arg += label_offset as u64;
                }
                InstructionOrLabelReference::LabelReference(LabelReference {label_reference, .. }) => {
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

#[derive(Debug)]
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
}

impl StructSymbol {
    pub fn field(&self, name: &str) -> Option<&StructFieldSymbol> {
        self.fields.iter().find(|f|f.name == name)
    }
}

impl From<BoundStructSymbol<'_>> for StructSymbol {
    fn from(it: BoundStructSymbol<'_>) -> Self {
        Self {
            name: it.name.into(),
            fields: it.fields.into_iter().map(Into::into).collect(),
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
