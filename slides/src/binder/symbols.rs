use crate::instruction_converter::{InstructionOrLabelReference, LabelReference, Program, instruction::{Instruction, op_codes::OpCode}};

use super::{typing::FunctionType, FunctionDeclarationBody};

#[derive(Debug)]
pub struct Library {
    pub instructions: Vec<InstructionOrLabelReference>,
    pub program: Program,
    pub functions: Vec<FunctionSymbol>,
}

impl Library {
    pub fn error() -> Self {
        Self {
            instructions: vec![],
            program: Program::error(),
            functions: vec![],
        }
    }

    pub fn look_up_function_by_name(&self, name: &str) -> Option<&FunctionSymbol> {
        self.functions.iter().find(|f| f.name == name)
    }

    pub fn relocate(&mut self, label_offset: usize) {
        for inst in self.instructions.iter_mut() {
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
