use crate::instruction_converter::instruction::op_codes::OpCode;

use super::{instruction::Instruction, InstructionOrLabel};

pub(crate) fn replace_labels(instructions: Vec<InstructionOrLabel>) -> Vec<Instruction> {
    let labels = collect_labels(&instructions);
    instructions
        .into_iter()
        .map(|i_l| match i_l {
            InstructionOrLabel::Instruction(i) => i,
            InstructionOrLabel::Label(l) => Instruction::load_pointer(labels[l.0]),
        })
        .map(|i| {
            if i.op_code == OpCode::Label {
                Instruction::noop()
            } else {
                i
            }
        })
        .collect()
}

fn collect_labels(instructions: &[InstructionOrLabel]) -> Vec<u64> {
    let mut result = vec![];
    for (index, instruction) in instructions.iter().enumerate() {
        match instruction {
            InstructionOrLabel::Instruction(instruction) => match instruction.op_code {
                OpCode::Label => {
                    result.push(index as _);
                }
                _ => {}
            },
            InstructionOrLabel::Label(_) => {}
        }
    }
    result
}
