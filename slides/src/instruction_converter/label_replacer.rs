use crate::{DebugFlags, instruction_converter::instruction::op_codes::OpCode};

use super::{instruction::Instruction, InstructionOrLabelReference};

pub(crate) fn replace_labels(instructions: Vec<InstructionOrLabelReference>, debug_flags: DebugFlags) -> Vec<Instruction> {
    let labels = collect_labels(&instructions);
    instructions
        .into_iter()
        .map(|i_l| match i_l {
            InstructionOrLabelReference::Instruction(i) => i,
            InstructionOrLabelReference::LabelReference(l) => Instruction::load_pointer(labels[l.0]),
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

fn collect_labels(instructions: &[InstructionOrLabelReference]) -> Vec<u64> {
    let mut result = vec![];
    for (index, instruction) in instructions.iter().enumerate() {
        match instruction {
            InstructionOrLabelReference::Instruction(instruction) => match instruction.op_code {
                OpCode::Label => {
                    result.push(index as _);
                }
                _ => {}
            },
            InstructionOrLabelReference::LabelReference(_) => {}
        }
    }
    result
}
