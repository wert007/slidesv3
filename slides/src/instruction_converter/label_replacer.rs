use crate::{DebugFlags, instruction_converter::instruction::op_codes::OpCode};

use super::{instruction::Instruction, InstructionOrLabelReference};

pub(crate) fn replace_labels(instructions: Vec<InstructionOrLabelReference>, debug_flags: DebugFlags) -> Vec<Instruction> {
    let labels = collect_labels(&instructions);
    if debug_flags.print_labels {
        for (index, label) in labels.iter().enumerate() {
            println!("L{}: #{}", index, label);
        }
    }
    instructions
        .into_iter()
        .map(|i_l| match i_l {
            InstructionOrLabelReference::Instruction(i) => i,
            InstructionOrLabelReference::LabelReference(l) => Instruction::load_pointer(labels[l.0]),
        })
        .map(|i| {
            match i.op_code {
                OpCode::Label => Instruction::noop(),
                OpCode::Jump => Instruction::jump(labels[i.arg as usize]),
                OpCode::JumpIfFalse => Instruction::jump_if_false(labels[i.arg as usize]),
                OpCode::JumpIfTrue => Instruction::jump_if_true(labels[i.arg as usize]),
                _ => i,
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
                    while result.len() <= instruction.arg as _ {
                        // Extend the labels to the needed label index and
                        // insert improbable results, so if there will be a bug,
                        // where labels are not defined, it will crash loudly.
                        result.push(u64::MAX);
                    }
                    result[instruction.arg as usize] = index as _;
                }
                _ => {}
            },
            InstructionOrLabelReference::LabelReference(_) => {}
        }
    }
    result
}
