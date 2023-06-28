use crate::{
    binder::typing::TypeCollection, instruction_converter::instruction::op_codes::OpCode,
    DebugFlags,
};

use super::{instruction::Instruction, InstructionOrLabelReference};

pub(crate) fn replace_labels(
    instructions: Vec<InstructionOrLabelReference>,
    debug_flags: DebugFlags,
    types: &mut TypeCollection,
) -> Vec<Instruction> {
    let labels = collect_labels(&instructions);
    if debug_flags.print_labels {
        for (index, label) in labels.iter().enumerate() {
            println!("L{:X}: #{:x}", index, label);
        }
    }
    types.for_each_type(|t| {
        if let Some(s) = t.as_struct_type_mut() {
            for f in s.function_table.function_symbols_iter_mut() {
                f.function_label = labels[f.function_label as usize];
            }
        }
    });
    types.for_each_generic_type(|t| {
        if let Some(s) = t.as_struct_type_mut() {
            for f in s.function_table.function_symbols_iter_mut() {
                f.function_label = labels[f.function_label as usize];
            }
        }
    });
    instructions
        .into_iter()
        .map(|i_l| match i_l {
            InstructionOrLabelReference::Instruction(i) => i,
            InstructionOrLabelReference::LabelReference(l) => {
                Instruction::load_pointer(labels[l.label_reference], l.location)
            }
        })
        .map(|i| {
            // TODO: We loose the span here. This is easily fixable.
            match i.op_code {
                OpCode::Label => Instruction::noop(i.location),
                OpCode::Jump => Instruction::jump(labels[i.arg as usize], i.location),
                OpCode::JumpIfFalse => {
                    Instruction::jump_if_false(labels[i.arg as usize], i.location)
                }
                OpCode::JumpIfTrue => Instruction::jump_if_true(labels[i.arg as usize], i.location),
                _ => i,
            }
        })
        .collect()
}

fn collect_labels(instructions: &[InstructionOrLabelReference]) -> Vec<u64> {
    let mut result = vec![];
    for (index, instruction) in instructions.iter().enumerate() {
        match instruction {
            InstructionOrLabelReference::Instruction(instruction) => {
                if instruction.op_code == OpCode::Label {
                    while result.len() <= instruction.arg as _ {
                        // Extend the labels to the needed label index and
                        // insert improbable results, so if there will be a bug,
                        // where labels are not defined, it will crash loudly.
                        result.push(u64::MAX);
                    }
                    result[instruction.arg as usize] = index as _;
                }
            }
            InstructionOrLabelReference::LabelReference(_) => {}
        }
    }
    result
}
