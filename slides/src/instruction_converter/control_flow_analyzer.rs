use std::collections::{HashMap, VecDeque};

use crate::{
    binder::{
        control_flow_analyzer::{BasicBlock, BasicBlockKind, BasicCodeBlock, OutgoingConnections},
        typing::{FunctionType, SystemCallKind, TypeId},
    },
    debug::basic_blocks::BasicBlockConditionDisplay,
};

use super::{instruction::op_codes::OpCode, InstructionOrLabelReference};

struct Commented<T> {
    value: T,
    comment: String,
}

impl<T: BasicBlockConditionDisplay> BasicBlockConditionDisplay for Commented<T> {
    fn display(&self, types: &crate::binder::typing::TypeCollection, buffer: &mut String) {
        self.value.display(types, buffer);
        buffer.pop();
        buffer.push_str(&self.comment);
        buffer.push('\n');
    }
}

pub(crate) fn check_stack_usage(
    instructions: &[InstructionOrLabelReference],
    function_type: &FunctionType<TypeId>,
) {
    let mut blocks = collect_basic_blocks(instructions);
    connect_basic_blocks(&mut blocks, instructions);
    let stack_value = function_type.parameter_types.len() as i32
        + if function_type.this_type.is_some() {
            1
        } else {
            0
        };
    let mut checked_blocks: HashMap<usize, i32> = HashMap::with_capacity(blocks.len());
    let mut gray_blocks: VecDeque<_> = vec![(0, stack_value)].into();
    let mut commented_instructions: Vec<_> = instructions
        .iter()
        .map(|i| Commented {
            value: *i,
            comment: String::new(),
        })
        .collect();
    let mut emit_block = false;
    while let Some((block_index, mut stack_value)) = gray_blocks.pop_front() {
        checked_blocks.insert(block_index, stack_value);
        blocks[block_index].comment = Some(stack_value);
        let current_block = &blocks[block_index];
        match current_block.kind {
            BasicBlockKind::Start => {}
            BasicBlockKind::End => {
                if stack_value != 0 {
                    eprintln!("Corrupted Stack in {}", function_type.name());
                    crate::debug::basic_blocks::output_basic_instruction_blocks_to_dot(
                        function_type.name().as_str(),
                        &blocks,
                        &commented_instructions,
                    );
                    panic!();
                }
            }
            BasicBlockKind::CodeBlock(block) => {
                for (offset, instruction) in instructions[block.index..][..block.length]
                    .iter()
                    .enumerate()
                {
                    stack_value += if let InstructionOrLabelReference::Instruction(instruction) =
                        instruction
                    {
                        match instruction.op_code {
                            OpCode::NoOp => 0,
                            OpCode::LoadImmediate => 1,
                            OpCode::LoadPointer => 1,
                            OpCode::DuplicateOver => 1,
                            OpCode::Pop => -1,
                            OpCode::LoadRegister => 1,
                            OpCode::StoreInRegister => -1,
                            OpCode::StoreInMemory => -3,
                            OpCode::WriteToStack => 0,
                            OpCode::WriteToHeap => {
                                if instruction.arg == 0 {
                                    eprintln!("WARNING: Cannot determine stack usage for this, since the stack usage is not constant here!");
                                    emit_block = true;
                                    0
                                } else {
                                    1 - (instruction.arg as i32)
                                }
                            }
                            OpCode::Allocate => 1,
                            OpCode::ReadWordWithOffset => 0,
                            OpCode::MemoryCopy => todo!(),
                            OpCode::TypeIdentifier => 1,
                            OpCode::Label => 0,
                            OpCode::Rotate => 0,
                            OpCode::BitwiseTwosComplement => 0,
                            OpCode::BitwiseXor => -1,
                            OpCode::BitwiseNxor => -1,
                            OpCode::Addition => -1,
                            OpCode::Subtraction => -1,
                            OpCode::Multiplication => -1,
                            OpCode::Division => -1,
                            OpCode::Equals => -1,
                            OpCode::NotEquals => -1,
                            OpCode::ArrayEquals => -1,
                            OpCode::ArrayNotEquals => -1,
                            OpCode::NoneableEquals => -1,
                            OpCode::TypeIdentifierEquals => 0,
                            OpCode::LessThan => -1,
                            OpCode::GreaterThan => -1,
                            OpCode::LessThanEquals => -1,
                            OpCode::GreaterThanEquals => -1,
                            OpCode::StringConcat => -1,
                            OpCode::NoneableOrValue => -1,
                            OpCode::Jump => 0,
                            OpCode::JumpIfFalse => -1,
                            OpCode::JumpIfTrue => -1,
                            OpCode::SysCall => {
                                let system_call =
                                    SystemCallKind::try_from(instruction.arg as u8).unwrap();
                                let system_call = FunctionType::system_call(system_call);
                                let return_value = if system_call.return_type == typeid!(Type::Void)
                                {
                                    0
                                } else {
                                    1
                                };
                                let this_type = if system_call.this_type.is_some() {
                                    1
                                } else {
                                    0
                                };
                                // The -1 is for the argument count which is
                                // also on the stack
                                return_value
                                    - (system_call.parameter_types.len() as i32)
                                    - 1
                                    - this_type
                            }
                            OpCode::FunctionCall => {
                                let argument_count = instruction.arg >> 1;
                                let returns_value = instruction.arg & 0x1 == 0x1;
                                let returns_value = if returns_value { 1 } else { 0 };
                                // the -2 is for the argument count which is
                                // currently also always on the stack and the
                                // function pointer which will also be on the
                                // stack.
                                returns_value - 2 - (argument_count as i32)
                            }
                            OpCode::Return => {
                                if instruction.arg & 0x1 == 0x1 {
                                    -1
                                } else {
                                    0
                                }
                            }
                            OpCode::DecodeClosure => {
                                let argument_count = instruction.arg >> 1;
                                let has_function_pointer = instruction.arg & 0x1 == 0x1;
                                let has_function_pointer = if has_function_pointer { 1 } else { 0 };
                                argument_count as i32 + has_function_pointer
                            }
                            OpCode::Breakpoint => 0,
                            OpCode::Unknown => 0,
                        }
                    } else {
                        1
                    };
                    commented_instructions[offset + block.index].comment =
                        format!(" ; STACK COUNT = {stack_value}");
                }
            }
        }
        let mut append = current_block
            .outgoing_connections
            .to_vec()
            .into_iter()
            .zip(Some(stack_value).into_iter().cycle())
            .filter(|(b, s)| {
                if gray_blocks.iter().any(|(b2, _)| b == b2) || checked_blocks.get(b) != None {
                    false
                } else if gray_blocks.iter().any(|(b2, s2)| b == b2 && s != s2)
                    || (checked_blocks.get(b) != None && checked_blocks.get(b) != Some(s))
                {
                    eprintln!("Corrupted Stack in {}", function_type.name());
                    crate::debug::basic_blocks::output_basic_instruction_blocks_to_dot(
                        function_type.name().as_str(),
                        &blocks,
                        &commented_instructions,
                    );
                    panic!();
                } else {
                    true
                }
            })
            .collect();
        gray_blocks.append(&mut append);
    }
    if emit_block {
        crate::debug::basic_blocks::output_basic_instruction_blocks_to_dot(
            function_type.name().as_str(),
            &blocks,
            &commented_instructions,
        );
    }
}

fn collect_basic_blocks(instructions: &[InstructionOrLabelReference]) -> Vec<BasicBlock<(), i32>> {
    let mut result = vec![BasicBlock::start()];
    let mut current_block = BasicCodeBlock {
        index: 0,
        length: 0,
    };
    let mut label_indices = vec![];
    for (index, instruction) in instructions.iter().enumerate() {
        current_block.length += 1;
        match instruction {
            InstructionOrLabelReference::Instruction(instruction) => {
                match instruction.op_code {
                    super::instruction::op_codes::OpCode::Label => {
                        current_block.length -= 1;
                        // Start new Block
                        if current_block.length > 0 {
                            result.push(BasicBlock::code_block(
                                result.len(),
                                current_block,
                                label_indices,
                            ));
                            label_indices = vec![];
                        }
                        label_indices.push(instruction.arg as usize);
                        current_block = BasicCodeBlock {
                            index: index + 1,
                            length: 0,
                        };
                    }
                    super::instruction::op_codes::OpCode::Return
                    | super::instruction::op_codes::OpCode::Jump => {
                        // Start new Block
                        if current_block.length > 0 {
                            result.push(BasicBlock::code_block(
                                result.len(),
                                current_block,
                                label_indices,
                            ));
                            label_indices = vec![];
                        }
                        current_block = BasicCodeBlock {
                            index: index + 1,
                            length: 0,
                        };
                    }
                    super::instruction::op_codes::OpCode::JumpIfFalse
                    | super::instruction::op_codes::OpCode::JumpIfTrue => {
                        // Start new Block
                        if current_block.length > 0 {
                            result.push(BasicBlock::code_block(
                                result.len(),
                                current_block,
                                label_indices,
                            ));
                            label_indices = vec![];
                        }
                        current_block = BasicCodeBlock {
                            index: index + 1,
                            length: 0,
                        };
                    }
                    _ => {}
                }
            }
            InstructionOrLabelReference::LabelReference(_) => {}
        }
    }

    // End old Block
    if current_block.length > 0 {
        result.push(BasicBlock::code_block(
            result.len(),
            current_block,
            label_indices,
        ));
        label_indices = vec![];
    }
    result.push(BasicBlock::end(result.len(), label_indices));
    result
}

fn connect_basic_blocks(
    basic_blocks: &mut Vec<BasicBlock<(), i32>>,
    instructions: &[InstructionOrLabelReference],
) {
    let end_index = basic_blocks.last().unwrap().index;
    let mut incoming_connections = vec![];
    let label_index_to_basic_block_index: HashMap<usize, usize> = basic_blocks
        .iter()
        .enumerate()
        .flat_map(|(i, b)| b.label_indices.iter().map(move |v| (*v, i)))
        .collect();
    for basic_block in basic_blocks.iter_mut() {
        match basic_block.kind {
            BasicBlockKind::Start => {
                basic_block.outgoing_connections =
                    OutgoingConnections::Single(basic_block.index + 1);
                incoming_connections.push((basic_block.index, basic_block.index + 1));
            }
            BasicBlockKind::End => {}
            BasicBlockKind::CodeBlock(code_block) => {
                let instruction = instructions[code_block.index + code_block.length - 1];
                if let InstructionOrLabelReference::Instruction(instruction) = instruction {
                    match &instruction.op_code {
                        OpCode::Jump => {
                            let then_address =
                                label_index_to_basic_block_index[&(instruction.arg as usize)];
                            incoming_connections.push((basic_block.index, then_address));
                            basic_block.outgoing_connections =
                                OutgoingConnections::Single(then_address)
                        }
                        OpCode::JumpIfTrue => {
                            let then_address =
                                label_index_to_basic_block_index[&(instruction.arg as usize)];
                            incoming_connections.push((basic_block.index, then_address));
                            incoming_connections.push((basic_block.index, basic_block.index + 1));
                            basic_block.outgoing_connections = OutgoingConnections::IfTrue(
                                (),
                                then_address,
                                basic_block.index + 1,
                            );
                        }
                        OpCode::JumpIfFalse => {
                            let then_address =
                                label_index_to_basic_block_index[&(instruction.arg as usize)];
                            incoming_connections.push((basic_block.index, then_address));
                            incoming_connections.push((basic_block.index, basic_block.index + 1));
                            basic_block.outgoing_connections = OutgoingConnections::IfFalse(
                                (),
                                then_address,
                                basic_block.index + 1,
                            )
                        }
                        OpCode::Return => {
                            basic_block.outgoing_connections =
                                OutgoingConnections::Single(end_index);
                            incoming_connections.push((basic_block.index, end_index));
                        }
                        _ => {
                            basic_block.outgoing_connections =
                                OutgoingConnections::Single(basic_block.index + 1);
                            incoming_connections.push((basic_block.index, basic_block.index + 1));
                        }
                    }
                }
            }
        }
    }

    for (from, to) in incoming_connections {
        basic_blocks[to]
            .incoming_connections
            .add_connection_from(from);
    }
}
