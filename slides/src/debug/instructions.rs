use std::{
    convert::TryFrom,
    path::{Path, PathBuf},
};

use crate::{
    binder::typing::SystemCallKind,
    evaluator::{memory::{WORD_SIZE_IN_BYTES, self}, EvaluatorState},
    instruction_converter::{
        instruction::{op_codes::OpCode, Instruction},
        InstructionOrLabelReference, LabelReference,
    },
    text::SourceTextCollection,
};

pub fn print_instructions_with_source_code(
    instructions: &[Instruction],
    source_text_collection: &SourceTextCollection,
) {
    let output = instructions_with_source_code_to_string(instructions, source_text_collection);
    println!("{}", output)
}

pub fn output_instructions_with_source_code_to_sldasm(
    file_name: impl AsRef<Path>,
    instructions: &[Instruction],
    source_text_collection: &SourceTextCollection,
) {
    let output = instructions_with_source_code_to_string(instructions, source_text_collection);
    let output_path = PathBuf::from("../debug-out").join(
        file_name
            .as_ref()
            .with_extension("sldasm")
            .file_name()
            .unwrap(),
    );
    std::fs::write(output_path, output).unwrap();
}

pub fn print_instructions_or_labels_with_source_code(
    instructions: &[InstructionOrLabelReference],
    source_text_collection: &SourceTextCollection,
) {
    let output =
        instructions_or_labels_with_source_code_to_string(instructions, source_text_collection);
    println!("{}", output)
}

pub fn output_instructions_or_labels_with_source_code_to_sldasm(
    file_name: impl AsRef<Path>,
    instructions: &[InstructionOrLabelReference],
    source_text_collection: &SourceTextCollection,
) {
    let mut current_span = None;
    let mut buffer = String::new();
    for (index, instruction) in instructions.iter().enumerate() {
        // if new_is_foreign != is_foreign {
        //     buffer.push_str("> Foreign instructions ");
        //     if new_is_foreign {
        //         buffer.push_str("start\n");
        //     } else {
        //         buffer.push_str("end\n");
        //     }
        //     is_foreign = new_is_foreign;
        // }
        if Some(instruction.location()) != current_span {
            current_span = Some(instruction.location());
            buffer.push_str("> { ");
            buffer.push_str(&source_text_collection[current_span.unwrap()]);
            buffer.push_str(" }\n");
        }
        buffer.push_str("  ");
        match instruction {
            InstructionOrLabelReference::Instruction(instruction) => {
                if instruction.op_code == OpCode::Label {
                    buffer.push_str(&format!("L{:02X}: {:3X}", instruction.arg, index));
                } else {
                    buffer.push_str(&format!("{:3X}: ", index));
                    buffer.push_str(&instruction_to_string(*instruction, true, None));
                }
            }
            InstructionOrLabelReference::LabelReference(label) => {
                buffer.push_str(&format!("{:3X}: ", index));
                buffer.push_str(&label_reference_to_string(*label));
            }
        }
        buffer.push('\n');
    }
    let output_path = PathBuf::from("./debug-out").join(
        file_name
            .as_ref()
            .with_extension("sldasm")
            .file_name()
            .unwrap(),
    );
    std::fs::write(output_path, buffer).unwrap();
}

fn instructions_with_source_code_to_string(
    instructions: &[Instruction],
    source_text_collection: &SourceTextCollection,
) -> String {
    let mut current_span = None;
    let mut buffer = String::new();
    for (index, instruction) in instructions.iter().enumerate() {
        if Some(instruction.location) != current_span {
            current_span = Some(instruction.location);
            buffer.push_str("> { ");
            buffer.push_str(&source_text_collection[current_span.unwrap()]);
            buffer.push_str(" }\n");
        }
        buffer.push_str("  ");
        buffer.push_str(&format!("{:3X}: ", index));
        buffer.push_str(&instruction_to_string(*instruction, false, None));
        buffer.push('\n');
    }
    buffer
}

fn instructions_or_labels_with_source_code_to_string(
    instructions: &[InstructionOrLabelReference],
    source_text_collection: &SourceTextCollection,
) -> String {
    let mut current_span = None;
    let mut buffer = String::new();
    for (index, instruction) in instructions.iter().enumerate() {
        if Some(instruction.location()) != current_span {
            current_span = Some(instruction.location());
            buffer.push_str("> { ");
            buffer.push_str(&source_text_collection[current_span.unwrap()]);
            buffer.push_str(" }\n");
        }
        buffer.push_str("  ");
        match instruction {
            InstructionOrLabelReference::Instruction(instruction) => {
                if instruction.op_code == OpCode::Label {
                    buffer.push_str(&format!("L{:02X}: {:3X}", instruction.arg, index));
                } else {
                    buffer.push_str(&format!("{:3X}: ", index));
                    buffer.push_str(&instruction_to_string(*instruction, true, None));
                }
            }
            InstructionOrLabelReference::LabelReference(label) => {
                buffer.push_str(&format!("{:3X}: ", index));
                buffer.push_str(&label_reference_to_string(*label));
            }
        }
        buffer.push('\n');
    }
    buffer
}

fn instructions_or_labels_to_string(
    start_index: usize,
    instructions: &[InstructionOrLabelReference],
) -> String {
    let mut result = String::new();
    let mut index = start_index;
    for instruction in instructions {
        result.push_str("  ");
        match instruction {
            InstructionOrLabelReference::Instruction(instruction) => {
                if instruction.op_code == OpCode::Label {
                    result.push_str(&format!("L{:02X}: {:3X}", instruction.arg, index));
                } else {
                    result.push_str(&format!("{:3X}: ", index));
                    result.push_str(&instruction_to_string(*instruction, true, None));
                }
            }
            InstructionOrLabelReference::LabelReference(label) => {
                result.push_str(&format!("{:3X}: ", index));
                result.push_str(&label_reference_to_string(*label));
            }
        }
        index += 1;
        result.push('\n');
    }
    result
}

fn instructions_to_string(
    start_index: usize,
    instructions: &[Instruction],
    reg_names: &[Option<&str>],
) -> String {
    let mut result = String::new();
    let mut index = start_index;
    for instruction in instructions {
        result.push_str("  ");
        result.push_str(&format!("{:3X}: ", index));
        let reg_name = if (reg_names.len() as u64) < instruction.arg {
            reg_names[instruction.arg as usize]
        } else {
            None
        };
        result.push_str(&instruction_to_string(*instruction, false, reg_name));
        index += 1;
        result.push('\n');
    }
    result
}

fn label_reference_to_string(label: LabelReference) -> String {
    format!("ldlbl L{:02X}", label.label_reference)
}

// pub fn instruction_to_string(instruction: Instruction, reg_name: Option<&str>) -> String {
//     instruction_or_label_to_string(instruction, false, reg_name)
// }

pub fn instruction_or_label_to_string(instruction_or_label: InstructionOrLabelReference) -> String {
    match instruction_or_label {
        InstructionOrLabelReference::Instruction(instruction) => {
            instruction_to_string(instruction, true, None)
        }
        InstructionOrLabelReference::LabelReference(label) => label_reference_to_string(label),
    }
}

pub fn commented_instruction_to_string(instruction: Instruction,
state: &EvaluatorState) -> Result<String, std::fmt::Error> {
    use std::fmt::Write;
    let mut base = instruction_to_string(instruction, false, None);
    match instruction.op_code {
        OpCode::NoOp => {},
        OpCode::LoadImmediate => {
            if let Some(type_) = state.project.types.to_type_id(instruction.arg as usize) {
                write!(base, "; {}", state.project.types.name_of_type_id_debug(type_))?;
            }
        },
        OpCode::LoadPointer => {
            if let Some(text) = state.read_string_from_memory(instruction.arg as usize) {
                write!(base, "; {text:?}")?;
            } else if let Some(symbol) = state.read_symbol_from_memory(instruction.arg as usize) {
                write!(base, "; {symbol}")?;
            } else if instruction.arg == 0 {
                write!(base, "; NONE")?;
            }
        },
        OpCode::DuplicateOver => {},
        OpCode::Pop => {},
        OpCode::LoadRegister => {
            write!(base, "; {}", state.load_register(instruction.arg as usize).unwrap())?;
        }
        OpCode::StoreInRegister => {
            write!(base, "; {}", state.peek_stack(0).unwrap())?;
        },
        OpCode::StoreInMemory => {},
        OpCode::WriteToStack => {},
        OpCode::WriteToHeap => {
            let size = if instruction.arg == 0 {
                state.peek_stack(0).unwrap().unwrap_value()
            } else {
                instruction.arg
            };
            if let Some(address) = state.next_allocation_address(size * memory::WORD_SIZE_IN_BYTES) {
                write!(base, "; #{address:X}")?;
            }
        },
        OpCode::Allocate => {},
        OpCode::ReadWordWithOffset => {
            if let Some(address) = state.peek_stack(0).map(|v| v.as_pointer()).flatten() {
                let offset = instruction.arg;
                let address = address + offset;
                if let Some(ptr) = state.read_pointer_safe(address) {
                    write!(base, "; {}", ptr)?;
                }
            }
        
        },
        OpCode::MemoryCopy => {},
        OpCode::TypeIdentifier => todo!(),
        OpCode::Label => unreachable!("Right now, this only gets called by the evaluator, and there are no labels left!"),
        OpCode::Rotate => {},
        OpCode::Unknown => {},
        OpCode::BitwiseTwosComplement => {},
        OpCode::BitwiseXor => {},
        OpCode::BitwiseNxor => {},
        OpCode::Addition => {},
        OpCode::Subtraction => {},
        OpCode::Multiplication => {},
        OpCode::Division => {},
        OpCode::Equals => {},
        OpCode::NotEquals => {},
        OpCode::ArrayEquals => {},
        OpCode::ArrayNotEquals => {},
        OpCode::LessThan => {},
        OpCode::GreaterThan => {},
        OpCode::LessThanEquals => {},
        OpCode::GreaterThanEquals => {},
        OpCode::StringConcat => {},
        OpCode::NoneableOrValue => {},
        OpCode::Jump => {},
        OpCode::JumpIfFalse => {},
        OpCode::JumpIfTrue => {},
        OpCode::SysCall => {},
        OpCode::FunctionCall => {},
        OpCode::Return => {},
        OpCode::DecodeClosure => {},
        OpCode::Breakpoint => {},
    }
    Ok(base)
}

pub fn instruction_to_string(
    instruction: Instruction,
    has_labels: bool,
    reg_name: Option<&str>,
) -> String {
    match instruction.op_code {
        OpCode::NoOp => instruction_no_arg_to_string("noop"),
        OpCode::Unknown => instruction_no_arg_to_string("???"),
        OpCode::LoadImmediate => instruction_dec_signed_arg_to_string("ldimm", instruction.arg),
        OpCode::LoadPointer => instruction_ptr_unsigned_arg_to_string("ldptr", instruction.arg),
        OpCode::DuplicateOver => instruction_word_count_arg_to_string("dupover", instruction.arg),
        OpCode::Pop => instruction_no_arg_to_string("pop"),
        OpCode::LoadRegister => {
            instruction_reg_arg_name_to_string("ldreg", instruction.arg, reg_name)
        }
        OpCode::StoreInRegister => {
            instruction_reg_arg_name_to_string("streg", instruction.arg, reg_name)
        }
        OpCode::StoreInMemory => instruction_ptr_unsigned_arg_to_string("stm", instruction.arg),
        OpCode::WriteToStack => instruction_ptr_unsigned_arg_to_string("wrtstck", instruction.arg),
        OpCode::WriteToHeap => instruction_word_count_arg_to_string("wrtheap", instruction.arg),
        OpCode::Allocate => instruction_byte_count_arg_to_string("alloc", instruction.arg),
        OpCode::ReadWordWithOffset => {
            instruction_byte_count_arg_to_string("rdwrdoffset", instruction.arg)
        }
        OpCode::MemoryCopy => instruction_no_arg_to_string("memcpy"),
        OpCode::TypeIdentifier => instruction_dec_signed_arg_to_string("ldtyp", instruction.arg),
        OpCode::Label => instruction_label_arg_to_string("lbl", instruction.arg),
        OpCode::Rotate => instruction_word_count_arg_to_string("rotate", instruction.arg),
        OpCode::BitwiseTwosComplement => instruction_no_arg_to_string("btwoscomplement"),
        OpCode::BitwiseXor => instruction_no_arg_to_string("bxor"),
        OpCode::BitwiseNxor => instruction_no_arg_to_string("bnxor"),
        OpCode::Addition => instruction_no_arg_to_string("add"),
        OpCode::Subtraction => instruction_no_arg_to_string("sub"),
        OpCode::Multiplication => instruction_no_arg_to_string("mul"),
        OpCode::Division => instruction_no_arg_to_string("div"),
        OpCode::Equals => instruction_no_arg_to_string("eq"),
        OpCode::NotEquals => instruction_no_arg_to_string("neq"),
        OpCode::ArrayEquals => instruction_no_arg_to_string("eqa"),
        OpCode::ArrayNotEquals => instruction_no_arg_to_string("neqa"),
        OpCode::LessThan => instruction_no_arg_to_string("lt"),
        OpCode::GreaterThan => instruction_no_arg_to_string("gt"),
        OpCode::LessThanEquals => instruction_no_arg_to_string("lte"),
        OpCode::GreaterThanEquals => instruction_no_arg_to_string("gte"),
        OpCode::StringConcat => instruction_no_arg_to_string("strconcat"),
        OpCode::NoneableOrValue => {
            instruction_dereference_arg_to_string("noneableor", instruction.arg)
        }
        OpCode::Jump if !has_labels => {
            instruction_ptr_unsigned_arg_to_string("jmp", instruction.arg)
        }
        OpCode::JumpIfFalse if !has_labels => {
            instruction_ptr_unsigned_arg_to_string("jmpfalse", instruction.arg)
        }
        OpCode::JumpIfTrue if !has_labels => {
            instruction_ptr_unsigned_arg_to_string("jmptrue", instruction.arg)
        }
        OpCode::Jump => instruction_label_arg_to_string("jmp", instruction.arg),
        OpCode::JumpIfFalse => instruction_label_arg_to_string("jmpfalse", instruction.arg),
        OpCode::JumpIfTrue => instruction_label_arg_to_string("jmptrue", instruction.arg),
        OpCode::SysCall => instruction_syscall_arg_to_string("syscall", instruction.arg),
        OpCode::FunctionCall => instruction_no_arg_to_string("fncall"),
        OpCode::Return => instruction_return_arg_to_string("ret", instruction.arg),
        OpCode::DecodeClosure => {
            instruction_decode_closure_arg_to_string("decclosure", instruction.arg)
        }
        OpCode::Breakpoint => instruction_no_arg_to_string("brk"),
    }
}

fn instruction_dereference_arg_to_string(name: &str, dereference: u64) -> String {
    let dereference = dereference != 0;
    if dereference {
        format!("{} with derefence", name)
    } else {
        format!("{} no dereference", name)
    }
}

fn instruction_no_arg_to_string(name: &str) -> String {
    name.into()
}

fn instruction_dec_signed_arg_to_string(name: &str, arg: u64) -> String {
    let arg = arg as i64;
    format!("{} {}", name, arg)
}

fn instruction_ptr_unsigned_arg_to_string(name: &str, arg: u64) -> String {
    format!("{} 0x{:x}", name, arg)
}

fn instruction_reg_arg_to_string(name: &str, arg: u64) -> String {
    instruction_reg_arg_name_to_string(name, arg, None)
}

fn instruction_reg_arg_name_to_string(name: &str, arg: u64, reg_name: Option<&str>) -> String {
    if let Some(reg_name) = reg_name {
        format!("{} {}", name, reg_name)
    } else {
        format!("{} r#{}", name, arg)
    }
}

fn instruction_label_arg_to_string(name: &str, arg: u64) -> String {
    format!("{} L{:02X}", name, arg)
}

fn instruction_word_count_arg_to_string(name: &str, arg: u64) -> String {
    instruction_byte_count_arg_to_string(name, arg * WORD_SIZE_IN_BYTES)
}

fn instruction_byte_count_arg_to_string(name: &str, arg: u64) -> String {
    // FIXME: Maybe add a human readable version of the byte count (like KB, MB etc..)
    format!("{} {} bytes", name, arg)
}

fn instruction_syscall_arg_to_string(name: &str, arg: u64) -> String {
    let arg = SystemCallKind::try_from(arg as u8).unwrap();
    format!("{} {}", name, arg)
}

fn instruction_return_arg_to_string(name: &str, arg: u64) -> String {
    let mut postfix = String::new();
    if arg & 1 != 0 {
        postfix.push_str(" with value");
    }
    if arg & 2 != 0 {
        postfix.push_str(" restores registers")
    }
    format!("{}{}", name, postfix)
}

fn instruction_decode_closure_arg_to_string(name: &str, arg: u64) -> String {
    // arg: (argument_count << 1) + has_function_pointer as u64,
    let has_function_pointer = arg & 1 != 0;
    let argument_count = arg >> 1;
    if has_function_pointer {
        format!(
            "{} argument count {} with function pointer",
            name, argument_count
        )
    } else {
        format!("{} argument count {}", name, argument_count)
    }
}
