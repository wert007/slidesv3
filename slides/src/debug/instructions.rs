use std::{convert::TryFrom, path::{Path, PathBuf}};

use crate::{binder::typing::SystemCallKind, evaluator::memory::WORD_SIZE_IN_BYTES, instruction_converter::{InstructionOrLabelReference, LabelReference, instruction::{Instruction, op_codes::OpCode}}, text::SourceText};

pub fn print_instructions_with_source_code(start_index: usize, instructions: &[Instruction], source: &SourceText) {
    let output = instructions_with_source_code_to_string(start_index, instructions, source);
    println!("{}", output)
}

pub fn print_instructions_or_labels_with_source_code(start_index: usize, instructions: &[InstructionOrLabelReference], source: &SourceText) {
    let output = instructions_or_labels_with_source_code_to_string(start_index, instructions, source);
    println!("{}", output)
}

pub fn output_instructions_with_source_code_to_sldasm(start_index: usize, instructions: &[Instruction], source: &SourceText) {
    let contents = instructions_with_source_code_to_string(start_index, instructions, source);
    let output_path = PathBuf::from("../debug-out").join(Path::new(source.file_name).with_extension("sldasm").file_name().unwrap());
    std::fs::write(output_path, contents).unwrap();
}

pub fn output_instructions_with_source_code_to_sldasm_skip(skip_start: usize, skip_end: usize, instructions: &[Instruction], source: &SourceText) {
    let mut contents = instructions_with_source_code_to_string(0, &instructions[..skip_start], source);
    contents += "> Foreign instructions start\n";
    contents += &instructions_to_string(skip_start, &instructions[skip_start..skip_end]);
    contents += "> Foreign instructions end\n";
    contents += &instructions_with_source_code_to_string(skip_end, &instructions[skip_end..], source);
    let output_path = PathBuf::from("../debug-out").join(Path::new(source.file_name).with_extension("sldasm").file_name().unwrap());
    std::fs::write(output_path, contents).unwrap();
}

pub fn output_instructions_or_labels_with_source_code_to_sldasm(start_index: usize, instructions: &[InstructionOrLabelReference], source: &SourceText) {
    let contents = instructions_or_labels_with_source_code_to_string(start_index, instructions, source);
    let output_path = PathBuf::from("../debug-out").join(Path::new(source.file_name).with_extension("sldasm").file_name().unwrap());
    std::fs::write(output_path, contents).unwrap();
}

fn instructions_with_source_code_to_string(start_index: usize, instructions: &[Instruction], source: &SourceText) -> String {
    let mut current_span = None;
    let mut result = String::new();
    let mut index = start_index;
    for instruction in instructions {
        if instruction.span != current_span {
            current_span = instruction.span;
            if let Some(span) = current_span {
                result.push_str("> { ");
                result.push_str(&source.text[span.start()..span.end()]);
                result.push_str(" }\n");
            }
        }
        result.push_str("  ");
        result.push_str(&format!("{:3X}: ", index));
        result.push_str(&instruction_to_string(*instruction));
        result.push('\n');
        index += 1;
    }
    result
}

fn instructions_or_labels_with_source_code_to_string(start_index: usize, instructions: &[InstructionOrLabelReference], source: &SourceText) -> String {
    let mut current_span = None;
    let mut result = String::new();
    let mut index = start_index;
    for instruction in instructions {
        if instruction.span() != current_span {
            current_span = instruction.span();
            if let Some(span) = current_span {
                result.push_str("> { ");
                result.push_str(&source.text[span.start()..span.end()]);
                result.push_str(" }\n");
            }
        }
        match instruction {
            InstructionOrLabelReference::Instruction(instruction) => {
                result.push_str("  ");
                result.push_str(&format!("{:3X}: ", index));
                result.push_str(&instruction_to_string(*instruction));
                index += 1;
            }
            InstructionOrLabelReference::LabelReference(label) => {
                result.push_str(" ");
                result.push_str(&label_to_string(*label));
            }
        }
        result.push('\n');
    }
    result
}

fn instructions_to_string(start_index: usize, instructions: &[Instruction]) -> String {
    let mut result = String::new();
    let mut index = start_index;
    for instruction in instructions {
        result.push_str("  ");
        result.push_str(&format!("{:3X}: ", index));
        result.push_str(&instruction_to_string(*instruction));
        index += 1;
        result.push('\n');
    }
    result
}

fn label_to_string(label: LabelReference) -> String {
    format!("L{:X}:", label.label_reference)
}

pub fn instruction_to_string(instruction: Instruction) -> String {
    match instruction.op_code {
        OpCode::NoOp => instruction_no_arg_to_string("noop"),
        OpCode::LoadImmediate => instruction_dec_signed_arg_to_string("ldimm", instruction.arg),
        OpCode::LoadPointer => instruction_ptr_unsigned_arg_to_string("ldptr", instruction.arg),
        OpCode::Duplicate => instruction_no_arg_to_string("dup"),
        OpCode::Pop => instruction_no_arg_to_string("pop"),
        OpCode::LoadRegister => instruction_reg_arg_to_string("ldreg", instruction.arg),
        OpCode::StoreInRegister => instruction_reg_arg_to_string("streg", instruction.arg),
        OpCode::ArrayIndex => instruction_no_arg_to_string("arrayidx"),
        OpCode::StoreInMemory => instruction_ptr_unsigned_arg_to_string("stm", instruction.arg),
        OpCode::WriteToStack => instruction_ptr_unsigned_arg_to_string("wrtstck", instruction.arg),
        OpCode::WriteToHeap => instruction_word_count_arg_to_string("wrtheap", instruction.arg),
        OpCode::ReadWordWithOffset => instruction_byte_count_arg_to_string("rdwrdoffset", instruction.arg),
        OpCode::MemoryCopy => instruction_no_arg_to_string("memcpy"),
        OpCode::TypeIdentifier => instruction_dec_signed_arg_to_string("ldtyp", instruction.arg),
        OpCode::Label => instruction_label_arg_to_string("lbl", instruction.arg),
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
        OpCode::NoneableEquals => instruction_no_arg_to_string("eqn"),
        OpCode::TypeIdentifierEquals => instruction_no_arg_to_string("eqt"),
        OpCode::LessThan => instruction_no_arg_to_string("lt"),
        OpCode::GreaterThan => instruction_no_arg_to_string("gt"),
        OpCode::LessThanEquals => instruction_no_arg_to_string("lte"),
        OpCode::GreaterThanEquals => instruction_no_arg_to_string("gte"),
        OpCode::StringConcat => instruction_no_arg_to_string("strconcat"),
        OpCode::PointerAddition => instruction_no_arg_to_string("addptr"),
        OpCode::NoneableOrValue => instruction_no_arg_to_string("noneableor"),
        OpCode::Jump => instruction_ptr_unsigned_arg_to_string("jmp", instruction.arg),
        OpCode::JumpIfFalse => instruction_ptr_unsigned_arg_to_string("jmpfalse", instruction.arg),
        OpCode::JumpIfTrue => instruction_ptr_unsigned_arg_to_string("jmptrue", instruction.arg),
        OpCode::SysCall => instruction_syscall_arg_to_string("syscall", instruction.arg),
        OpCode::FunctionCall => instruction_no_arg_to_string("fncall"),
        OpCode::Return => instruction_return_arg_to_string("ret", instruction.arg),
        OpCode::DecodeClosure => instruction_decode_closure_arg_to_string("decclosure", instruction.arg),
        OpCode::CheckArrayBounds => instruction_no_arg_to_string("chkarraybounds"),
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
    format!("{} r#{}", name, arg)
}

fn instruction_label_arg_to_string(name: &str, arg: u64) -> String {
    format!("{} l{}", name, arg)
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
        format!("{} argument count {} with function pointer", name, argument_count)
    } else {
        format!("{} argument count {}", name, argument_count)
    }
}