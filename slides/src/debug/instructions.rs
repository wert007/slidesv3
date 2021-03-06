use std::{
    convert::TryFrom,
    path::{Path, PathBuf},
};

use crate::{
    binder::typing::SystemCallKind,
    evaluator::memory::{static_memory::StaticMemory, WORD_SIZE_IN_BYTES},
    instruction_converter::{
        instruction::{op_codes::OpCode, Instruction},
        InstructionOrLabelReference, LabelReference,
    },
    text::SourceText,
};

#[derive(Default, Clone, Copy)]
pub struct Context<'a> {
    reg_names: &'a [Option<String>],
    static_memory: Option<&'a StaticMemory>,
}

impl<'a> Context<'a> {
    pub fn with_static_memory(self, static_memory: &'a StaticMemory) -> Self {
        Self {
            static_memory: Some(static_memory),
            ..self
        }
    }

    pub fn with_register_names(self, reg_names: &'a [Option<String>]) -> Self {
        Self { reg_names, ..self }
    }
}

pub fn print_instructions_with_source_code(
    instructions: &[Instruction],
    source: &SourceText,
    context: Context,
) {
    let output = instructions_with_source_code_to_string(instructions, source, context);
    println!("{}", output)
}

pub fn output_instructions_with_source_code_to_sldasm(
    instructions: &[Instruction],
    source: &SourceText,
    context: Context,
) {
    let output = instructions_with_source_code_to_string(instructions, source, context);
    let output_path = PathBuf::from("../debug-out").join(
        Path::new(source.file_name)
            .with_extension("sldasm")
            .file_name()
            .unwrap(),
    );
    std::fs::write(output_path, output).unwrap();
}

pub fn print_instructions_or_labels_with_source_code(
    instructions: &[InstructionOrLabelReference],
    source: &SourceText,
    context: Context,
) {
    let output = instructions_or_labels_with_source_code_to_string(instructions, source, context);
    println!("{}", output)
}

pub fn output_instructions_or_labels_with_source_code_to_sldasm(
    instructions: &[InstructionOrLabelReference],
    source: &SourceText,
    context: Context,
) {
    let mut is_foreign = false;
    let mut current_span = None;
    let mut buffer = String::new();
    for (index, instruction) in instructions.iter().enumerate() {
        let new_is_foreign = instruction.span().map(|s| s.is_foreign()).unwrap_or(true);
        if new_is_foreign != is_foreign {
            buffer.push_str("> Foreign instructions ");
            if new_is_foreign {
                buffer.push_str("start\n");
            } else {
                buffer.push_str("end\n");
            }
            is_foreign = new_is_foreign;
        }
        if !new_is_foreign && instruction.span() != current_span {
            current_span = instruction.span();
            buffer.push_str("> { ");
            buffer
                .push_str(&source.text[current_span.unwrap().start()..current_span.unwrap().end()]);
            buffer.push_str(" }\n");
        }
        buffer.push_str("  ");
        match instruction {
            InstructionOrLabelReference::Instruction(instruction) => {
                if instruction.op_code == OpCode::Label {
                    buffer.push_str(&format!("L{:02X}: {:3X}", instruction.arg, index));
                } else {
                    buffer.push_str(&format!("{:3X}: ", index));
                    buffer.push_str(&instruction_or_label_to_string(*instruction, true, context));
                }
            }
            InstructionOrLabelReference::LabelReference(label) => {
                buffer.push_str(&format!("{:3X}: ", index));
                buffer.push_str(&label_reference_to_string(*label));
            }
        }
        buffer.push('\n');
    }
    let output_path = PathBuf::from("../debug-out").join(
        Path::new(source.file_name)
            .with_extension("sldasm")
            .file_name()
            .unwrap(),
    );
    std::fs::write(output_path, buffer).unwrap();
}

fn instructions_with_source_code_to_string(
    instructions: &[Instruction],
    source: &SourceText,
    context: Context,
) -> String {
    let mut is_foreign = false;
    let mut current_span = None;
    let mut buffer = String::new();
    for (index, instruction) in instructions.iter().enumerate() {
        let new_is_foreign = instruction.span.map(|s| s.is_foreign()).unwrap_or(true);
        if new_is_foreign != is_foreign {
            buffer.push_str("> Foreign instructions ");
            if new_is_foreign {
                buffer.push_str("start\n");
            } else {
                buffer.push_str("end\n");
            }
            is_foreign = new_is_foreign;
        }
        if !new_is_foreign && instruction.span != current_span {
            current_span = instruction.span;
            buffer.push_str("> { ");
            buffer
                .push_str(&source.text[current_span.unwrap().start()..current_span.unwrap().end()]);
            buffer.push_str(" }\n");
        }
        buffer.push_str("  ");
        buffer.push_str(&format!("{:3X}: ", index));
        buffer.push_str(&instruction_to_string(*instruction, context));
        buffer.push('\n');
    }
    buffer
}

fn instructions_or_labels_with_source_code_to_string(
    instructions: &[InstructionOrLabelReference],
    source: &SourceText,
    context: Context,
) -> String {
    let mut is_foreign = false;
    let mut current_span = None;
    let mut buffer = String::new();
    for (index, instruction) in instructions.iter().enumerate() {
        let new_is_foreign = instruction.span().map(|s| s.is_foreign()).unwrap_or(true);
        if new_is_foreign != is_foreign {
            buffer.push_str("> Foreign instructions ");
            if new_is_foreign {
                buffer.push_str("start\n");
            } else {
                buffer.push_str("end\n");
            }
            is_foreign = new_is_foreign;
        }
        if !new_is_foreign && instruction.span() != current_span {
            current_span = instruction.span();
            buffer.push_str("> { ");
            buffer
                .push_str(&source.text[current_span.unwrap().start()..current_span.unwrap().end()]);
            buffer.push_str(" }\n");
        }
        buffer.push_str("  ");
        match instruction {
            InstructionOrLabelReference::Instruction(instruction) => {
                if instruction.op_code == OpCode::Label {
                    buffer.push_str(&format!("L{:02X}: {:3X}", instruction.arg, index));
                } else {
                    buffer.push_str(&format!("{:3X}: ", index));
                    buffer.push_str(&instruction_or_label_to_string(*instruction, true, context));
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
    context: Context,
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
                    result.push_str(&instruction_or_label_to_string(*instruction, true, context));
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
    context: Context,
) -> String {
    let mut result = String::new();
    let mut index = start_index;
    for instruction in instructions {
        result.push_str("  ");
        result.push_str(&format!("{:3X}: ", index));
        result.push_str(&instruction_to_string(*instruction, context));
        index += 1;
        result.push('\n');
    }
    result
}

fn label_reference_to_string(label: LabelReference) -> String {
    format!("ldlbl L{:02X}", label.label_reference)
}

pub fn instruction_to_string(instruction: Instruction, context: Context) -> String {
    instruction_or_label_to_string(instruction, false, context)
}

fn instruction_or_label_to_string(
    instruction: Instruction,
    has_labels: bool,
    context: Context,
) -> String {
    match instruction.op_code {
        OpCode::NoOp => instruction_no_arg_to_string("noop"),
        OpCode::LoadImmediate => instruction_dec_signed_arg_to_string("ldimm", instruction.arg),
        OpCode::LoadPointer => {
            instruction_ptr_unsigned_arg_to_string_with_context("ldptr", instruction.arg, context)
        }
        OpCode::DuplicateOver => instruction_word_count_arg_to_string("dupover", instruction.arg),
        OpCode::Pop => instruction_no_arg_to_string("pop"),
        OpCode::LoadRegister => {
            instruction_reg_arg_name_to_string("ldreg", instruction.arg, context)
        }
        OpCode::StoreInRegister => {
            instruction_reg_arg_name_to_string("streg", instruction.arg, context)
        }
        OpCode::StoreInMemory => instruction_ptr_unsigned_arg_to_string("stm", instruction.arg),
        OpCode::WriteToStack => instruction_ptr_unsigned_arg_to_string("wrtstck", instruction.arg),
        OpCode::WriteToHeap => instruction_word_count_arg_to_string("wrtheap", instruction.arg),
        OpCode::Allocate => instruction_byte_count_arg_to_string("alloc", instruction.arg),
        OpCode::ReadWordWithOffset => {
            instruction_byte_count_arg_to_string("rdwrdoffset", instruction.arg)
        }
        OpCode::ReadByteWithOffset => {
            instruction_byte_count_arg_to_string("rdbyteoffset", instruction.arg)
        }
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

fn instruction_ptr_unsigned_arg_to_string_with_context(
    name: &str,
    arg: u64,
    context: Context,
) -> String {
    let string = context
        .static_memory
        .map(|it| it.try_read_string_from(arg))
        .flatten();
    match string {
        Some(it) => {
            format!("{} 0x{:x} > '{}'", name, arg, it)
        }
        None => {
            format!("{} 0x{:x}", name, arg)
        }
    }
}

fn instruction_reg_arg_name_to_string(name: &str, arg: u64, context: Context) -> String {
    if let Some(reg_name) = context
        .reg_names
        .get(arg as usize)
        .map(|i| i.as_ref())
        .flatten()
    {
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
