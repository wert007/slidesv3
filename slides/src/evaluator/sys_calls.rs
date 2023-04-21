use crate::{
    binder::typing::{self, Type, TypeId},
    evaluator::memory::{bytes_to_word, Memory},
};

use super::{memory::FlaggedWord, EvaluatorState, WORD_SIZE_IN_BYTES};

pub fn print(argument: &FlaggedWord, state: &mut EvaluatorState) {
    println!("{}", to_string_native(typeid!(Type::Any), argument, state));
}

pub fn to_string(argument: &FlaggedWord, state: &mut EvaluatorState) {
    let string = to_string_native(typeid!(Type::Any), argument, state);
    let string_length = string.len() as u64;
    let mut pointer = state.reallocate(0, WORD_SIZE_IN_BYTES + string_length);
    let result = pointer;
    if result == 0 {
        state.runtime_diagnostics.no_heap_memory_left(
            state.instructions[state.pc].location,
            WORD_SIZE_IN_BYTES + string_length,
        );
        state
            .runtime_diagnostics
            .clone()
            .flush_to_console(&state.project.source_text_collection);
        state.runtime_diagnostics.diagnostics.clear();
        state.runtime_error_happened = true;
        state.stack.push_pointer(result);
        return;
    }
    // TODO: Clear bucket maybe?
    state.heap.write_flagged_word(
        pointer,
        FlaggedWord::value(string_length).with_comment(format!("String length of {string:?}")),
    );
    pointer += WORD_SIZE_IN_BYTES;
    for &byte in string.as_bytes() {
        state.heap.write_byte(pointer, byte);
        pointer += 1;
    }
    state.stack.push_pointer(result);
}

fn get_to_string_function(type_id: TypeId, state: &EvaluatorState) -> Option<u64> {
    state.project.types[type_id]
        .as_struct_type()
        .map(|s| {
            s.function_table
                .to_string_function
                .as_ref()
                .map(|f| f.function_label)
        })
        .flatten()
}

fn decode_type(address: &FlaggedWord, state: &mut EvaluatorState) -> TypeId {
    let address = address.unwrap_pointer();
    let type_identifier = state.read_pointer(address).unwrap_value();
    // SAFETY: This is actually only as safe as the implementation. If something
    // gets read as TypeId, but it actually isn't, strange things will happen.
    let type_identifier = unsafe { TypeId::from_raw(type_identifier) };
    type_identifier
}

fn to_string_native(type_: TypeId, argument: &FlaggedWord, state: &mut EvaluatorState) -> String {
    match &state.project.types[type_] {
        Type::Library(_)
        | Type::GenericType
        | Type::IntegerLiteral
        | Type::StructPlaceholder(..) => unreachable!("{:#?}", &state.project.types[type_]),
        Type::Error => todo!(),
        Type::Void => todo!(),
        Type::Any => {
            let type_ = decode_type(argument, state);
            let address = argument.unwrap_pointer() + WORD_SIZE_IN_BYTES;
            let argument = state.read_pointer(address).clone();
            to_string_native(type_, &argument, state)
        }
        Type::None => "none".into(),
        Type::Noneable(base_type) => {
            if argument.unwrap_pointer() == 0 {
                "none".into()
            } else if state.project.types[*base_type].is_pointer() {
                to_string_native(*base_type, argument, state)
            } else {
                let argument = state.read_pointer(argument.unwrap_pointer()).clone();
                to_string_native(*base_type, &argument, state)
            }
        }
        Type::Struct(struct_type) => {
            let to_string_function = get_to_string_function(type_, state);
            match to_string_function {
                Some(it) => {
                    let return_value =
                        super::execute_function(state, it as usize, &[argument.clone()]);
                    match return_value {
                        Ok(it) => to_string_native(typeid!(Type::String), &it.unwrap(), state),
                        Err(()) => {
                            println!("To string function failed!");
                            String::new()
                        }
                    }
                }
                None => {
                    use std::fmt::Write;
                    let mut result = format!("struct {} {{\n", struct_type.name);
                    for field in struct_type.fields.clone() {
                        let value = state
                            .read_pointer(
                                argument.unwrap_pointer()
                                    + field.offset_or_address.unwrap_offset() as u64,
                            )
                            .clone();
                        let value = to_string_native(field.type_, &value, state);
                        let mut value = value.lines();
                        write!(result, "    {} = {}", field.name, value.next().unwrap()).unwrap();
                        for line in value {
                            writeln!(result).unwrap();
                            write!(result, "    {line}").unwrap();
                        }
                        writeln!(result, ",").unwrap();
                    }
                    result.push('}');
                    result
                }
            }
        }
        Type::Integer(integer_type) => match integer_type {
            typing::IntegerType::Signed64 => format!("{}", argument.unwrap_value() as i64),
            typing::IntegerType::Unsigned8 => format!("{}", argument.unwrap_value() as u8),
            typing::IntegerType::Unsigned64 => format!("{}", argument.unwrap_value() as u64),
        },
        Type::Pointer => {
            format!("0x{:x}", argument.unwrap_pointer())
        }
        Type::PointerOf(base_type) => {
            let ptr = argument.unwrap_pointer();
            if ptr == 0 {
                "none".into()
            } else {
                let argument = if state.project.types[*base_type].is_pointer() {
                    FlaggedWord::pointer(ptr)
                } else {
                    state.read_pointer(ptr).clone()
                };
                to_string_native(*base_type, &argument, state)
            }
        }
        Type::Boolean => {
            if argument.unwrap_value() == 0 {
                "false".into()
            } else {
                "true".into()
            }
        }
        Type::SystemCall(kind) => format!("system call {}", kind),
        Type::Function(kind) => format!("fn {}", kind.display(&state.project.types)),
        Type::Closure(closure) => format!("fn {}", closure.base_function_type),
        Type::String => string_to_string_native(argument, state),
        Type::Enum(a, b) => {
            let index = argument.unwrap_value() as usize;
            format!("{a}.{}", b[index])
        }
    }
}

fn string_to_string_native(argument: &FlaggedWord, state: &mut EvaluatorState) -> String {
    let string_start = argument.unwrap_pointer();
    let pointer = state.read_pointer(string_start).unwrap_value();

    let string_length_in_bytes = pointer;
    let string_length_in_words = bytes_to_word(string_length_in_bytes);
    // FIXME: Use string_length_in_words instead.
    let mut string_buffer: Vec<u8> = Vec::with_capacity(string_length_in_bytes as _);

    let range = (string_start + WORD_SIZE_IN_BYTES
        ..string_start + WORD_SIZE_IN_BYTES + string_length_in_words * WORD_SIZE_IN_BYTES)
        .step_by(WORD_SIZE_IN_BYTES as _);

    for i in range {
        let word = state.read_pointer(i).unwrap_value();
        let bytes = word.to_be_bytes();
        string_buffer.extend_from_slice(&bytes);
    }
    String::from_utf8_lossy(&string_buffer[..string_length_in_bytes as usize]).into_owned()
}

pub fn array_length(argument: &FlaggedWord, state: &mut EvaluatorState) {
    let type_ = decode_type(argument, state);
    let argument = state.read_pointer(argument.unwrap_pointer() + WORD_SIZE_IN_BYTES);
    let array_start = argument.unwrap_pointer();
    let pointer = state.read_pointer(array_start).unwrap_value();

    let array_length = pointer / state.project.types[type_].array_element_size_in_bytes();

    state.stack.push(array_length);
}

pub fn heap_dump(argument: &FlaggedWord, state: &mut EvaluatorState) {
    let argument = string_to_string_native(argument, state);
    let argument = argument.replace('\0', "");
    crate::debug::output_allocator_to_dot(&argument, &state.heap);
}

pub fn reallocate(pointer: &FlaggedWord, size: &FlaggedWord, state: &mut EvaluatorState) {
    let pointer = pointer.unwrap_pointer();
    let size = size.unwrap_value();
    let result = state.reallocate(pointer, size);
    state.stack.push_flagged_word(FlaggedWord::pointer(result));
}

pub fn runtime_error(argument: &FlaggedWord, state: &mut EvaluatorState) {
    let argument = string_to_string_native(argument, state);
    let argument = argument.replace('\0', "");
    println!("Runtime error happened: {}", argument);
    state.runtime_error_happened = true;
}

pub fn address_of(argument: &FlaggedWord, state: &mut EvaluatorState) {
    state.stack.push(argument.value);
}
