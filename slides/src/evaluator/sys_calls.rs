use crate::{
    binder::typing::Type,
    evaluator::memory::bytes_to_word,
};

use super::{EvaluatorState, WORD_SIZE_IN_BYTES, memory::FlaggedWord};

pub fn print(type_: Type, argument: FlaggedWord, state: &mut EvaluatorState) {
    println!("{}", to_string_native(type_, argument, state));
}

pub fn to_string(type_: Type, argument: FlaggedWord, state: &mut EvaluatorState) {
    let string = to_string_native(type_, argument, state);
    let string_length = string.len() as u64;
    let mut pointer = state.heap.allocate(WORD_SIZE_IN_BYTES + string_length);
    let result = pointer;
    state.heap.write_word(pointer, string_length);
    pointer += WORD_SIZE_IN_BYTES;
    for &byte in string.as_bytes() {
        state.heap.write_byte(pointer, byte);
        pointer += 1;
    }
    state.stack.push_pointer(result);
}

fn to_string_native(type_: Type, argument: FlaggedWord, state: &mut EvaluatorState) -> String {
    match type_ {
        Type::Error => todo!(),
        Type::Void => todo!(),
        Type::Any => todo!(),
        Type::None => "none".into(),
        Type::Noneable(base_type) => {
            if argument.unwrap_value() == 0 {
                "none".into()
            } else {
                let argument = state.read_pointer(argument.unwrap_value());
                to_string_native(*base_type, argument, state)
            }
        },
        Type::Struct(_) | Type::StructReference(_) => todo!(),
        Type::Integer => {
            format!("{}", argument.unwrap_value() as i64)
        }
        Type::Boolean => {
            if argument.unwrap_value() == 0 {
                "false".into()
            } else {
                "true".into()
            }
        }
        Type::SystemCall(kind) => format!("system call {}", kind),
        Type::Function(kind) => format!("fn {}", kind),
        Type::Closure(closure) => format!("fn {}", closure.base_function_type),
        Type::Array(base_type) => array_to_string_native(*base_type, argument, state),
        Type::String => string_to_string_native(argument, state),
    }
}

fn array_to_string_native(
    base_type: Type,
    argument: FlaggedWord,
    state: &mut EvaluatorState,
) -> String {
    let argument = argument.unwrap_pointer();
    let array_length_in_bytes = state.read_pointer(argument).unwrap_value();
    let array_length_in_words = bytes_to_word(array_length_in_bytes);
    let array_start = argument + WORD_SIZE_IN_BYTES;
    let array_end = array_start + array_length_in_words * WORD_SIZE_IN_BYTES;
    let mut result: String = "[ ".into();
    match base_type {
        Type::Error => todo!(),
        Type::Void => todo!(),
        Type::Any => todo!(),
        Type::None | Type::Noneable(_) => todo!(),
        Type::Struct(_) | Type::StructReference(_) => todo!(),
        Type::Integer => {
            for i in (array_start..array_end).step_by(WORD_SIZE_IN_BYTES as _) {
                let value = state.read_pointer(i).unwrap_value();
                result.push_str(&format!("{}, ", value as i64));
            }
        }
        Type::Boolean => {
            for i in (array_start..array_end).step_by(WORD_SIZE_IN_BYTES as _) {
                let value = state.read_pointer(i).unwrap_value();
                if value == 0 {
                    result.push_str("false, ");
                } else {
                    result.push_str("true, ");
                }
            }
        }
        Type::SystemCall(kind) => {
            for _ in (array_start..array_end).step_by(WORD_SIZE_IN_BYTES as _) {
                result.push_str(&format!("system call {}, ", kind));
            }
        }
        Type::Function(kind) => {
            for _ in (array_start..array_end).step_by(WORD_SIZE_IN_BYTES as _) {
                result.push_str(&format!("fn {}, ", kind));
            }
        }
        Type::Closure(closure) => {
            for _ in (array_start..array_end).step_by(WORD_SIZE_IN_BYTES as _) {
                result.push_str(&format!("fn {}, ", closure.base_function_type));
            }
        }
        Type::Array(base_type) => {
            for pointer_index in (array_start..array_end).step_by(WORD_SIZE_IN_BYTES as _) {
                let argument = state.read_pointer(pointer_index);
                result.push_str(&array_to_string_native(*base_type.clone(), argument, state));
                result.push_str(", ");
            }
        }
        Type::String => {
            for pointer_index in (array_start..array_end).step_by(WORD_SIZE_IN_BYTES as _) {
                let argument = state.read_pointer(pointer_index);
                result.push('\'');
                result.push_str(&string_to_string_native(argument, state));
                result.push_str("', ");
            }
        }
    }
    result.push(']');
    result
}

fn string_to_string_native(argument: FlaggedWord, state: &mut EvaluatorState) -> String {
    let string_start = argument.unwrap_pointer();
    let pointer = state.read_pointer(string_start).unwrap_value();

    let string_length_in_bytes = pointer;
    let string_length_in_words = bytes_to_word(string_length_in_bytes);
    let mut string_buffer: Vec<u8> = Vec::with_capacity(string_length_in_bytes as _);

    let range = (string_start + WORD_SIZE_IN_BYTES
        ..string_start + WORD_SIZE_IN_BYTES + string_length_in_words * WORD_SIZE_IN_BYTES)
        .step_by(WORD_SIZE_IN_BYTES as _);

    for i in range {
        let word = state.read_pointer(i).unwrap_value();
        let bytes = word.to_be_bytes();
        string_buffer.extend_from_slice(&bytes);
    }
    String::from_utf8_lossy(&string_buffer).into_owned()
}

pub fn array_length(type_: Type, argument: FlaggedWord, state: &mut EvaluatorState) {
    let array_start = argument.unwrap_pointer();
    let pointer = state.read_pointer(array_start).unwrap_value();

    let array_length = pointer / type_.array_element_size_in_bytes();

    state.stack.push(array_length);
}
