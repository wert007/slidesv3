use crate::{binder::typing::Type, evaluator::is_heap_pointer};

use super::{EvaluatorState, TypedU64};


pub fn print(type_: Type, argument: TypedU64, state: &mut EvaluatorState) {
    println!("{}", to_string_native(type_, argument, state));
}

pub fn to_string(type_: Type, argument: TypedU64, state: &mut EvaluatorState) {
    let string = to_string_native(type_, argument, state);
    let string_length = string.len() as u64;
    let mut pointer = state.heap.allocate(4 + string_length);
    let result = pointer;
    state.heap.write_word(pointer as _, string_length);
    pointer += 4;
    for &byte in string.as_bytes() {
        state.heap.write_byte(pointer as _, byte);
        pointer += 1;
    }
    state.set_pointer(state.stack.len());
    state.stack.push(result);
}

fn to_string_native(type_: Type, argument: TypedU64, state: &mut EvaluatorState) -> String {
    match type_ {
        Type::Error => todo!(),
        Type::Void => todo!(),
        Type::Any => todo!(),
        Type::Integer => {
            format!("{}", argument.value as i64)
        }
        Type::Boolean => {
            if argument.value == 0 {
                "false".into()
            } else {
                "true".into()
            }
        }
        Type::SystemCall(kind) => format!("system call {}", kind),
        Type::Array(base_type) => {
            array_to_string_native(*base_type, argument, state)
        }
        Type::String => {
            string_to_string_native(argument, state)
        }
    }
}

fn array_to_string_native(base_type: Type, argument: TypedU64, state: &mut EvaluatorState) -> String {
    assert!(argument.is_pointer, "argument = {:#?}", argument);
    let array_length_in_bytes = if is_heap_pointer(argument.value) {
        state.heap.read_word(argument.value as _)
    } else {
        state.stack[argument.value as usize]
    };
    let array_length_in_words = (array_length_in_bytes + 3) / 4;
    let array_start = argument.value;
    let array_end = array_start - array_length_in_words;
    let mut result : String = "[ ".into();
    match base_type {
        Type::Error => todo!(),
        Type::Void => todo!(),
        Type::Any => todo!(),
        Type::Integer => {
            for i in (array_end..array_start).rev() {
                let value = if is_heap_pointer(i) {
                    state.heap.read_word(i as _)
                } else {
                    state.stack[i as usize]
                };
                result.push_str(&format!("{}, ", value as i64));
            }
        }
        Type::Boolean => {
            for i in (array_end..array_start).rev() {
                let value = if is_heap_pointer(i) {
                    state.heap.read_word(i as _)
                } else {
                    state.stack[i as usize]
                };
                if value == 0 {
                    result.push_str("false, ");
                } else {
                    result.push_str("true, ");
                }
            }
        }
        Type::SystemCall(kind) => {
            for _ in (array_end..array_start).rev() {
                result.push_str(&format!("system call {}, ", kind));
            }
        }
        Type::Array(base_type) => {
            for pointer_index in (array_end..array_start).rev() {
                let pointer = if is_heap_pointer(pointer_index) {
                    state.heap.read_word(pointer_index as _)
                } else {
                    state.stack[pointer_index as usize]
                };
                let argument = TypedU64 {
                    value: pointer,
                    is_pointer: true,
                };
                result.push_str(&array_to_string_native(*base_type.clone(), argument, state));
                result.push_str(", ");
            }
        }
        Type::String => {
            for pointer_index in (array_end..array_start).rev() {
                let pointer = if is_heap_pointer(pointer_index) {
                    state.heap.read_word(pointer_index as _)
                } else {
                    state.stack[pointer_index as usize]
                };
                let argument = TypedU64 {
                    value: pointer,
                    is_pointer: true,
                };
                result.push('\'');
                result.push_str(&string_to_string_native(argument, state));
                result.push_str("', ");
            }
        }
    }
    result.push_str("]");
    result
}

fn string_to_string_native(argument: TypedU64, state: &mut EvaluatorState) -> String {
    let mut is_heap_string;
    let mut string_start = argument.value;
    let mut pointer = if is_heap_pointer(string_start) {
        is_heap_string = true;
        state.heap.read_word(string_start as _)
    } else {
        is_heap_string = false;
        state.stack[string_start as usize]
    };

    while state.is_pointer(string_start as _) && !is_heap_string {
        string_start = pointer;
        pointer = if is_heap_pointer(pointer) {
            is_heap_string = true;
            state.heap.read_word(pointer as _)
        } else {
            is_heap_string = false;
            state.stack[pointer as usize]
        };
    }
    let string_length_in_bytes = pointer;
    let string_length_in_words = (string_length_in_bytes + 3) / 4;
    let string_end = string_start - string_length_in_words;
    let mut string_buffer: Vec<u8> = Vec::with_capacity(string_length_in_bytes as usize);

    let range = if is_heap_string {
        (string_start + 4..string_start + 4 + string_length_in_words * 4).step_by(4)
    } else {
        (string_end..string_start).step_by(1)
    };

    for i in range {
        let word = if is_heap_string {
            state.heap.read_word(i as usize)
        } else {
            state.stack[(string_end + string_start - i - 1) as usize]
        };
        string_buffer.push((word & 0xFF) as u8);
        string_buffer.push((word >> 8 & 0xFF) as u8);
        string_buffer.push((word >> 16 & 0xFF) as u8);
        string_buffer.push((word >> 24 & 0xFF) as u8);
    }
    String::from_utf8_lossy(&string_buffer).into_owned()
}

pub fn array_length(type_: Type, argument: TypedU64, state: &mut EvaluatorState) {
    let mut is_heap_array;
    let mut array_start = argument.value;
    let mut pointer = if is_heap_pointer(array_start) {
        is_heap_array = true;
        state.heap.read_word(array_start as _)
    } else {
        is_heap_array = false;
        state.stack[array_start as usize]
    };

    while state.is_pointer(array_start as _) && !is_heap_array {
        array_start = pointer;
        pointer = if is_heap_pointer(pointer) {
            is_heap_array = true;
            state.heap.read_word(pointer as _)
        } else {
            is_heap_array = false;
            state.stack[pointer as usize]
        };
    }

    let array_length = match type_ {
        Type::Error => todo!(),
        Type::Void => todo!(),
        Type::Any => todo!(),
        Type::Integer |
        Type::Boolean |
        Type::SystemCall(_) |
        Type::Array(_) => pointer / 4,
        Type::String => pointer,
    };

    state.stack.push(array_length);
}