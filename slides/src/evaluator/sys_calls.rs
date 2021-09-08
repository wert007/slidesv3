use crate::{binder::typing::Type, evaluator::is_heap_pointer};

use super::{EvaluatorState, TypedU64};

pub fn print(type_: Type, argument: TypedU64, state: &mut EvaluatorState) {
    print!("PRINT ");
    match type_ {
        Type::Error => todo!(),
        Type::Void => todo!(),
        Type::Any => todo!(),
        Type::Integer => {
            println!("{}", argument.value as i64)
        }
        Type::Boolean => {
            if argument.value == 0 {
                println!("false");
            } else {
                println!("true");
            }
        }
        Type::SystemCall(kind) => println!("system call {}", kind),
        Type::Array(base_type) => {
            print_array(*base_type, argument, state);
            println!();
        }
        Type::String => {
            print_string(argument, state);
            println!();
        }
    }
}

fn print_array(base_type: Type, argument: TypedU64, state: &mut EvaluatorState) {
    assert!(argument.is_pointer, "argument = {:#?}", argument);
    let array_length_in_bytes = if is_heap_pointer(argument.value) {
        state.heap.read_word(argument.value as _)
    } else {
        state.stack[argument.value as usize]
    };
    let array_length_in_words = (array_length_in_bytes + 3) / 4;
    let array_start = argument.value;
    let array_end = array_start - array_length_in_words;
    print!("[ ");
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
                print!("{}, ", value as i64);
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
                    print!("false, ");
                } else {
                    print!("true, ");
                }
            }
        }
        Type::SystemCall(kind) => {
            for _ in (array_end..array_start).rev() {
                print!("system call {}, ", kind);
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
                print_array(*base_type.clone(), argument, state);
                print!(", ");
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
                print_string(argument, state);
                print!(", ");
            }
        }
    }
    print!("]");
}

fn print_string(argument: TypedU64, state: &mut EvaluatorState) {
    let is_heap_string = is_heap_pointer(argument.value);
    let string_length_in_bytes = if is_heap_string {
        state.heap.read_word(argument.value as _)
    } else {
        state.stack[argument.value as usize]
    };
    let string_length_in_words = (string_length_in_bytes + 3) / 4;
    let string_start = argument.value; // Includes the length of the string
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
    print!("'{}'", String::from_utf8_lossy(&string_buffer));
}
