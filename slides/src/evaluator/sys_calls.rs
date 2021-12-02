use crate::{
    binder::typing::{FunctionType, Type},
    evaluator::memory::bytes_to_word,
};

use super::{memory::FlaggedWord, EvaluatorState, WORD_SIZE_IN_BYTES};

pub fn print(argument: FlaggedWord, state: &mut EvaluatorState) {
    println!("{}", to_string_native(Type::Any, 0, argument, state));
}

pub fn to_string(argument: FlaggedWord, state: &mut EvaluatorState) {
    let string = to_string_native(Type::Any, 0, argument, state);
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

fn decode_type(address: FlaggedWord, state: &mut EvaluatorState) -> (Type, u64, u64) {
    let address = address.unwrap_pointer();
    let _type_identifier_size_in_bytes = state.read_pointer(address).unwrap_value();
    let address = address + WORD_SIZE_IN_BYTES;
    let type_identifier_kind = state.read_pointer(address).unwrap_value();
    let address = address + WORD_SIZE_IN_BYTES;
    if let Some(type_) = Type::simple_type_from_type_identifier(type_identifier_kind) {
        return (type_, address, 0);
    }
    match type_identifier_kind {
        Type::TYPE_IDENTIFIER_ARRAY => {
            let array_count = state.read_pointer(address).unwrap_value();
            let address = address + WORD_SIZE_IN_BYTES;
            let (mut base_type, address, to_string_function) =
                decode_type(FlaggedWord::pointer(address), state);
            for _ in 0..=array_count {
                base_type = Type::array(base_type);
            }
            (base_type, address, to_string_function)
        }
        Type::TYPE_IDENTIFIER_NONEABLE => {
            let (base_type, address, to_string_function) =
                decode_type(FlaggedWord::pointer(address), state);
            (Type::noneable(base_type), address, to_string_function)
        }
        Type::TYPE_IDENTIFIER_STRUCT | Type::TYPE_IDENTIFIER_STRUCT_REFERENCE => {
            let struct_id = state.read_pointer(address).unwrap_value();
            let address = address + WORD_SIZE_IN_BYTES;
            let to_string_function = state.read_pointer(address).unwrap_pointer();
            let address = address + WORD_SIZE_IN_BYTES;
            (
                Type::StructReference(struct_id),
                address,
                to_string_function,
            )
        }
        Type::TYPE_IDENTIFIER_FUNCTION | Type::TYPE_IDENTIFIER_CLOSURE => {
            let type_count = state.read_pointer(address).unwrap_value();
            let address = address + WORD_SIZE_IN_BYTES;
            let (this_type, address, _to_string_function) =
                decode_type(FlaggedWord::pointer(address), state);
            let (return_type, mut address, _to_string_function) =
                decode_type(FlaggedWord::pointer(address), state);
            let mut parameter_types = Vec::with_capacity(type_count as usize - 2);
            for _ in 2..type_count {
                let (parameter_type, new_address, _to_string_function) =
                    decode_type(FlaggedWord::pointer(address), state);
                address = new_address;
                parameter_types.push(parameter_type);
            }
            let this_type = if matches!(this_type, Type::Void) {
                None
            } else {
                Some(this_type)
            };
            (
                Type::function(FunctionType::function(
                    parameter_types,
                    this_type,
                    return_type,
                )),
                address,
                0,
            )
        }
        error => unreachable!("Found Type Identifier Kind {}", error),
    }
}

fn to_string_native(
    type_: Type,
    to_string_function: usize,
    argument: FlaggedWord,
    state: &mut EvaluatorState,
) -> String {
    match type_ {
        Type::Library(_) => unreachable!(),
        Type::Error => todo!(),
        Type::Void => todo!(),
        Type::Any => {
            let (type_, address, to_string_function) = decode_type(argument, state);
            let argument = state.read_pointer(address);
            to_string_native(type_, to_string_function as _, argument, state)
        }
        Type::None => "none".into(),
        Type::Noneable(base_type) => {
            if argument.unwrap_pointer() == 0 {
                "none".into()
            } else if base_type.is_pointer() {
                to_string_native(*base_type, to_string_function, argument, state)
            } else {
                let argument = state.read_pointer(argument.unwrap_pointer());
                to_string_native(*base_type, to_string_function, argument, state)
            }
        }
        Type::Struct(struct_type) => {
            if to_string_function != 0 {
                // FIXME: If there is an Err(()) returned, this means, there was
                // a runtime error, this should be handled the same way as in
                // evaluate.
                let return_value =
                    super::execute_function(state, to_string_function as usize, &[argument])
                        .unwrap()
                        .unwrap();
                to_string_native(Type::String, 0, return_value, state)
            } else {
                format!("type struct id#{}", struct_type.id)
            }
        }
        Type::StructReference(id) => {
            if to_string_function != 0 {
                // FIXME: If there is an Err(()) returned, this means, there was
                // a runtime error, this should be handled the same way as in
                // evaluate.
                let return_value =
                    super::execute_function(state, to_string_function as usize, &[argument])
                        .unwrap()
                        .unwrap();
                to_string_native(Type::String, 0, return_value, state)
            } else {
                format!("type struct id#{}", id)
            }
        }
        Type::Integer => {
            format!("{}", argument.unwrap_value() as i64)
        }
        Type::Pointer => {
            format!("0x{:x}", argument.unwrap_pointer())
        }
        Type::PointerOf(base_type) => {
            let ptr = argument.unwrap_pointer();
            if ptr == 0 {
                "none".into()
            } else {
                let argument = if base_type.is_pointer() {
                    FlaggedWord::pointer(ptr)
                } else {
                    state.read_pointer(ptr)
                };
                to_string_native(*base_type, to_string_function, argument, state)
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
        Type::Function(kind) => format!("fn {}", kind),
        Type::Closure(closure) => format!("fn {}", closure.base_function_type),
        Type::Array(base_type) => {
            array_to_string_native(*base_type, to_string_function, argument, state)
        }
        Type::String => string_to_string_native(argument, state),
    }
}

fn array_to_string_native(
    base_type: Type,
    to_string_function: usize,
    argument: FlaggedWord,
    state: &mut EvaluatorState,
) -> String {
    let argument = argument.unwrap_pointer();
    let array_length_in_bytes = state.read_pointer(argument).unwrap_value();
    let array_length_in_words = bytes_to_word(array_length_in_bytes);
    let array_start = argument + WORD_SIZE_IN_BYTES;
    let array_end = array_start + array_length_in_words * WORD_SIZE_IN_BYTES;
    let mut result: String = "[ ".into();
    for address in (array_start..array_end).step_by(WORD_SIZE_IN_BYTES as _) {
        let value = state.read_pointer(address);
        result.push_str(&to_string_native(
            base_type.clone(),
            to_string_function,
            value,
            state,
        ));
        result.push_str(", ");
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

pub fn array_length(argument: FlaggedWord, state: &mut EvaluatorState) {
    let (type_, address, _) = decode_type(argument, state);
    let argument = state.read_pointer(address);
    let array_start = argument.unwrap_pointer();
    let pointer = state.read_pointer(array_start).unwrap_value();

    let array_length = pointer / type_.array_element_size_in_bytes();

    state.stack.push(array_length);
}

pub fn heap_dump(argument: FlaggedWord, state: &mut EvaluatorState) {
    let argument = string_to_string_native(argument, state);
    let argument = argument.replace('\0', "");
    crate::debug::output_allocator_to_dot(&argument, &state.heap);
}
