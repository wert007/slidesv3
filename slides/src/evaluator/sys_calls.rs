use crate::{
    binder::typing::{self, Type, TypeId},
    evaluator::memory::{bytes_to_word, Memory},
    text::TextLocation,
};

use super::{memory::FlaggedWord, EvaluatorState, InstructionError, WORD_SIZE_IN_BYTES};

pub fn print(
    argument: &FlaggedWord,
    location: TextLocation,
    state: &mut EvaluatorState,
) -> Result<(), InstructionError> {
    println!(
        "{}",
        to_string_native(typeid!(Type::Any), argument, location, state)?
    );
    Ok(())
}

fn native_string_to_string(
    value: String,
    location: TextLocation,
    state: &mut EvaluatorState,
) -> FlaggedWord {
    let string_length = value.len() as u64;
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
        return FlaggedWord::pointer(result, location, Some(state.pc));
    }
    // TODO: Clear bucket maybe?
    state.heap.write_flagged_word(
        pointer,
        FlaggedWord::value(string_length, location, Some(state.pc))
            .with_comment(format!("String length of {value:?}")),
    );
    pointer += WORD_SIZE_IN_BYTES;
    for &byte in value.as_bytes() {
        state.heap.write_byte(pointer, byte, location, state.pc);
        pointer += 1;
    }
    FlaggedWord::pointer(result, location, Some(state.pc))
}

pub fn to_string(
    argument: &FlaggedWord,
    location: TextLocation,
    state: &mut EvaluatorState,
) -> Result<(), InstructionError> {
    let string = to_string_native(typeid!(Type::Any), argument, location, state)?;
    let value = native_string_to_string(string, location, state);
    state.stack.push_flagged_word(value);
    Ok(())
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
    let type_identifier = state.read_pointer_word(address).unwrap_value();
    // SAFETY: This is actually only as safe as the implementation. If something
    // gets read as TypeId, but it actually isn't, strange things will happen.
    let type_identifier = unsafe { TypeId::from_raw(type_identifier) };
    type_identifier
}

fn to_string_native(
    type_: TypeId,
    argument: &FlaggedWord,
    location: TextLocation,
    state: &mut EvaluatorState,
) -> Result<String, InstructionError> {
    Ok(match &state.project.types[type_] {
        Type::Library(_)
        | Type::GenericType(_, _)
        | Type::IntegerLiteral
        | Type::StructPlaceholder(..)
        | Type::IgnoreTypeChecking => unreachable!("{:#?}", &state.project.types[type_]),
        Type::Error => todo!(),
        Type::Void => todo!(),
        Type::TypeId => {
            let type_id = argument.unwrap_value();
            state
                .project
                .types
                .name_of_type_id(unsafe { TypeId::from_raw(type_id) })
                .into_owned()
        }
        Type::Any => {
            let type_ = decode_type(argument, state);
            let address = argument.unwrap_pointer() + WORD_SIZE_IN_BYTES;
            let argument = state.read_pointer_word(address).into_owned();
            to_string_native(type_, &argument, location, state)?
        }
        Type::None => "none".into(),
        Type::AbstractTypeBox(type_) => to_string_native(*type_, argument, location, state)?,
        Type::Struct(struct_type) => {
            let to_string_function = get_to_string_function(type_, state);
            match to_string_function {
                Some(it) => {
                    let return_value =
                        super::execute_function(state, it as usize, &[argument.clone()]).map_err(
                            |i| i.unwrap_or(InstructionError::unknown(line!(), location, state.pc)),
                        )?;
                    to_string_native(
                        typeid!(Type::String),
                        &return_value.unwrap(),
                        location,
                        state,
                    )?
                }
                None => {
                    use std::fmt::Write;
                    let mut result = format!("struct {} {{\n", struct_type.name);
                    let mut parents = vec![struct_type];
                    while let Some(parent) = parents.last().unwrap().parent {
                        let parent = state.project.types[parent].as_struct_type().unwrap();
                        parents.push(parent);
                    }
                    let fields: Vec<_> =
                        parents.into_iter().flat_map(|p| p.fields.clone()).collect();
                    for field in fields {
                        let value = state
                            .read_pointer_word(
                                argument.as_pointer().map_err(|err| {
                                    dbg!(field.offset_or_address.unwrap_offset());
                                    InstructionError::value(err, line!(), location, state.pc)
                                })? + field.offset_or_address.unwrap_offset() as u64,
                            )
                            .into_owned();
                        let value = to_string_native(field.type_, &value, location, state)?;
                        let value = if field.type_ == typeid!(Type::String) {
                            format!("\'{value}\'")
                        } else {
                            value
                        };
                        let mut value = value.lines();
                        write!(
                            result,
                            "    {} = {}",
                            field.name,
                            value.next().unwrap_or_default()
                        )
                        .unwrap();
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
        Type::Float => {
            let value = f64::from_bits(argument.unwrap_value());
            if value.fract() == 0.0 {
                format!("{value:.1}")
            } else {
                format!("{value:}")
            }
        }
        Type::Pointer => {
            format!("0x{:x}", argument.unwrap_pointer())
        }
        Type::PointerOf(base_type) => {
            let ptr = argument.unwrap_pointer();
            if ptr == 0 {
                "none".into()
            } else {
                let argument = if state.project.types[*base_type].is_pointer() {
                    FlaggedWord::pointer(ptr, location, Some(state.pc))
                } else {
                    state.read_pointer_word(ptr).into_owned()
                };
                to_string_native(*base_type, &argument, location, state)?
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
        Type::String => string_to_string_native(argument, location, state)?,
        Type::Enum(a, b) => {
            let index = argument.unwrap_value() as usize;
            format!("{a}.{}", b[index])
        }
    })
}

fn string_to_string_native(
    argument: &FlaggedWord,
    location: TextLocation,
    state: &mut EvaluatorState,
) -> Result<String, InstructionError> {
    let string_start = argument
        .as_pointer()
        .map_err(|err| InstructionError::value(err, line!(), location, state.pc))?;
    let pointer = state
        .read_pointer_word(string_start)
        .as_value()
        .map_err(|err| InstructionError::pointer(err, line!(), location, state.pc))?;

    let string_length_in_bytes = pointer;
    let string_length_in_words = bytes_to_word(string_length_in_bytes);
    // FIXME: Use string_length_in_words instead.
    let mut string_buffer: Vec<u8> = Vec::with_capacity(string_length_in_bytes as _);

    let range = (string_start + WORD_SIZE_IN_BYTES
        ..string_start + WORD_SIZE_IN_BYTES + string_length_in_words * WORD_SIZE_IN_BYTES)
        .step_by(WORD_SIZE_IN_BYTES as _);

    for i in range {
        let word = state
            .read_pointer_word(i)
            .as_value()
            .map_err(|err| InstructionError::pointer(err, line!(), location, state.pc))?;
        let bytes = word.to_le_bytes();
        string_buffer.extend_from_slice(&bytes);
    }
    Ok(String::from_utf8_lossy(&string_buffer[..string_length_in_bytes as usize]).into_owned())
}

pub fn array_length(argument: &FlaggedWord, location: TextLocation, state: &mut EvaluatorState) {
    let type_ = decode_type(argument, state);
    let argument = state.read_pointer_word(argument.unwrap_pointer() + WORD_SIZE_IN_BYTES);
    let array_start = argument.unwrap_pointer();
    let pointer = state.read_pointer_word(array_start).unwrap_value();

    let array_length = pointer / state.project.types[type_].array_element_size_in_bytes();

    state.stack.push(array_length, location, Some(state.pc));
}

pub fn heap_dump(
    argument: &FlaggedWord,
    location: TextLocation,
    state: &mut EvaluatorState,
) -> Result<(), InstructionError> {
    let argument = string_to_string_native(argument, location, state)?;
    let argument = argument.replace('\0', "");
    crate::debug::output_allocator_to_dot(&argument, &state.heap);
    Ok(())
}

pub fn reallocate(
    pointer: &FlaggedWord,
    size: &FlaggedWord,
    location: TextLocation,
    state: &mut EvaluatorState,
) {
    let pointer = pointer.unwrap_pointer();
    let size = size.unwrap_value();
    let result = state.reallocate(pointer, size);
    state
        .stack
        .push_flagged_word(FlaggedWord::pointer(result, location, Some(state.pc)));
}

pub fn runtime_error(
    argument: &FlaggedWord,
    location: TextLocation,
    state: &mut EvaluatorState,
) -> Result<(), InstructionError> {
    let argument = string_to_string_native(argument, location, state)?;
    let argument = argument.replace('\0', "");
    println!(
        "Runtime error happened {}: {}",
        state.instructions[state.pc]
            .location
            .display(&state.project.source_text_collection),
        argument
    );
    for location in &state.stack_trace {
        println!(
            "  ...{}",
            location.display(&state.project.source_text_collection)
        );
    }
    state.runtime_error_happened = true;
    Ok(())
}

pub fn hash(argument: &FlaggedWord, location: TextLocation, state: &mut EvaluatorState) {
    let hash = hash_value(argument, typeid!(Type::Any), state);
    state.stack.push(hash, location, Some(state.pc));
}

fn hash_value(argument: &FlaggedWord, type_: TypeId, state: &mut EvaluatorState) -> u64 {
    match &state.project.types[type_] {
        Type::Error
        | Type::Void
        | Type::IntegerLiteral
        | Type::Library(_)
        | Type::GenericType(_, _)
        | Type::StructPlaceholder(_)
        | Type::IgnoreTypeChecking => unreachable!(),
        Type::Any => {
            let type_ = decode_type(argument, state);
            let argument = state
                .read_pointer_word(argument.unwrap_pointer() + WORD_SIZE_IN_BYTES)
                .into_owned();
            hash_value(&argument, type_, state)
        }
        Type::Integer(_)
        | Type::Boolean
        | Type::None
        | Type::Enum(_, _)
        | Type::SystemCall(_)
        | Type::TypeId => argument.unwrap_value(),
        Type::String => {
            let ptr = argument.unwrap_pointer();
            let length = state.read_pointer_word(ptr).unwrap_value();
            hash_array(length, ptr + WORD_SIZE_IN_BYTES, state)
        }
        Type::Function(_) => todo!(),
        Type::Closure(_) => todo!(),
        Type::Float => todo!(),
        Type::AbstractTypeBox(type_) => hash_value(argument, *type_, state),
        Type::Struct(s) => hash_array(s.size_in_bytes, argument.unwrap_pointer(), state),
        Type::Pointer => argument.unwrap_value(),
        Type::PointerOf(type_) => {
            let argument = state
                .read_pointer_word(argument.unwrap_pointer())
                .into_owned();
            hash_value(&argument, *type_, state)
        }
    }
}

fn hash_array(length: u64, pointer: u64, state: &mut EvaluatorState) -> u64 {
    let mut hash = 5381;
    for i in 0..length / WORD_SIZE_IN_BYTES {
        hash = ((hash << 5) + hash)
            + state
                .read_pointer_word(pointer + i * WORD_SIZE_IN_BYTES)
                .value;
    }
    let too_many_bits = length % WORD_SIZE_IN_BYTES;
    hash = ((hash << 5) + hash)
        + (state
            .read_pointer_word(
                pointer + bytes_to_word(length) * WORD_SIZE_IN_BYTES - WORD_SIZE_IN_BYTES,
            )
            .value
            & ((1 << too_many_bits) - 1));
    hash
}

pub fn byte_to_char(argument: &FlaggedWord, location: TextLocation, state: &mut EvaluatorState) {
    let byte = argument.unwrap_value() as u8;
    let result = (byte as char).to_string();
    let value = native_string_to_string(result, location, state);
    state.stack.push_flagged_word(value);
}

pub fn type_of_value(argument: &FlaggedWord, location: TextLocation, state: &mut EvaluatorState) {
    let type_ = decode_type(argument, state);
    state.stack.push(type_.as_raw(), location, Some(state.pc));
}

pub(crate) fn write_to_file(
    path: &FlaggedWord,
    content: &FlaggedWord,
    location: TextLocation,
    state: &mut EvaluatorState,
) -> Result<(), InstructionError> {
    let path = to_string_native(typeid!(Type::String), path, location, state)?;
    let content = to_string_native(typeid!(Type::String), content, location, state)?;
    match std::fs::write(&path, content) {
        Ok(_) => {}
        Err(err) => {
            state.runtime_diagnostics.write_to_file_failed(
                state.instructions[state.pc].location,
                &path,
                err,
            );
            state
                .runtime_diagnostics
                .clone()
                .flush_to_console(&state.project.source_text_collection);
            state.runtime_diagnostics.diagnostics.clear();
            state.runtime_error_happened = true;
        }
    }
    Ok(())
}
