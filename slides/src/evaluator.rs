mod allocator;
pub mod stack;
mod sys_calls;

use crate::{
    binder::typing::{SystemCallKind, Type},
    instruction_converter::instruction::{op_codes::OpCode, Instruction},
    value::Value,
    DebugFlags,
};
use num_enum::TryFromPrimitive;

use self::{allocator::Allocator, stack::Stack};

type ResultType = Value;

const HEAP_POINTER: u64 = 0x80_00_00_00_00_00_00_00;
pub const WORD_SIZE_IN_BYTES: u64 = 8;

pub const fn bytes_to_word(bytes: u64) -> u64 {
    (bytes + WORD_SIZE_IN_BYTES - 1) / WORD_SIZE_IN_BYTES
}

pub struct EvaluatorState {
    stack: Stack,
    heap: Allocator,
    registers: Vec<TypedU64>,
    pc: usize,
}

impl EvaluatorState {
    fn set_variable(&mut self, variable: u64, value: u64, is_pointer: bool) {
        let variable = variable as usize;
        while self.registers.len() <= variable {
            self.registers.push(TypedU64 {
                value: 0,
                is_pointer: false,
            });
        }
        self.registers[variable] = TypedU64 { value, is_pointer };
    }

    fn is_pointer(&mut self, address: usize) -> bool {
        is_heap_pointer(address as _) || self.stack.is_pointer(address)
    }
}

pub fn evaluate(instructions: Vec<Instruction>, debug_flags: DebugFlags) -> ResultType {
    let mut state = EvaluatorState {
        stack: Stack::new(debug_flags),
        heap: Allocator::new(1024, debug_flags),
        registers: vec![],
        pc: 0,
    };
    while state.pc < instructions.len() {
        let pc = state.pc;
        if debug_flags.print_current_instruction() {
            println!("  CI {}: {:?}", pc, instructions[pc]);
        }
        execute_instruction(&mut state, instructions[pc]);
        state.pc += 1;
    }
    if state.stack.len() == 1 {
        (state.stack.pop().value as i64).into()
    } else {
        for (variable, &value) in state.registers.iter().enumerate() {
            if value.is_pointer {
                println!("{:00}: #{}", variable, value.value)
            } else {
                println!("{:00}: {}", variable, value.value as i64)
            }
        }
        Value::Integer(-1)
    }
}

fn execute_instruction(state: &mut EvaluatorState, instruction: Instruction) {
    match instruction.op_code {
        OpCode::Label => unreachable!(),
        OpCode::NoOp => {}
        OpCode::LoadImmediate => evaluate_load_immediate(state, instruction),
        OpCode::LoadPointer => evaluate_load_pointer(state, instruction),
        OpCode::Pop => evaluate_pop(state, instruction),
        OpCode::LoadRegister => evaluate_load_register(state, instruction),
        OpCode::StoreInRegister => evaluate_assign_to_variable(state, instruction),
        OpCode::CreateStackPointer => evaluate_create_stack_pointer(state, instruction),
        OpCode::ArrayIndex => evaluate_array_index(state, instruction),
        OpCode::StoreInMemory => evaluate_write_to_memory(state, instruction),
        OpCode::TypeIdentifier => evaluate_load_immediate(state, instruction),
        OpCode::BitwiseTwosComplement => evaluate_bitwise_twos_complement(state, instruction),
        OpCode::BitwiseXor => evaluate_bitwise_xor(state, instruction),
        OpCode::BitwiseNxor => evaluate_bitwise_nxor(state, instruction),
        OpCode::Addition => evaluate_addition(state, instruction),
        OpCode::Subtraction => evaluate_subtraction(state, instruction),
        OpCode::Multiplication => evaluate_multiplication(state, instruction),
        OpCode::Division => evaluate_division(state, instruction),
        OpCode::Equals => evaluate_equals(state, instruction),
        OpCode::NotEquals => evaluate_not_equals(state, instruction),
        OpCode::ArrayEquals => evaluate_array_equals(state, instruction),
        OpCode::ArrayNotEquals => evaluate_array_not_equals(state, instruction),
        OpCode::LessThan => evaluate_less_than(state, instruction),
        OpCode::GreaterThan => evaluate_greater_than(state, instruction),
        OpCode::LessThanEquals => evaluate_less_than_equals(state, instruction),
        OpCode::GreaterThanEquals => evaluate_greater_than_equals(state, instruction),
        OpCode::StringConcat => evaluate_string_concat(state, instruction),
        OpCode::JmpRelative => evaluate_jmp_relative(state, instruction),
        OpCode::JmpIfFalse => evaluate_jmp_if_false(state, instruction),
        OpCode::SysCall => evaluate_sys_call(state, instruction),
        OpCode::FunctionCall => evaluate_function_call(state, instruction),
        OpCode::Return => evaluate_return(state, instruction),
    }
}

fn evaluate_load_immediate(state: &mut EvaluatorState, instruction: Instruction) {
    state.stack.push(instruction.arg);
}

fn evaluate_load_pointer(state: &mut EvaluatorState, instruction: Instruction) {
    state.stack.push_pointer(instruction.arg);
}

fn is_heap_pointer(address: u64) -> bool {
    address & HEAP_POINTER > 0
}

fn evaluate_pop(state: &mut EvaluatorState, _: Instruction) {
    state.stack.pop();
}

fn evaluate_load_register(state: &mut EvaluatorState, instruction: Instruction) {
    let value = state.registers[instruction.arg as usize];
    if value.is_pointer {
        state.stack.push_pointer(value.value);
    } else {
        state.stack.push(value.value);
    }
}

fn evaluate_assign_to_variable(state: &mut EvaluatorState, instruction: Instruction) {
    let TypedU64 { value, is_pointer } = state.stack.pop();
    state.set_variable(instruction.arg, value, is_pointer);
}

fn evaluate_create_stack_pointer(state: &mut EvaluatorState, instruction: Instruction) {
    let stack_pointer = state.stack.len() as u64 - instruction.arg;
    state.stack.push_pointer(stack_pointer);
}

fn evaluate_array_index(state: &mut EvaluatorState, _: Instruction) {
    let index_in_words = state.stack.pop().value;
    let array = state.stack.pop();
    assert!(
        array.is_pointer,
        "array = {:#?}, pointers = {:?}, stack = {:?}",
        array, state.stack.flags, state.stack.data
    );
    let array = array.value;

    let array_length_in_bytes = if is_heap_pointer(array) {
        state.heap.read_word(array as _)
    } else {
        state.stack.read_word(array as _)
    };
    let array_length_in_words = bytes_to_word(array_length_in_bytes);
    if (index_in_words as i64) < 0 || index_in_words > array_length_in_words {
        println!(
            "RuntimeError: Index {} is out of Bounds ({}) for this array!",
            index_in_words as i64, array_length_in_words
        );
        state.stack.push(0);
        return;
    }
    let index_in_bytes = array as usize + index_in_words as usize * WORD_SIZE_IN_BYTES as usize + WORD_SIZE_IN_BYTES as usize;
    let value = if is_heap_pointer(array) {
        state.heap.read_word(index_in_bytes)
    } else {
        state.stack.read_word(index_in_bytes)
    };
    if state.is_pointer(index_in_bytes) {
        state.stack.push_pointer(value);
    } else {
        state.stack.push(value);
    }
}

fn evaluate_write_to_memory(state: &mut EvaluatorState, _: Instruction) {
    let index = state.stack.pop().value;
    let array = state.stack.pop();
    assert!(
        array.is_pointer,
        "array = {:#?}, pointers = {:?}, stack = {:?}",
        array, state.stack.flags, state.stack.data
    );
    let array = array.value;
    let value = state.stack.pop().value;
    let array_length_in_bytes = if is_heap_pointer(array) {
        state.heap.read_word(array as _)
    } else {
        state.stack.read_word(array as usize)
    };
    let array_length_in_words = bytes_to_word(array_length_in_bytes);
    if (index as i64) < 0 || index > array_length_in_words {
        println!(
            "RuntimeError: Index {} is out of Bounds ({}) for this array!",
            index as i64, array_length_in_words
        );
        return;
    }
    let index = array + index * WORD_SIZE_IN_BYTES + WORD_SIZE_IN_BYTES;
    let index = index as _;
    if is_heap_pointer(array) {
        state.heap.write_word(index, value);
    } else {
        state.stack.write_word(index, value);
    }
}

fn evaluate_bitwise_twos_complement(state: &mut EvaluatorState, _: Instruction) {
    let value = state.stack.pop().value as i64;
    state.stack.push((-value) as u64);
}

fn evaluate_bitwise_xor(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().value;
    let lhs = state.stack.pop().value;
    state.stack.push(lhs ^ rhs);
}

fn evaluate_bitwise_nxor(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().value;
    let lhs = state.stack.pop().value;
    state.stack.push(!(lhs ^ rhs));
}

fn evaluate_addition(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().value;
    let lhs = state.stack.pop().value;
    state.stack.push(lhs.wrapping_add(rhs));
}

fn evaluate_subtraction(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().value;
    let lhs = state.stack.pop().value;
    state.stack.push(lhs.wrapping_sub(rhs));
}

fn evaluate_multiplication(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().value;
    let lhs = state.stack.pop().value;
    state.stack.push(lhs.wrapping_mul(rhs));
}

fn evaluate_division(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().value;
    let lhs = state.stack.pop().value;
    state.stack.push(lhs.wrapping_div(rhs));
}

fn evaluate_equals(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().value;
    let lhs = state.stack.pop().value;
    state.stack.push((lhs == rhs) as _);
}

fn evaluate_not_equals(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().value;
    let lhs = state.stack.pop().value;
    state.stack.push((lhs != rhs) as _);
}

fn array_equals(state: &mut EvaluatorState) -> bool {
    // Pop lhs and rhs and eventual array to get the correct other side (lhs)
    let rhs = state.stack.pop();
    let lhs = state.stack.pop();

    assert!(
        lhs.is_pointer,
        "{:#?}, stack = {:?}, pointers = {:?}",
        lhs, state.stack.data, state.stack.flags
    );
    assert!(
        rhs.is_pointer,
        "{:#?}, stack = {:?}, pointers = {:?}",
        rhs, state.stack.data, state.stack.flags
    );
    let lhs_address = lhs.value;
    let lhs_length_in_bytes = if is_heap_pointer(lhs_address) {
        state.heap.read_word(lhs_address as _)
    } else {
        state.stack.read_word(lhs_address as _)
    };
    let lhs_length_in_words = bytes_to_word(lhs_length_in_bytes);
    let rhs_address = rhs.value;
    let rhs_length_in_bytes = if is_heap_pointer(rhs_address) {
        state.heap.read_word(rhs_address as _)
    } else {
        state.stack.read_word(rhs_address as _)
    };
    let _rhs_length_in_words = bytes_to_word(rhs_length_in_bytes);
    // If two arrays are not equal in length, we don't compare their elements.
    // But when we compare their elements we expect a true result and only
    // change it if its false.
    let mut result = if lhs_length_in_bytes == rhs_length_in_bytes {
        true
    } else {
        false
    };
    if lhs_address != rhs_address && lhs_length_in_bytes == rhs_length_in_bytes {
        for i in (0..lhs_length_in_words * WORD_SIZE_IN_BYTES).step_by(WORD_SIZE_IN_BYTES as _) {
            let lhs_index = lhs_address as usize + i as usize;
            let rhs_index = rhs_address as usize + i as usize;
            let lhs = if is_heap_pointer(lhs_address) {
                state.heap.read_word(lhs_index)
            } else {
                state.stack.read_word(lhs_index)
            };
            let rhs = if is_heap_pointer(rhs_address) {
                state.heap.read_word(rhs_index)
            } else {
                state.stack.read_word(rhs_index)
            };
            if lhs != rhs {
                result = false;
                break;
            }
        }
    }
    result
}

fn evaluate_array_equals(state: &mut EvaluatorState, _: Instruction) {
    let result = array_equals(state);
    state.stack.push(result as _);
}

fn evaluate_array_not_equals(state: &mut EvaluatorState, _: Instruction) {
    let result = !array_equals(state);
    state.stack.push(result as _);
}

fn evaluate_less_than(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().value;
    let lhs = state.stack.pop().value;
    state.stack.push((lhs < rhs) as _);
}

fn evaluate_greater_than(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().value;
    let lhs = state.stack.pop().value;
    state.stack.push((lhs > rhs) as _);
}

fn evaluate_less_than_equals(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().value;
    let lhs = state.stack.pop().value;
    state.stack.push((lhs <= rhs) as _);
}

fn evaluate_greater_than_equals(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().value;
    let lhs = state.stack.pop().value;
    state.stack.push((lhs >= rhs) as _);
}

fn evaluate_string_concat(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop();
    assert!(rhs.is_pointer, "{:#?}", rhs);
    let lhs = state.stack.pop();
    assert!(
        lhs.is_pointer,
        "lhs = {:#?}, rhs = {:#?}, stack = {:x?}",
        lhs, rhs, state.stack.data
    );

    let lhs_length = if is_heap_pointer(lhs.value) {
        state.heap.read_word(lhs.value as usize)
    } else {
        state.stack.read_word(lhs.value as usize)
    };
    let rhs_length = if is_heap_pointer(rhs.value) {
        state.heap.read_word(rhs.value as usize)
    } else {
        state.stack.read_word(rhs.value as usize)
    };
    let result_length = lhs_length + rhs_length;
    let mut pointer = state.heap.allocate(result_length + WORD_SIZE_IN_BYTES);
    if pointer == 0 {
        println!(
            "RuntimeError: No memory left for string with length {} + {}.",
            result_length, WORD_SIZE_IN_BYTES,
        );
        pointer = HEAP_POINTER;
    } else {
        let mut writing_pointer = pointer;
        state.heap.write_word(writing_pointer as _, result_length);
        writing_pointer += WORD_SIZE_IN_BYTES;
        for i in 0..lhs_length {
            let lhs_address = lhs.value;
            let address = lhs_address + i + WORD_SIZE_IN_BYTES;
            let address = address as usize;
            let lhs_byte = if is_heap_pointer(address as _) {
                state
                    .heap
                    .read_byte(address)
            } else {
                let word = state
                    .stack
                    .read_word(address & !(WORD_SIZE_IN_BYTES - 1) as usize);
                let bytes = word.to_be_bytes();
                bytes[address % WORD_SIZE_IN_BYTES as usize]
            };
            state.heap.write_byte(writing_pointer as _, lhs_byte);
            writing_pointer += 1;
        }
        for i in 0..rhs_length {
            let rhs_address = rhs.value;
            let address = rhs_address + i + WORD_SIZE_IN_BYTES;
            let address = address as usize;
            let rhs_byte = if is_heap_pointer(address as _) {
                state
                    .heap
                    .read_byte(address)
            } else {
                let word = state
                    .stack
                    .read_word(address & !(WORD_SIZE_IN_BYTES - 1) as usize);
                let bytes = word.to_be_bytes();
                bytes[address % WORD_SIZE_IN_BYTES as usize]
            };
            state.heap.write_byte(writing_pointer as _, rhs_byte);
            writing_pointer += 1;
        }
    }
    state.stack.push_pointer(pointer);
}

fn evaluate_jmp_relative(state: &mut EvaluatorState, instruction: Instruction) {
    state.pc = ((state.pc as i64) + (instruction.arg as i64)) as usize;
}

fn evaluate_jmp_if_false(state: &mut EvaluatorState, instruction: Instruction) {
    let condition = state.stack.pop().value;
    if condition == 0 {
        state.pc = ((state.pc as i64) + (instruction.arg as i64)) as usize;
    }
}

#[derive(Clone, Copy, Debug)]
pub struct TypedU64 {
    value: u64,
    is_pointer: bool,
}

fn evaluate_sys_call(state: &mut EvaluatorState, instruction: Instruction) {
    let sys_call_kind = SystemCallKind::try_from_primitive((instruction.arg & 0xFF) as u8).unwrap();
    let argument_count = (instruction.arg >> 8) as usize;
    let mut arguments = Vec::with_capacity(argument_count);
    let mut types = Vec::with_capacity(argument_count);

    for _ in 0..argument_count {
        let type_ = state.stack.pop().value;
        let type_ = Type::from_type_identifier(type_)
            .unwrap_or_else(|| panic!("Invalid type identifier = {} | 0x{:x}", type_, type_));
        types.push(type_);
        arguments.push(state.stack.pop());
    }
    match sys_call_kind {
        SystemCallKind::Print => sys_calls::print(types.remove(0), arguments[0], state),
        SystemCallKind::ArrayLength => {
            sys_calls::array_length(types.remove(0), arguments[0], state)
        }
        SystemCallKind::ToString => sys_calls::to_string(types.remove(0), arguments[0], state),
    }
}

fn evaluate_function_call(state: &mut EvaluatorState, instruction: Instruction) {
    let base = state.stack.pop();
    assert!(base.is_pointer);
    let return_address = state.pc + 1;
    let mut argument_values = vec![];
    for _ in 0..instruction.arg {
        argument_values.push(state.stack.pop());
    }
    state.stack.push(return_address as _);
    for v in argument_values.into_iter().rev() {
        if v.is_pointer {
            state.stack.push_pointer(v.value);
        } else {
            state.stack.push(v.value);
        }
    }
    state.pc = base.value as _;
}

fn evaluate_return(state: &mut EvaluatorState, instruction: Instruction) {
    let has_return_value = instruction.arg != 0;
    if has_return_value {
        todo!()
        // let result = pop_array(state);
        // let return_address = state.stack.pop().unwrap();
        // state.pc = return_address as _;
        // for v in result.1 {
        //     if v.is_pointer {
        //         state.stack.set_pointer(state.stack.len());
        //     }
        //     state.stack.push(v.value);
        // }
        // if result.0.is_pointer {
        //     state.stack.set_pointer(state.stack.len());
        // }
        // state.stack.push(result.0.value);
    } else {
        let return_address = state.stack.pop().value;
        state.pc = return_address as _;
    }
}
