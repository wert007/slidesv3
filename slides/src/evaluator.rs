mod allocator;
mod sys_calls;

use crate::{
    binder::typing::{SystemCallKind, Type},
    instruction_converter::instruction::{op_codes::OpCode, Instruction},
    value::Value,
    DebugFlags,
};
use num_enum::TryFromPrimitive;

use self::allocator::Allocator;

type ResultType = Value;

const HEAP_POINTER: u64 = 0x80_00_00_00_00_00_00_00;

pub struct EvaluatorState {
    stack: Vec<u64>,
    heap: Allocator,
    registers: Vec<TypedU64>,
    pc: usize,
    pointers: Vec<usize>,
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

    fn set_pointer(&mut self, address: usize) {
        if !self.pointers.contains(&address) {
            self.pointers.push(address);
        }
    }

    fn is_pointer(&mut self, address: usize) -> bool {
        is_heap_pointer(address as _) || self.pointers.contains(&address)
    }

    fn pop_stack(&mut self) -> Option<TypedU64> {
        let is_pointer = if let Some(i) = self
            .pointers
            .iter()
            .position(|&p| p == self.stack.len() - 1)
        {
            self.pointers.remove(i);
            true
        } else {
            false
        };
        self.stack.pop().map(|value| TypedU64 { value, is_pointer })
    }
}

pub fn evaluate(instructions: Vec<Instruction>, debug_flags: DebugFlags) -> ResultType {
    let mut state = EvaluatorState {
        stack: vec![],
        heap: Allocator::new(512, debug_flags),
        registers: vec![],
        pc: 0,
        pointers: vec![],
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
        (state.pop_stack().unwrap().value as i64).into()
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
        OpCode::NoOp => {}
        OpCode::LoadImmediate => evaluate_load_immediate(state, instruction),
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
    }
}

fn evaluate_load_immediate(state: &mut EvaluatorState, instruction: Instruction) {
    state.stack.push(instruction.arg);
}

fn pop_array(state: &mut EvaluatorState) -> (TypedU64, Vec<TypedU64>) {
    let address = state.pop_stack().unwrap();
    let mut popped = vec![];
    if address.is_pointer && !is_heap_pointer(address.value) {
        let address = address.value;
        let length = (state.stack[address as usize] + 3) / 4;
        let difference = state.stack.len() as u64 - address;
        if difference == 1 {
            for _ in 0..length + 1 {
                match state.pop_stack() {
                    Some(value) => popped.push(value),
                    None => unreachable!(),
                }
            }
        }
    }
    (address, popped)
}

fn is_heap_pointer(address: u64) -> bool {
    address & HEAP_POINTER > 0
}

fn evaluate_pop(state: &mut EvaluatorState, _: Instruction) {
    pop_array(state);
}

fn evaluate_load_register(state: &mut EvaluatorState, instruction: Instruction) {
    let value = state.registers[instruction.arg as usize];
    if value.is_pointer {
        state.set_pointer(state.stack.len());
    }
    state.stack.push(value.value);
}

fn evaluate_assign_to_variable(state: &mut EvaluatorState, instruction: Instruction) {
    let TypedU64 { value, is_pointer } = state.pop_stack().unwrap();
    state.set_variable(instruction.arg, value, is_pointer);
}

fn evaluate_create_stack_pointer(state: &mut EvaluatorState, instruction: Instruction) {
    let stack_pointer = state.stack.len() as u64 - instruction.arg;
    state.stack.push(stack_pointer);
    state.set_pointer(state.stack.len() - 1);
}

fn evaluate_array_index(state: &mut EvaluatorState, _: Instruction) {
    let index = state.pop_stack().unwrap().value;
    let array = state.pop_stack().unwrap();
    assert!(
        array.is_pointer,
        "array = {:#?}, pointers = {:?}, stack = {:?}",
        array, state.pointers, state.stack
    );
    let array = array.value;

    let array_length = if is_heap_pointer(array) {
        state.heap.read_word(array as _)
    } else {
        state.stack[array as usize]
    } / 4;
    if (index as i64) < 0 || index > array_length {
        println!(
            "RuntimeError: Index {} is out of Bounds ({}) for this array!",
            index as i64, array_length
        );
        state.stack.push(0);
        return;
    }
    let index = array as usize - 1 - index as usize;
    let value = if is_heap_pointer(array) {
        state.heap.read_word(index)
    } else {
        state.stack[index]
    };
    if state.is_pointer(index) {
        state.set_pointer(state.stack.len());
    }
    state.stack.push(value);
}

fn evaluate_write_to_memory(state: &mut EvaluatorState, _: Instruction) {
    let index = state.pop_stack().unwrap().value;
    let array = state.pop_stack().unwrap();
    assert!(
        array.is_pointer,
        "array = {:#?}, pointers = {:?}, stack = {:?}",
        array, state.pointers, state.stack
    );
    let array = array.value;
    let value = state.pop_stack().unwrap().value;
    let array_length = if is_heap_pointer(array) {
        state.heap.read_word(array as _)
    } else {
        state.stack[array as usize]
    } / 4;
    if (index as i64) < 0 || index > array_length {
        println!(
            "RuntimeError: Index {} is out of Bounds ({}) for this array!",
            index as i64, array_length
        );
        return;
    }
    let index = array as usize - 1 - index as usize;
    if is_heap_pointer(array) {
        state.heap.write_word(index, value);
    } else {
        state.stack[index] = value;
    }
}

fn evaluate_bitwise_twos_complement(state: &mut EvaluatorState, _: Instruction) {
    let value = state.pop_stack().unwrap().value as i64;
    state.stack.push((-value) as u64);
}

fn evaluate_bitwise_xor(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.pop_stack().unwrap().value;
    let lhs = state.pop_stack().unwrap().value;
    state.stack.push(lhs ^ rhs);
}

fn evaluate_bitwise_nxor(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.pop_stack().unwrap().value;
    let lhs = state.pop_stack().unwrap().value;
    state.stack.push(!(lhs ^ rhs));
}

fn evaluate_addition(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.pop_stack().unwrap().value;
    let lhs = state.pop_stack().unwrap().value;
    state.stack.push(lhs.wrapping_add(rhs));
}

fn evaluate_subtraction(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.pop_stack().unwrap().value;
    let lhs = state.pop_stack().unwrap().value;
    state.stack.push(lhs.wrapping_sub(rhs));
}

fn evaluate_multiplication(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.pop_stack().unwrap().value;
    let lhs = state.pop_stack().unwrap().value;
    state.stack.push(lhs.wrapping_mul(rhs));
}

fn evaluate_division(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.pop_stack().unwrap().value;
    let lhs = state.pop_stack().unwrap().value;
    state.stack.push(lhs.wrapping_div(rhs));
}

fn evaluate_equals(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.pop_stack().unwrap().value;
    let lhs = state.pop_stack().unwrap().value;
    state.stack.push((lhs == rhs) as _);
}

fn evaluate_not_equals(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.pop_stack().unwrap().value;
    let lhs = state.pop_stack().unwrap().value;
    state.stack.push((lhs != rhs) as _);
}

fn array_equals(state: &mut EvaluatorState) -> bool {
    // Pop lhs and rhs and eventual array to get the correct other side (lhs)
    let (rhs, rhs_values) = pop_array(state);
    let (lhs, lhs_values) = pop_array(state);

    // Remember how long the stack is supposed to be after comparison
    let expected_stack_length = state.stack.len();

    // Reconstruct stack so that the pointers work
    for v in lhs_values.into_iter().rev() {
        state.stack.push(v.value);
        assert!(!v.is_pointer);
    }
    state.stack.push(lhs.value);
    for v in rhs_values.into_iter().rev() {
        state.stack.push(v.value);
        assert!(!v.is_pointer);
    }
    state.stack.push(rhs.value);

    assert!(
        lhs.is_pointer,
        "{:#?}, stack = {:?}, pointers = {:?}",
        lhs, state.stack, state.pointers
    );
    assert!(
        rhs.is_pointer,
        "{:#?}, stack = {:?}, pointers = {:?}",
        rhs, state.stack, state.pointers
    );
    let lhs_address = lhs.value;
    let lhs_length = if is_heap_pointer(lhs_address) {
        state.heap.read_word(lhs_address as _)
    } else {
        state.stack[lhs_address as usize]
    } / 4;
    let rhs_address = rhs.value;
    let rhs_length = if is_heap_pointer(rhs_address) {
        state.heap.read_word(rhs_address as _)
    } else {
        state.stack[rhs_address as usize]
    } / 4;
    // If two arrays are not equal in length, we don't compare their elements.
    // But when we compare their elements we expect a true result and only
    // change it if its false.
    let mut result = if lhs_length == rhs_length {
        true
    } else {
        false
    };
    if lhs_address != rhs_address && lhs_length == rhs_length {
        for i in 0..lhs_length {
            let lhs_index = lhs_address as usize - 1 - i as usize;
            let rhs_index = rhs_address as usize - 1 - i as usize;
            let lhs = if is_heap_pointer(lhs_address) {
                state.heap.read_word(lhs_index)
            } else {
                state.stack[lhs_index]
            };
            let rhs = if is_heap_pointer(rhs_address) {
                state.heap.read_word(rhs_index)
            } else {
                state.stack[rhs_index]
            };
            if lhs != rhs {
                result = false;
                break;
            }
        }
    }
    // Clear stack after the comparison.
    while state.stack.len() > expected_stack_length {
        assert!(state.pop_stack().is_some());
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
    let rhs = state.pop_stack().unwrap().value;
    let lhs = state.pop_stack().unwrap().value;
    state.stack.push((lhs < rhs) as _);
}

fn evaluate_greater_than(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.pop_stack().unwrap().value;
    let lhs = state.pop_stack().unwrap().value;
    state.stack.push((lhs > rhs) as _);
}

fn evaluate_less_than_equals(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.pop_stack().unwrap().value;
    let lhs = state.pop_stack().unwrap().value;
    state.stack.push((lhs <= rhs) as _);
}

fn evaluate_greater_than_equals(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.pop_stack().unwrap().value;
    let lhs = state.pop_stack().unwrap().value;
    state.stack.push((lhs >= rhs) as _);
}

fn evaluate_string_concat(state: &mut EvaluatorState, _: Instruction) {
    let (rhs, rhs_values) = pop_array(state);
    assert!(rhs.is_pointer, "{:#?}", rhs);
    let (lhs, lhs_values) = pop_array(state);
    assert!(lhs.is_pointer, "lhs = {:#?}, rhs = {:#?}, rhs_values = {:x?}, stack = {:x?}", lhs, rhs, rhs_values, state.stack);

    // Remember how long the stack is supposed to be after comparison
    let expected_stack_length = state.stack.len();

    // Reconstruct stack so that the pointers work
    for v in lhs_values.into_iter().rev() {
        state.stack.push(v.value);
        assert!(!v.is_pointer);
    }
    state.stack.push(lhs.value);
    for v in rhs_values.into_iter().rev() {
        state.stack.push(v.value);
        assert!(!v.is_pointer);
    }
    state.stack.push(rhs.value);

    let lhs_length = if is_heap_pointer(lhs.value) {
        state.heap.read_word(lhs.value as usize)
    } else {
        state.stack[lhs.value as usize]
    };
    let rhs_length = if is_heap_pointer(rhs.value) {
        state.heap.read_word(rhs.value as usize)
    } else {
        state.stack[rhs.value as usize]
    };
    let result_length = lhs_length + rhs_length;
    let mut pointer = state.heap.allocate(result_length + 4);
    if pointer == 0 {
        println!("RuntimeError: No memory left for string with length {} + 4.", result_length);
        pointer = HEAP_POINTER;
    }
    else {   
        let mut writing_pointer = pointer;
        state.heap.write_word(writing_pointer as _, result_length);
        writing_pointer += 4;
        for i in 0..lhs_length {
            let lhs_address = lhs.value;
            let lhs_byte = if is_heap_pointer(lhs_address + i) {
                state.heap.read_byte(lhs_address as usize + i as usize)
            } else {
                let word = state.stack[lhs_address as usize - 1 - (i / 4) as usize];
                let bytes = [
                    (word & 0xFF) as u8,
                    ((word >> 8) & 0xFF) as u8,
                    ((word >> 16) & 0xFF) as u8,
                    ((word >> 24) & 0xFF) as u8,
                ];
                bytes[(i % 4) as usize]
            };
            state.heap.write_byte(writing_pointer as _, lhs_byte);
            writing_pointer += 1;
        }
        for i in 0..rhs_length {
            let rhs_address = rhs.value;
            let rhs_byte = if is_heap_pointer(rhs_address + i) {
                state.heap.read_byte(rhs_address as usize + i as usize)
            } else {
                let word = state.stack[rhs_address as usize - 1 - (i / 4) as usize];
                let bytes = [
                    (word & 0xFF) as u8,
                    ((word >> 8) & 0xFF) as u8,
                    ((word >> 16) & 0xFF) as u8,
                    ((word >> 24) & 0xFF) as u8,
                ];
                bytes[(i % 4) as usize]
            };
            state.heap.write_byte(writing_pointer as _, rhs_byte);
            writing_pointer += 1;
        }
    }

    // Clear stack after the comparison.
    while state.stack.len() > expected_stack_length {
        assert!(state.pop_stack().is_some());
    }

    state.set_pointer(state.stack.len());
    state.stack.push(pointer);
}

fn evaluate_jmp_relative(state: &mut EvaluatorState, instruction: Instruction) {
    state.pc = ((state.pc as i64) + (instruction.arg as i64)) as usize;
}

fn evaluate_jmp_if_false(state: &mut EvaluatorState, instruction: Instruction) {
    let condition = state.pop_stack().unwrap().value;
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
        let type_ = state.pop_stack().unwrap().value;
        let type_ = Type::from_type_identifier(type_).unwrap_or_else(|| panic!("Invalid type identifier = {} | 0x{:x}", type_, type_));
        types.push(type_);
        arguments.push(state.pop_stack().unwrap());
    }
    match sys_call_kind {
        SystemCallKind::Print => sys_calls::print(types.remove(0), arguments[0], state),
        SystemCallKind::ArrayLength => sys_calls::array_length(types.remove(0), arguments[0], state),
    }
}
