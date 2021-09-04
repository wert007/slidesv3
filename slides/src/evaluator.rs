mod sys_calls;

use crate::{
    binder::typing::SystemCallKind,
    instruction_converter::instruction::{op_codes::OpCode, Instruction},
    value::Value,
    DebugFlags,
};
use num_enum::TryFromPrimitive;

type ResultType = Value;

struct EvaluatorState {
    stack: Vec<u64>,
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

    fn pop_stack(&mut self) -> Option<TypedU64> {
        let is_pointer = if let Some(i) = self.pointers.iter().position(|p| p == &self.stack.len())
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
        OpCode::ArrayLength => evaluate_array_literal(state, instruction),
        OpCode::ArrayIndex => evaluate_array_index(state, instruction),
        OpCode::StoreInMemory => evaluate_write_to_memory(state, instruction),
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
    if address.is_pointer {
        let address = address.value;
        let length = state.stack[address as usize] / 4;
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

fn evaluate_pop(state: &mut EvaluatorState, _: Instruction) {
    pop_array(state);
}

fn evaluate_load_register(state: &mut EvaluatorState, instruction: Instruction) {
    let value = state.registers[instruction.arg as usize];
    state.stack.push(value.value);
    if value.is_pointer {
        state.set_pointer(state.stack.len());
    }
}

fn evaluate_assign_to_variable(state: &mut EvaluatorState, instruction: Instruction) {
    let TypedU64 { value, is_pointer } = state.pop_stack().unwrap();
    state.set_variable(instruction.arg, value, is_pointer);
}

fn evaluate_array_literal(state: &mut EvaluatorState, _: Instruction) {
    let array_address = state.stack.len() as u64 - 1;
    state.stack.push(array_address);
    state.set_pointer(state.stack.len());
}

fn evaluate_array_index(state: &mut EvaluatorState, _: Instruction) {
    let index = state.pop_stack().unwrap().value;
    let array = state.pop_stack().unwrap();
    assert!(
        array.is_pointer,
        "array = {:#?}, pointers = {:#?}, stack = {:#?}",
        array, state.pointers, state.stack
    );
    let array = array.value;
    let array_length = state.stack[array as usize] / 4;
    if (index as i64) < 0 || index > array_length {
        println!(
            "RuntimeError: Index {} is out of Bounds ({}) for this array!",
            index as i64, array_length
        );
        state.stack.push(0);
        return;
    }
    let value = state.stack[array as usize - 1 - index as usize];
    state.stack.push(value);
}

fn evaluate_write_to_memory(state: &mut EvaluatorState, _: Instruction) {
    let index = state.pop_stack().unwrap().value;
    let array = state.pop_stack().unwrap();
    assert!(
        array.is_pointer,
        "array = {:#?}, pointers = {:#?}, stack = {:#?}",
        array, state.pointers, state.stack
    );
    let array = array.value;
    let value = state.pop_stack().unwrap().value;
    let array_length = state.stack[array as usize] / 4;
    if (index as i64) < 0 || index > array_length {
        println!(
            "RuntimeError: Index {} is out of Bounds ({}) for this array!",
            index as i64, array_length
        );
        return;
    }
    state.stack[array as usize - 1 - index as usize] = value;
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

fn evaluate_array_equals(state: &mut EvaluatorState, _: Instruction) {
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
        "{:#?}, stack = {:#?}, pointers = {:#?}",
        lhs, state.stack, state.pointers
    );
    assert!(
        rhs.is_pointer,
        "{:#?}, stack = {:#?}, pointers = {:#?}",
        rhs, state.stack, state.pointers
    );
    let lhs_address = lhs.value;
    let lhs_length = state.stack[lhs_address as usize] / 4;
    let rhs_address = rhs.value;
    let rhs_length = state.stack[rhs_address as usize] / 4;
    // If two arrays are not equal in length, we don't compare their elements.
    // But when we compare their elements we expect a true result and only
    // change it if its false.
    let mut result = if lhs_length == rhs_length { 1 } else { 0 };
    if lhs_address != rhs_address && lhs_length == rhs_length {
        for i in 0..lhs_length {
            let lhs = state.stack[lhs_address as usize - 1 - i as usize];
            let rhs = state.stack[rhs_address as usize - 1 - i as usize];
            if lhs != rhs {
                result = 0;
                break;
            }
        }
    }
    // Clear stack after the comparison.
    while state.stack.len() > expected_stack_length {
        assert!(state.pop_stack().is_some());
    }
    state.stack.push(result);
}

fn evaluate_array_not_equals(state: &mut EvaluatorState, _: Instruction) {
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
        "{:#?}, stack = {:#?}, pointers = {:#?}",
        lhs, state.stack, state.pointers
    );
    assert!(
        rhs.is_pointer,
        "{:#?}, stack = {:#?}, pointers = {:#?}",
        rhs, state.stack, state.pointers
    );
    let lhs_address = lhs.value;
    let lhs_length = state.stack[lhs_address as usize] / 4;
    let rhs_address = rhs.value;
    let rhs_length = state.stack[rhs_address as usize] / 4;
    // If two arrays are not equal in length, we don't compare their elements.
    // But when we compare their elements we expect a false result and only
    // change it if its true.
    let mut result = if lhs_length == rhs_length { 0 } else { 1 };
    if lhs_address != rhs_address && lhs_length == rhs_length {
        for i in 0..lhs_length {
            let lhs = state.stack[lhs_address as usize - 1 - i as usize];
            let rhs = state.stack[rhs_address as usize - 1 - i as usize];
            if lhs != rhs {
                result = 1;
                break;
            }
        }
    }
    // Clear stack after the comparison.
    while state.stack.len() > expected_stack_length {
        assert!(state.pop_stack().is_some());
    }
    state.stack.push(result);
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
    for _ in 0..argument_count {
        arguments.push(state.pop_stack().unwrap());
    }
    match sys_call_kind {
        SystemCallKind::Print => sys_calls::print(arguments[0], &state.stack),
    }
}
