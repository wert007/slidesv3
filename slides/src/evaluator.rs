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
            self.registers.push(TypedU64 {value: 0, is_pointer: false });
        }
        self.registers[variable] = TypedU64 {value, is_pointer};
    }

    fn set_pointer(&mut self, address: usize) {
        if !self.pointers.contains(&address) {
            self.pointers.push(address);
        }
    }

    fn pop_stack(&mut self) -> Option<TypedU64> {
        let is_pointer = if let Some(i) = self.pointers.iter().position(|p| p == &self.stack.len()) {
            self.pointers.remove(i);
            true
        } else {
            false
        };
        self.stack.pop().map(|value| TypedU64 {value, is_pointer})
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

fn evaluate_pop(state: &mut EvaluatorState, _: Instruction) {
    let address = state.pop_stack().unwrap();
    if address.is_pointer {
        let address = address.value;
        let difference = state.stack.len() as u64 - address;
        if difference == state.stack[address as usize] {
            while state.stack.len() != address as usize {
                assert!(state.pop_stack().is_some());
            }
        }
    }
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

fn evaluate_array_literal(state: &mut EvaluatorState, instruction: Instruction) {
    let array_length_in_bytes = instruction.arg;
    let array_length = array_length_in_bytes / 4;
    let array_address = state.stack.len() as u64 - array_length - 1;
    state.stack.push(array_address);
    state.set_pointer(state.stack.len());
}

fn evaluate_array_index(state: &mut EvaluatorState, _: Instruction) {
    let index = state.pop_stack().unwrap().value;
    let array = state.pop_stack().unwrap();
    assert!(array.is_pointer, "array = {:#?}, pointers = {:#?}, stack = {:#?}", array, state.pointers, state.stack);
    let array = array.value;
    let value = state.stack[array as usize + 1 + index as usize];
    state.stack.push(value);
}

fn evaluate_write_to_memory(state: &mut EvaluatorState, _: Instruction) {
    let index = state.pop_stack().unwrap().value;
    let array = state.pop_stack().unwrap();
    assert!(array.is_pointer, "array = {:#?}, pointers = {:#?}, stack = {:#?}", array, state.pointers, state.stack);
    let array = array.value;
    let value = state.pop_stack().unwrap().value;
    state.stack[array as usize + 1 + index as usize] = value;
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
