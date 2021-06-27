use num_enum::TryFromPrimitive;

use crate::{DebugFlags, binder::typing::SystemCallKind, instruction_converter::instruction::{op_codes::OpCode, Instruction}, value::Value};

type ResultType = Value;

struct EvaluatorState {
    stack: Vec<u64>,
    registers: Vec<u64>,
    pc: usize,
}

impl EvaluatorState {
    fn set_variable(&mut self, variable: u64, value: u64) {
        let variable = variable as usize;
        while self.registers.len() <= variable {
            self.registers.push(0);
        }
        self.registers[variable] = value;
    }
}

pub fn evaluate(instructions: Vec<Instruction>, debug_flags: DebugFlags) -> ResultType {
    let mut state = EvaluatorState {
        stack: vec![],
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
        (state.stack.pop().unwrap() as i64).into()
    } else if state.stack.is_empty() {
        for (variable, &value) in state.registers.iter().enumerate() {
            println!("{:00}: {}", variable, value as i64)
        }
        Value::Integer(-1)
    } else {
        panic!();
    }
}

fn execute_instruction(state: &mut EvaluatorState, instruction: Instruction) {
    match instruction.op_code {
        OpCode::NoOp => {}
        OpCode::LoadImmediate => evaluate_load_immediate(state, instruction),
        OpCode::Pop => evaluate_pop(state, instruction),
        OpCode::LoadRegister => evaluate_load_register(state, instruction),
        OpCode::StoreInRegister => evaluate_assign_to_variable(state, instruction),
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
    assert!(state.stack.pop().is_some())
}

fn evaluate_load_register(state: &mut EvaluatorState, instruction: Instruction) {
    let value = state.registers[instruction.arg as usize];
    state.stack.push(value);
}

fn evaluate_assign_to_variable(state: &mut EvaluatorState, instruction: Instruction) {
    let value = state.stack.pop().unwrap();
    state.set_variable(instruction.arg, value);
}

fn evaluate_bitwise_twos_complement(state: &mut EvaluatorState, _: Instruction) {
    let value = state.stack.pop().unwrap() as i64;
    state.stack.push((-value) as u64);
}

fn evaluate_bitwise_xor(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap();
    let lhs = state.stack.pop().unwrap();
    state.stack.push(lhs ^ rhs);
}

fn evaluate_bitwise_nxor(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap();
    let lhs = state.stack.pop().unwrap();
    state.stack.push(!(lhs ^ rhs));
}

fn evaluate_addition(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap();
    let lhs = state.stack.pop().unwrap();
    state.stack.push(lhs.wrapping_add(rhs));
}

fn evaluate_subtraction(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap();
    let lhs = state.stack.pop().unwrap();
    state.stack.push(lhs.wrapping_sub(rhs));
}

fn evaluate_multiplication(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap();
    let lhs = state.stack.pop().unwrap();
    state.stack.push(lhs.wrapping_mul(rhs));
}

fn evaluate_division(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap();
    let lhs = state.stack.pop().unwrap();
    state.stack.push(lhs.wrapping_div(rhs));
}

fn evaluate_equals(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap();
    let lhs = state.stack.pop().unwrap();
    state.stack.push((lhs == rhs) as _);
}

fn evaluate_not_equals(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap();
    let lhs = state.stack.pop().unwrap();
    state.stack.push((lhs != rhs) as _);
}

fn evaluate_less_than(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap();
    let lhs = state.stack.pop().unwrap();
    state.stack.push((lhs < rhs) as _);
}

fn evaluate_greater_than(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap();
    let lhs = state.stack.pop().unwrap();
    state.stack.push((lhs > rhs) as _);
}

fn evaluate_less_than_equals(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap();
    let lhs = state.stack.pop().unwrap();
    state.stack.push((lhs <= rhs) as _);
}

fn evaluate_greater_than_equals(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap();
    let lhs = state.stack.pop().unwrap();
    state.stack.push((lhs >= rhs) as _);
}

fn evaluate_jmp_relative(state: &mut EvaluatorState, instruction: Instruction) {
    state.pc = ((state.pc as i64) + (instruction.arg as i64)) as usize;
}

fn evaluate_jmp_if_false(state: &mut EvaluatorState, instruction: Instruction) {
    let condition = state.stack.pop().unwrap();
    if condition == 0 {
        state.pc = ((state.pc as i64) + (instruction.arg as i64)) as usize;
    }
}

fn evaluate_sys_call(state: &mut EvaluatorState, instruction: Instruction) {
    let sys_call_kind = SystemCallKind::try_from_primitive((instruction.arg & 0xFF) as u8).unwrap();
    let argument_count = (instruction.arg >> 8) as usize;
    let mut arguments = Vec::with_capacity(argument_count);
    for _ in 0..argument_count {
        arguments.push(state.stack.pop().unwrap());
    }
    match sys_call_kind {
        SystemCallKind::Print => println!("PRINT {}", arguments[0]),
    }
}