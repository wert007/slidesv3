pub mod memory;
mod sys_calls;

use crate::{DebugFlags, DiagnosticBag, binder::typing::{SystemCallKind, Type}, evaluator::memory::WORD_SIZE_IN_BYTES, instruction_converter::{
        instruction::{op_codes::OpCode, Instruction},
        Program,
    }, value::Value};
use num_enum::TryFromPrimitive;

use self::memory::{FlaggedWord, allocator::{Allocator, garbage_collector::garbage_collect}, stack::Stack};

macro_rules! runtime_error {
    ($evaluator:ident, $($fn_call:tt)*) => {
        $evaluator.runtime_diagnostics.$($fn_call)*;
        $evaluator.runtime_diagnostics.clone().flush_to_console();
        $evaluator.runtime_diagnostics.diagnostics.clear();
    };
}

type ResultType = Value;

pub struct EvaluatorState<'a> {
    stack: Stack,
    heap: Allocator,
    registers: Vec<FlaggedWord>,
    protected_registers: usize,
    pc: usize,
    is_main_call: bool,
    runtime_diagnostics: DiagnosticBag<'a>,
}

impl EvaluatorState<'_> {
    fn set_variable(&mut self, variable: u64, value: FlaggedWord) {
        let variable = variable as usize;
        self.registers[variable] = value;
    }

    fn read_pointer(&self, address: u64) -> FlaggedWord {
        if memory::is_heap_pointer(address) {
            self.heap.read_flagged_word(address)
        } else {
            self.stack.read_flagged_word(address)
        }
    }

    fn write_pointer(&mut self, address: u64, value: FlaggedWord) {
        if memory::is_heap_pointer(address) {
            self.heap.write_flagged_word(address, value);
        } else {
            self.stack.write_flagged_word(address, value);
        }
    }

    fn garbage_collect(&mut self) {
        let mut unchecked_pointers = vec![];
        for (flags, &value) in self.stack.flags.iter().zip(&self.stack.data) {
            if flags.is_pointer {
                unchecked_pointers.push(value);
            }
        }
        for value in &self.registers {
            if value.is_pointer() {
                unchecked_pointers.push(value.unwrap_pointer());
            }
        }
        garbage_collect(unchecked_pointers, &mut self.heap);
    }
}

pub fn evaluate(
    program: Program,
    source_text: &crate::text::SourceText<'_>,
    debug_flags: DebugFlags,
) -> ResultType {
    let mut state = EvaluatorState {
        stack: program.stack,
        heap: Allocator::new(64 * 1024, debug_flags),
        registers: vec![FlaggedWord::default(); program.max_used_variables],
        protected_registers: program.protected_variables,
        pc: 0,
        is_main_call: true,
        runtime_diagnostics: DiagnosticBag::new(source_text),
    };
    let instructions = program.instructions;
    while state.pc < instructions.len() {
        let pc = state.pc;
        if debug_flags.print_current_instruction() {
            println!("  CI {}: {:?}", pc, instructions[pc]);
        }
        if debug_flags.slow_mode {
            std::thread::sleep(std::time::Duration::from_millis(500));
        }
        execute_instruction(&mut state, instructions[pc]);
        state.pc += 1;
    }
    if state.stack.len() == 1 {
        (state.stack.pop().unwrap_value() as i64).into()
    } else {
        for (variable, &value) in state.registers.iter().enumerate() {
            if value.is_pointer() {
                println!("{:00}: #{}", variable, value.unwrap_pointer())
            } else {
                println!("{:00}: {}", variable, value.unwrap_value() as i64)
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
        OpCode::Duplicate => evaluate_duplicate(state, instruction),
        OpCode::Pop => evaluate_pop(state, instruction),
        OpCode::LoadRegister => evaluate_load_register(state, instruction),
        OpCode::StoreInRegister => evaluate_assign_to_variable(state, instruction),
        OpCode::ArrayIndex => evaluate_array_index(state, instruction),
        OpCode::StoreInMemory => evaluate_write_to_memory(state, instruction),
        OpCode::WriteToStack => evaluate_write_to_stack(state, instruction),
        OpCode::WriteToHeap => evaluate_write_to_heap(state, instruction),
        OpCode::ReadWordWithOffset => evaluate_read_word_with_offset(state, instruction),
        OpCode::MemoryCopy => evaluate_memory_copy(state, instruction),
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
        OpCode::NoneableEquals => evaluate_noneable_equals(state, instruction),
        OpCode::LessThan => evaluate_less_than(state, instruction),
        OpCode::GreaterThan => evaluate_greater_than(state, instruction),
        OpCode::LessThanEquals => evaluate_less_than_equals(state, instruction),
        OpCode::GreaterThanEquals => evaluate_greater_than_equals(state, instruction),
        OpCode::StringConcat => evaluate_string_concat(state, instruction),
        OpCode::PointerAddition => evaluate_pointer_addition(state, instruction),
        OpCode::NoneableOrValue => evaluate_noneable_or_value(state, instruction),
        OpCode::Jump => evaluate_jump(state, instruction),
        OpCode::JumpIfFalse => evaluate_jump_if_false(state, instruction),
        OpCode::JumpIfTrue => evaluate_jump_if_true(state, instruction),
        OpCode::SysCall => evaluate_sys_call(state, instruction),
        OpCode::FunctionCall => evaluate_function_call(state, instruction),
        OpCode::Return => evaluate_return(state, instruction),
        OpCode::DecodeClosure => evaluate_decode_closure(state, instruction),
        OpCode::CheckArrayBounds => evaluate_check_array_bounds(state, instruction),
    }
}

fn evaluate_load_immediate(state: &mut EvaluatorState, instruction: Instruction) {
    state.stack.push(instruction.arg);
}

fn evaluate_load_pointer(state: &mut EvaluatorState, instruction: Instruction) {
    state.stack.push_pointer(instruction.arg);
}

fn evaluate_duplicate(state: &mut EvaluatorState, _: Instruction) {
    let value = state.stack.pop();
    state.stack.push_flagged_word(value);
    state.stack.push_flagged_word(value);
}

fn evaluate_pop(state: &mut EvaluatorState, _: Instruction) {
    state.stack.pop();
}

fn evaluate_load_register(state: &mut EvaluatorState, instruction: Instruction) {
    let value = state.registers[instruction.arg as usize];
    state.stack.push_flagged_word(value);
}

fn evaluate_assign_to_variable(state: &mut EvaluatorState, instruction: Instruction) {
    let value = state.stack.pop();
    state.set_variable(instruction.arg, value);
}

fn evaluate_array_index(state: &mut EvaluatorState, instruction: Instruction) {
    let index_in_words = state.stack.pop().unwrap_value();
    let array = state.stack.pop().unwrap_pointer();

    let array_length_in_bytes = state.read_pointer(array).unwrap_value();
    let array_length_in_words = memory::bytes_to_word(array_length_in_bytes);
    if (index_in_words as i64) < 0 || index_in_words > array_length_in_words {
        runtime_error!(
            state,
            index_out_of_bounds(
                instruction.span,
                index_in_words as i64,
                array_length_in_words
            )
        );
        state.stack.push(0);
        return;
    }
    let index_in_bytes = array
        + index_in_words * WORD_SIZE_IN_BYTES
        + WORD_SIZE_IN_BYTES;
    let value = state.read_pointer(index_in_bytes);
    state.stack.push_flagged_word(value);
}

fn evaluate_write_to_memory(state: &mut EvaluatorState, _: Instruction) {
    let index = state.stack.pop().unwrap_value();
    let array = state.stack.pop().unwrap_pointer();
    let value = state.stack.pop();
    let index = array + index;
    state.write_pointer(index, value);
}

fn evaluate_write_to_stack(state: &mut EvaluatorState, instruction: Instruction) {
    let address = instruction.arg;
    let value = state.stack.pop();
    state.stack.write_flagged_word(address as _, value);
}

fn evaluate_write_to_heap(state: &mut EvaluatorState, instruction: Instruction) {
    let size_in_bytes = instruction.arg * WORD_SIZE_IN_BYTES;
    let address = state.heap.allocate(size_in_bytes);
    let mut writing_pointer = address;
    for _ in 0..instruction.arg {
        let value = state.stack.pop();
        if address != 0 {
            state.heap.write_flagged_word(writing_pointer as _, value);
            writing_pointer += WORD_SIZE_IN_BYTES;
        }
    }
    state.stack.push_pointer(address);
}

fn evaluate_read_word_with_offset(state: &mut EvaluatorState, instruction: Instruction) {
    let address = state.stack.pop().unwrap_pointer();
    let offset = instruction.arg;
    let address = address + offset;
    let value = state.read_pointer(address);
    state.stack.push_flagged_word(value);
}

fn evaluate_memory_copy(state: &mut EvaluatorState, instruction: Instruction) {
    let size_in_bytes = if instruction.arg == 0 {
        state.stack.pop().unwrap_value()
    } else {
        instruction.arg
    };
    // FIXME: Currently there are only complete words and no single bytes
    // supported!
    assert_eq!(size_in_bytes % WORD_SIZE_IN_BYTES, 0);
    let size_in_words = memory::bytes_to_word(size_in_bytes);
    let dest = state.stack.pop().unwrap_pointer();
    let src = state.stack.pop().unwrap_pointer();
    for word_index in 0..size_in_words {
        let src = src + word_index * WORD_SIZE_IN_BYTES;
        let dest = dest + word_index * WORD_SIZE_IN_BYTES;
        let buffer = state.read_pointer(src);
        state.write_pointer(dest, buffer);
    }
}

fn evaluate_bitwise_twos_complement(state: &mut EvaluatorState, _: Instruction) {
    let value = state.stack.pop().unwrap_value() as i64;
    state.stack.push((-value) as u64);
}

fn evaluate_bitwise_xor(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap_value();
    let lhs = state.stack.pop().unwrap_value();
    state.stack.push(lhs ^ rhs);
}

fn evaluate_bitwise_nxor(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap_value();
    let lhs = state.stack.pop().unwrap_value();
    state.stack.push(!(lhs ^ rhs));
}

fn evaluate_addition(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap_value();
    let lhs = state.stack.pop().unwrap_value();
    state.stack.push(lhs.wrapping_add(rhs));
}

fn evaluate_subtraction(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap_value();
    let lhs = state.stack.pop().unwrap_value();
    state.stack.push(lhs.wrapping_sub(rhs));
}

fn evaluate_multiplication(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap_value();
    let lhs = state.stack.pop().unwrap_value();
    state.stack.push(lhs.wrapping_mul(rhs));
}

fn evaluate_division(state: &mut EvaluatorState, instruction: Instruction) {
    let rhs = state.stack.pop().unwrap_value();
    let lhs = state.stack.pop().unwrap_value();
    if rhs == 0 {
        runtime_error!(state, division_by_zero(instruction.span));
        state.stack.push(0);
    } else {
        state.stack.push(lhs.wrapping_div(rhs));
    }
}

fn evaluate_equals(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap_value();
    let lhs = state.stack.pop().unwrap_value();
    state.stack.push((lhs == rhs) as _);
}

fn evaluate_not_equals(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap_value();
    let lhs = state.stack.pop().unwrap_value();
    state.stack.push((lhs != rhs) as _);
}

fn array_equals(state: &mut EvaluatorState) -> bool {
    let rhs = state.stack.pop().unwrap_pointer();
    let lhs = state.stack.pop().unwrap_pointer();

    let lhs_length_in_bytes = state.read_pointer(lhs).unwrap_value();
    let lhs_length_in_words = memory::bytes_to_word(lhs_length_in_bytes);
    let rhs_length_in_bytes = state.read_pointer(rhs).unwrap_value();
    let _rhs_length_in_words = memory::bytes_to_word(rhs_length_in_bytes);
    // If two arrays are not equal in length, we don't compare their elements.
    // But when we compare their elements we expect a true result and only
    // change it if its false.
    let mut result = lhs_length_in_bytes == rhs_length_in_bytes;
    if lhs != rhs && lhs_length_in_bytes == rhs_length_in_bytes {
        for i in (0..lhs_length_in_words * WORD_SIZE_IN_BYTES).step_by(WORD_SIZE_IN_BYTES as _) {
            let lhs_index = lhs + i;
            let rhs_index = rhs + i;
            let lhs = state.read_pointer(lhs_index).unwrap_value();
            let rhs = state.read_pointer(rhs_index).unwrap_value();
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

fn evaluate_noneable_equals(state: &mut EvaluatorState, instruction: Instruction) {
    let rhs = state.stack.pop();
    let lhs = state.stack.pop();
    let result = if rhs.unwrap_pointer() == lhs.unwrap_pointer() {
        true
    } else if rhs.unwrap_pointer() == 0 || lhs.unwrap_pointer() == 0 {
        false
    } else {
        let size_in_bytes = instruction.arg;
        let size_in_words = memory::bytes_to_word(size_in_bytes);
        let mut result = true;
        for offset in 0..size_in_words {
            let rhs_address = rhs.unwrap_pointer() + offset;
            let lhs_address = lhs.unwrap_pointer() + offset;
            let rhs_value = state.read_pointer(rhs_address);
            let lhs_value = state.read_pointer(lhs_address);
            if rhs_value != lhs_value {
                result = false;
                break;
            }
        }
        result
    };
    state.stack.push(result as _);
}

fn evaluate_less_than(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap_value();
    let lhs = state.stack.pop().unwrap_value();
    state.stack.push((lhs < rhs) as _);
}

fn evaluate_greater_than(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap_value();
    let lhs = state.stack.pop().unwrap_value();
    state.stack.push((lhs > rhs) as _);
}

fn evaluate_less_than_equals(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap_value();
    let lhs = state.stack.pop().unwrap_value();
    state.stack.push((lhs <= rhs) as _);
}

fn evaluate_greater_than_equals(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap_value();
    let lhs = state.stack.pop().unwrap_value();
    state.stack.push((lhs >= rhs) as _);
}

fn evaluate_string_concat(state: &mut EvaluatorState, instruction: Instruction) {
    let rhs = state.stack.pop().unwrap_pointer();
    let lhs = state.stack.pop().unwrap_pointer();

    let lhs_length = state.read_pointer(lhs).unwrap_value();
    let rhs_length = state.read_pointer(rhs).unwrap_value();
    let result_length = lhs_length + rhs_length;
    let pointer = state.heap.allocate(result_length + WORD_SIZE_IN_BYTES);
    if pointer == 0 {
        runtime_error!(
            state,
            no_heap_memory_left(instruction.span, result_length + WORD_SIZE_IN_BYTES)
        );
    } else {
        let mut writing_pointer = pointer;
        state.heap.write_word(writing_pointer as _, result_length);
        writing_pointer += WORD_SIZE_IN_BYTES;
        for i in 0..lhs_length {
            let address = lhs + i + WORD_SIZE_IN_BYTES;
            let address = address as usize;
            let lhs_byte = {
                let addr = address as u64 & !(WORD_SIZE_IN_BYTES - 1);
                let word = state.read_pointer(addr).unwrap_value();
                let bytes = word.to_be_bytes();
                bytes[address % WORD_SIZE_IN_BYTES as usize]
            };
            state.heap.write_byte(writing_pointer as _, lhs_byte);
            writing_pointer += 1;
        }
        for i in 0..rhs_length {
            let address = rhs + i + WORD_SIZE_IN_BYTES;
            let address = address as usize;
            let rhs_byte = {
                let addr = address as u64 & !(WORD_SIZE_IN_BYTES - 1);
                let word = state.read_pointer(addr).unwrap_value();
                let bytes = word.to_be_bytes();
                bytes[address % WORD_SIZE_IN_BYTES as usize]
            };
            state.heap.write_byte(writing_pointer as _, rhs_byte);
            writing_pointer += 1;
        }
    }
    state.stack.push_pointer(pointer);
}

fn evaluate_pointer_addition(state: &mut EvaluatorState, instruction: Instruction) {
    let lhs = state.stack.pop().unwrap_pointer();
    let rhs = instruction.arg as i64;
    let result = if rhs < 0 {
        lhs - (-rhs) as u64
    } else {
        lhs + rhs as u64
    };
    state.stack.push_pointer(result);
}

fn evaluate_noneable_or_value(state: &mut EvaluatorState, instruction: Instruction) {
    let rhs = state.stack.pop();
    let lhs = state.stack.pop();
    let result = if lhs.unwrap_pointer() == 0 {
        rhs
    } else {
        lhs
    };
    let result = if instruction.arg != 0 {
        state.read_pointer(result.unwrap_pointer())
    } else {
        result
    };
    state.stack.push_flagged_word(result);
}

fn evaluate_jump(state: &mut EvaluatorState, instruction: Instruction) {
    state.pc = instruction.arg as _;
}

fn evaluate_jump_if_false(state: &mut EvaluatorState, instruction: Instruction) {
    let condition = state.stack.pop().value;
    if condition == 0 {
        state.pc = instruction.arg as _;
    }
}

fn evaluate_jump_if_true(state: &mut EvaluatorState, instruction: Instruction) {
    let condition = state.stack.pop().value;
    if condition != 0 {
        state.pc = instruction.arg as _;
    }
}

fn evaluate_sys_call(state: &mut EvaluatorState, instruction: Instruction) {
    let sys_call_kind = SystemCallKind::try_from_primitive(instruction.arg as u8).unwrap();
    let argument_count = state.stack.pop().unwrap_value() as usize;
    let argument_count = argument_count / 2;
    let mut arguments = Vec::with_capacity(argument_count);
    let mut types = Vec::with_capacity(argument_count);

    for _ in 0..argument_count {
        let type_ = state.stack.pop().unwrap_value();
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
        SystemCallKind::DebugHeapDump => sys_calls::heap_dump(arguments[0], state),
    }
}

fn evaluate_function_call(state: &mut EvaluatorState, _: Instruction) {
    let argument_count = state.stack.pop().unwrap_value();
    let base = state.stack.pop().unwrap_pointer();
    let return_address = if state.is_main_call {
        state.is_main_call = false;
        usize::MAX - 1
    } else {
        state.pc
    };
    let mut argument_values = vec![];
    for _ in 0..argument_count {
        argument_values.push(state.stack.pop());
    }
    state.stack.push(return_address as _);

    for &register in state.registers.iter().skip(state.protected_registers) {
        state.stack.push_flagged_word(register);
    }

    for v in argument_values.into_iter().rev() {
        state.stack.push_flagged_word(v);
    }
    state.pc = base as _;
}

fn evaluate_return(state: &mut EvaluatorState, instruction: Instruction) {
    let has_return_value = instruction.arg & 0x1 == 0x1;
    if has_return_value {
        let result = state.stack.pop();

        for register in state
            .registers
            .iter_mut()
            .skip(state.protected_registers)
            .rev()
        {
            if instruction.arg & 0x2 == 0x2 {
                state.stack.pop();
            } else {
                *register = state.stack.pop();
            }
        }

        let return_address = state.stack.pop().unwrap_value();
        state.pc = return_address as _;
        state.stack.push_flagged_word(result);
    } else {
        for register in state
            .registers
            .iter_mut()
            .skip(state.protected_registers)
            .rev()
        {
            if instruction.arg & 0x2 == 0x2 {
                state.stack.pop();
            } else {
                *register = state.stack.pop();
            }
        }

        let return_address = state.stack.pop().unwrap_value();
        state.pc = return_address as _;
    }
    // TODO: DEBUG
    state.garbage_collect();
}

fn evaluate_decode_closure(state: &mut EvaluatorState, instruction: Instruction) {
    let has_function_pointer = instruction.arg & 1 == 1;
    let argument_count = instruction.arg >> 1;
    let mut closure_pointer = state.stack.pop().unwrap_pointer();

    let closure_length_in_bytes = state.read_pointer(closure_pointer).unwrap_value();
    let argument_count = memory::bytes_to_word(closure_length_in_bytes) - 1 + argument_count;
    closure_pointer += WORD_SIZE_IN_BYTES;
    let end_address = closure_pointer + closure_length_in_bytes;

    let function_pointer = if has_function_pointer {
        closure_pointer += WORD_SIZE_IN_BYTES;
        Some(state.read_pointer(closure_pointer - WORD_SIZE_IN_BYTES).unwrap_pointer())
    } else {
        None
    };

    while closure_pointer < end_address {
        let argument = state.read_pointer(closure_pointer);
        state.stack.push_flagged_word(argument);
        closure_pointer += WORD_SIZE_IN_BYTES;
    }
    if let Some(function_pointer) = function_pointer {
        state.stack.push_pointer(function_pointer);
    }
    state.stack.push(argument_count);
}

fn evaluate_check_array_bounds(state: &mut EvaluatorState, instruction: Instruction) {
    let index = state.stack.pop().unwrap_value();
    let array = state.stack.pop().unwrap_pointer();
    let array_length_in_bytes = state.read_pointer(array).unwrap_value();
    if index - WORD_SIZE_IN_BYTES >= array_length_in_bytes {
        let array_length_in_words = memory::bytes_to_word(array_length_in_bytes);
        let index = (index - WORD_SIZE_IN_BYTES) as _;
        runtime_error!(state, index_out_of_bounds(instruction.span, index, array_length_in_words));
    }
    state.stack.push_pointer(array);
    state.stack.push(index);
}
