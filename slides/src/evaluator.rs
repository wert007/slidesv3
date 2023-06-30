mod debugger;
pub mod memory;
mod sys_calls;

use std::borrow::Cow;

use crate::{
    binder::typing::SystemCallKind,
    evaluator::memory::WORD_SIZE_IN_BYTES,
    instruction_converter::{
        instruction::{op_codes::OpCode, Instruction},
        Program,
    },
    text::{SourceTextId, TextLocation},
    value::Value,
    DiagnosticBag, Project,
};
use num_enum::TryFromPrimitive;

use self::memory::{
    allocator::{
        garbage_collector::{garbage_collect, GarbageCollectStats},
        Allocator,
    },
    stack::Stack,
    FlaggedByte, FlaggedWord, Memory,
};

macro_rules! runtime_error {
    ($evaluator:ident, $($fn_call:tt)*) => {
        $evaluator.runtime_diagnostics.$($fn_call)*;
        $evaluator.runtime_diagnostics.clone().flush_to_console(&$evaluator.project.source_text_collection);
        $evaluator.runtime_diagnostics.diagnostics.clear();
        $evaluator.runtime_error_happened = true;
    };
}

type ResultType = Value;

#[derive(Debug, Clone, Copy)]
enum InstructionErrorKind {
    ExpectedPointer,
    ExpectedValue,
    OutOfBounds,
}

#[derive(Debug, Clone, Copy)]
struct InstructionError {
    value: u64,
    kind: InstructionErrorKind,
    line: u32,
    instruction_location: TextLocation,
    pc: usize,
}

impl InstructionError {
    pub fn pointer(value: u64, line: u32, instruction_location: TextLocation, pc: usize) -> Self {
        Self {
            value,
            kind: InstructionErrorKind::ExpectedValue,
            line,
            instruction_location,
            pc,
        }
    }

    pub fn value(value: u64, line: u32, instruction_location: TextLocation, pc: usize) -> Self {
        Self {
            value,
            kind: InstructionErrorKind::ExpectedPointer,

            line,
            instruction_location,
            pc,
        }
    }

    fn display(&self, state: &EvaluatorState) {
        let expected = match self.kind {
            InstructionErrorKind::ExpectedPointer => "pointer",
            InstructionErrorKind::ExpectedValue => "value",
            InstructionErrorKind::OutOfBounds => "valid address",
        };
        let actual = match self.kind {
            InstructionErrorKind::ExpectedPointer => {
                if (self.value as i64) < 0 {
                    format!("{} ({}) {:#x}", self.value, self.value as i64, self.value)
                } else {
                    format!("{} {:#x}", self.value, self.value)
                }
            }
            InstructionErrorKind::ExpectedValue => format!("#{:X}", self.value),
            InstructionErrorKind::OutOfBounds => {
                format!("out of bounds access (#{:X})", self.value)
            }
        };
        println!(
            "Error in line {}: Expected {expected} but found {actual} while executing Instruction {:X}: {} from {}.",
            self.line,
            self.pc,
            crate::debug::commented_instruction_to_string(state.instructions[self.pc], state).unwrap(),
            &state.project.source_text_collection[self.instruction_location]
        );
    }

    fn out_of_bounds(value: u64, line: u32, location: TextLocation, pc: usize) -> InstructionError {
        Self {
            value,
            kind: InstructionErrorKind::OutOfBounds,
            line,
            instruction_location: location,
            pc,
        }
    }
}

pub struct EvaluatorState {
    stack: Stack,
    static_memory_size_in_words: usize,
    heap: Allocator,
    registers: Vec<FlaggedWord>,
    protected_registers: usize,
    pc: usize,
    instructions: Vec<Instruction>,
    is_main_call: bool,
    runtime_diagnostics: DiagnosticBag,
    runtime_error_happened: bool,
    debugger_state: debugger::DebuggerState,
    protected_pointers: Vec<u64>,
    pub project: Project,
    stack_trace: Vec<TextLocation>,
    last_visible_source_line: (SourceTextId, usize),
}

impl EvaluatorState {
    fn set_variable(&mut self, variable: u64, value: FlaggedWord) {
        let variable = variable as usize;
        self.registers[variable] = value;
    }

    fn read_pointer_word(&self, address: u64) -> Cow<FlaggedWord> {
        if memory::is_heap_pointer(address) {
            self.heap.read_flagged_word(address)
        } else {
            self.stack.read_flagged_word(address)
        }
    }

    pub fn read_pointer_word_safe(&self, address: u64) -> Result<&FlaggedWord, u64> {
        if memory::is_heap_pointer(address) {
            self.heap.read_flagged_word_safe(address)
        } else {
            self.stack.read_flagged_word_safe(address)
        }
        .ok_or(address)
    }

    pub fn read_pointer_byte_safe(&self, address: u64) -> Option<FlaggedByte> {
        if memory::is_heap_pointer(address) {
            self.heap.read_flagged_byte_safe(address)
        } else {
            self.stack.read_flagged_byte_safe(address)
        }
    }

    fn write_pointer_byte(&mut self, address: u64, value: FlaggedByte) {
        if memory::is_heap_pointer(address) {
            self.heap.write_flagged_byte(address, value);
        } else {
            self.stack.write_flagged_byte(address, value);
        }
    }

    fn write_pointer_word(&mut self, address: u64, value: FlaggedWord) {
        if memory::is_heap_pointer(address) {
            self.heap.write_flagged_word(address, value);
        } else {
            self.stack.write_flagged_word(address, value);
        }
    }

    fn reallocate(&mut self, old_address: u64, expected_size_in_bytes: u64) -> u64 {
        let result = self.heap.reallocate(old_address, expected_size_in_bytes);
        if result == 0 {
            self.garbage_collect();
            self.heap.reallocate(old_address, expected_size_in_bytes)
        } else {
            result
        }
    }

    fn garbage_collect(&mut self) -> GarbageCollectStats {
        let mut unchecked_pointers = self.protected_pointers.clone();
        self.protected_pointers = vec![];
        for word in &self.stack.words {
            if word.flags.is_pointer {
                unchecked_pointers.push(word.value);
            }
        }
        for value in &self.registers {
            if value.is_pointer() {
                unchecked_pointers.push(value.unwrap_pointer());
            }
        }
        garbage_collect(unchecked_pointers, &mut self.heap)
    }

    fn protect_pointer(&mut self, pointer: u64) {
        self.protected_pointers.push(pointer);
    }

    fn maybe_print_source_code_line(&mut self) {
        if !self.project.debug_flags.print_lines {
            return;
        }
        let location = self.instructions[self.pc].location;
        let position = (
            location.source_text,
            location.line_index(&self.project.source_text_collection),
        );
        if self.last_visible_source_line == position {
            return;
        }
        self.last_visible_source_line = position;
        let line = self.project.source_text_collection[self.last_visible_source_line.0]
            .line(self.last_visible_source_line.1)
            .trim();
        println!("{:4}: {line}", position.1 + 1);
    }

    pub(crate) fn read_string_from_memory(&self, address: usize) -> Option<String> {
        let address = address as u64;
        if address % memory::WORD_SIZE_IN_BYTES != 0 || address == 0 {
            return None;
        }
        let length_in_bytes = self.read_pointer_word_safe(address).ok()?.as_value().ok()?;
        let length_in_words = memory::bytes_to_word(length_in_bytes);
        let mut buffer = Vec::with_capacity(length_in_bytes as _);
        for w in 0..length_in_words {
            let Ok(word) = self.read_pointer_word_safe(address + (w + 1) * memory::WORD_SIZE_IN_BYTES).ok()?.as_value() else {
                break;
            };
            let bytes = word.to_le_bytes();
            buffer.extend_from_slice(&bytes);
        }
        while let Some(0) = buffer.last() {
            buffer.pop();
        }
        String::from_utf8(buffer).ok()
    }

    pub(crate) fn read_symbol_from_memory(&self, _: usize) -> Option<String> {
        // TODO: Implement
        None
    }

    pub fn load_register(&self, index: usize) -> Option<&FlaggedWord> {
        self.registers.get(index)
    }

    pub fn peek_stack(&self, offset: usize) -> Option<&FlaggedWord> {
        self.stack.read_flagged_word_safe(
            (self.stack.len() as u64 - offset as u64 - 1) * WORD_SIZE_IN_BYTES,
        )
    }

    pub(crate) fn next_allocation_address(&self, size_in_bytes: u64) -> Option<u64> {
        let value = self.heap.dry_reallocate(0, size_in_bytes);
        if value == 0 {
            None
        } else {
            Some(value)
        }
    }
}

impl std::fmt::Debug for EvaluatorState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EvaluatorState")
            .field("stack", &self.stack)
            .field(
                "static_memory_size_in_words",
                &self.static_memory_size_in_words,
            )
            .field("registers", &self.registers)
            .field("protected_registers", &self.protected_registers)
            .field("pc", &self.pc)
            .field("is_main_call", &self.is_main_call)
            .field("runtime_diagnostics", &self.runtime_diagnostics)
            .field("runtime_error_happened", &self.runtime_error_happened)
            .field("debugger_state", &self.debugger_state)
            .finish()
    }
}

pub fn evaluate(
    program: Program,
    _source_text: crate::text::SourceTextId,
    project: Project,
) -> ResultType {
    let debug_flags = project.debug_flags;
    let mut stack = Stack::new(debug_flags);
    stack.push_static_memory(program.static_memory);
    let mut state = EvaluatorState {
        static_memory_size_in_words: stack.len(),
        stack,
        // heap: Allocator::new(256, debug_flags),
        heap: Allocator::new(64 * 1024, debug_flags),
        registers: (0..program.max_used_variables)
            .into_iter()
            .map(|i| FlaggedWord::default().with_comment(format!("Register #{i}")))
            .collect(),
        protected_registers: program.protected_variables,
        pc: 0,
        stack_trace: Vec::new(),
        instructions: program.instructions,
        is_main_call: true,
        runtime_diagnostics: DiagnosticBag::new(),
        runtime_error_happened: false,
        debugger_state: debugger::DebuggerState::default(),
        protected_pointers: vec![],
        project,
        last_visible_source_line: (unsafe { SourceTextId::from_raw(0) }, 0),
    };
    match execute_function(&mut state, program.entry_point, &[]) {
        Ok(Some(exit_code)) => (exit_code.unwrap_value() as i64).into(),
        Ok(None) => Value::Integer(0),
        Err(()) => {
            if debug_flags.use_debugger {
                debugger::create_session(&mut state);
            }
            Value::Integer(-1)
        }
    }
}

fn execute_function(
    state: &mut EvaluatorState,
    entry_point: usize,
    arguments: &[FlaggedWord],
) -> Result<Option<FlaggedWord>, ()> {
    let old_pc = state.pc;
    let old_registers = state.registers.clone();
    let old_protected_registers = state.protected_registers;
    state.pc = entry_point;
    state.runtime_error_happened = false;
    let min_stack_count = state.stack.len();
    for argument in arguments {
        state.stack.push_flagged_word(
            argument
                .clone()
                .replace_comment(|c| format!("{c} as argument")),
        );
    }
    let mut nestedness = 0;
    let mut debug_nestedness = 0;
    let mut stack_sizes = vec![state.stack.len()];
    while state.pc < state.instructions.len() {
        let pc = state.pc;
        state.maybe_print_source_code_line();
        if state.project.debug_flags.print_instructions {
            println!(
                "  CI {:X}*{}: {}",
                pc,
                nestedness,
                crate::debug::commented_instruction_to_string(state.instructions[pc], &state)
                    .unwrap()
            );
        }
        if state.project.debug_flags.slow_mode {
            std::thread::sleep(std::time::Duration::from_millis(500));
        }
        match state.instructions[pc].op_code {
            OpCode::FunctionCall => {
                stack_sizes.push(state.stack.len());
                nestedness += 1;
            }
            OpCode::Return => {
                let expected = stack_sizes.pop().expect("nestedness must have overflown");
                if state.stack.len() != expected {
                    // TODO: Make this actually work maybe?
                    // println!(
                    //     "Expected stack to have at least {expected} elements, but found {} instead",
                    //     state.stack.len()
                    // );
                    // debugger::create_session(state);
                }
                nestedness -= 1;
            }
            _ => {}
        }
        if nestedness < 0 {
            break;
        }
        let instruction_failed = execute_instruction(state, state.instructions[pc]).err();
        match state.debugger_state.session_state {
            debugger::SessionState::Continue => {
                debugger::create_session(state);
                if state.debugger_state.skip_function() {
                    debug_nestedness = nestedness;
                }
            }
            debugger::SessionState::SkipFunction => {
                if nestedness == debug_nestedness {
                    debugger::create_session(state);
                    if state.debugger_state.skip_function() {
                        debug_nestedness = nestedness;
                    }
                }
            }
            debugger::SessionState::Quit => {}
        }
        if state.runtime_error_happened {
            println!("Unusual termination.");
            return Err(());
        }
        if let Some(err) = instruction_failed {
            err.display(state);
            return Err(());
        }
        assert!(
            state.stack.len() >= min_stack_count,
            "{} >= {}",
            state.stack.len(),
            min_stack_count
        );
        state.pc += 1;
    }
    match state.debugger_state.session_state {
        debugger::SessionState::Continue => {
            debugger::create_session(state);
        }
        debugger::SessionState::SkipFunction => {
            if nestedness == debug_nestedness {
                debugger::create_session(state);
            }
        }
        debugger::SessionState::Quit => {}
    }
    state.pc = old_pc;
    state.registers = old_registers;
    state.protected_registers = old_protected_registers;
    Ok(
        if !state.runtime_error_happened && state.stack.len() > min_stack_count {
            Some(state.stack.pop())
        } else {
            None
        },
    )
}

#[must_use]
fn execute_instruction(
    state: &mut EvaluatorState,
    instruction: Instruction,
) -> Result<(), InstructionError> {
    match instruction.op_code {
        OpCode::Label | OpCode::Unknown => unreachable!(),
        OpCode::NoOp => {}
        OpCode::Breakpoint => evaluate_breakpoint(state, instruction),
        OpCode::LoadImmediate => evaluate_load_immediate(state, instruction),
        OpCode::LoadPointer => evaluate_load_pointer(state, instruction),
        OpCode::DuplicateOver => evaluate_duplicate_over(state, instruction),
        OpCode::Pop => evaluate_pop(state, instruction),
        OpCode::LoadRegister => evaluate_load_register(state, instruction),
        OpCode::StoreInRegister => evaluate_assign_to_variable(state, instruction),
        OpCode::StoreByteInMemory => evaluate_store_byte_in_memory(state, instruction),
        OpCode::StoreWordInMemory => evaluate_store_word_in_memory(state, instruction),
        OpCode::WriteToStack => evaluate_write_to_stack(state, instruction),
        OpCode::WriteToHeap => evaluate_write_to_heap(state, instruction),
        OpCode::Allocate => evaluate_allocate(state, instruction),
        OpCode::ReadWordWithOffset => evaluate_read_word_with_offset(state, instruction)?,
        OpCode::ReadByteWithOffset => evaluate_read_byte_with_offset(state, instruction)?,
        OpCode::MemoryCopy => evaluate_memory_copy(state, instruction),
        OpCode::TypeIdentifier => evaluate_load_immediate(state, instruction),
        OpCode::Rotate => evaluate_rotate(state, instruction),
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
        OpCode::LessThan => evaluate_less_than(state, instruction)?,
        OpCode::GreaterThan => evaluate_greater_than(state, instruction),
        OpCode::LessThanEquals => evaluate_less_than_equals(state, instruction),
        OpCode::GreaterThanEquals => evaluate_greater_than_equals(state, instruction),
        OpCode::StringConcat => evaluate_string_concat(state, instruction)?,
        OpCode::NoneableOrValue => evaluate_noneable_or_value(state, instruction)?,
        OpCode::Jump => evaluate_jump(state, instruction),
        OpCode::JumpDynamically => evaluate_jump_dynamically(state, instruction),
        OpCode::JumpIfFalse => evaluate_jump_if_false(state, instruction),
        OpCode::JumpIfTrue => evaluate_jump_if_true(state, instruction),
        OpCode::SysCall => evaluate_sys_call(state, instruction),
        OpCode::FunctionCall => evaluate_function_call(state, instruction),
        OpCode::Return => evaluate_return(state, instruction),
        OpCode::DecodeClosure => evaluate_decode_closure(state, instruction),
    }
    Ok(())
}

fn evaluate_breakpoint(state: &mut EvaluatorState, _: Instruction) {
    state.debugger_state.session_state = debugger::SessionState::Continue;
}

fn evaluate_load_immediate(state: &mut EvaluatorState, instruction: Instruction) {
    state
        .stack
        .push_flagged_word(FlaggedWord::value(instruction.arg).with_comment(format!(
            "Immediate from line {}",
            state.project.source_text_collection[instruction.location.source_text]
                .line_index(instruction.location.span.start()) + 1
        )));
}

fn evaluate_load_pointer(state: &mut EvaluatorState, instruction: Instruction) {
    state
        .stack
        .push_flagged_word(FlaggedWord::pointer(instruction.arg).with_comment(format!(
            "Pointer immediate from line {}",
            state.project.source_text_collection[instruction.location.source_text]
                .line_index(instruction.location.span.start()) + 1
        )));
}

fn evaluate_duplicate_over(state: &mut EvaluatorState, instruction: Instruction) {
    let value = state
        .stack
        .read_flagged_word((state.stack.len() as u64 - instruction.arg - 1) * WORD_SIZE_IN_BYTES);
    state.stack.push_flagged_word(value.into_owned());
}

fn evaluate_rotate(state: &mut EvaluatorState, instruction: Instruction) {
    let value = state.stack.remove_at_offset(instruction.arg);
    state.stack.push_flagged_word(value);
}

fn evaluate_pop(state: &mut EvaluatorState, _: Instruction) {
    state.stack.pop();
}

fn evaluate_load_register(state: &mut EvaluatorState, instruction: Instruction) {
    let value = state.registers[instruction.arg as usize].clone();
    let value = value.replace_comment(|c| format!("{c} loaded from register"));
    state.stack.push_flagged_word(value);
}

fn evaluate_assign_to_variable(state: &mut EvaluatorState, instruction: Instruction) {
    let value = state.stack.pop();
    state.set_variable(instruction.arg, value);
}

fn evaluate_store_byte_in_memory(state: &mut EvaluatorState, _: Instruction) {
    let address = state.stack.pop().unwrap_pointer();
    let value = state.stack.pop();
    let value = value.into_flagged_byte().unwrap();
    state.write_pointer_byte(address, value);
}

fn evaluate_store_word_in_memory(state: &mut EvaluatorState, _: Instruction) {
    let address = state.stack.pop().unwrap_pointer();
    let value = state.stack.pop();
    state.write_pointer_word(address, value);
}

fn evaluate_write_to_stack(state: &mut EvaluatorState, instruction: Instruction) {
    let address = instruction.arg;
    let value = state.stack.pop();
    state.stack.write_flagged_word(address as _, value);
}

fn evaluate_write_to_heap(state: &mut EvaluatorState, instruction: Instruction) {
    let size_in_words = if instruction.arg == 0 {
        state.stack.pop().unwrap_value()
    } else {
        instruction.arg
    };
    let size_in_bytes = size_in_words * WORD_SIZE_IN_BYTES;
    let address = state.reallocate(0, size_in_bytes);
    if address == 0 {
        runtime_error!(
            state,
            no_heap_memory_left(instruction.location, size_in_bytes)
        );
    } else {
        let mut writing_pointer = address;
        for _ in 0..size_in_words {
            let value = state.stack.pop();
            state.heap.write_flagged_word(writing_pointer as _, value);
            writing_pointer += WORD_SIZE_IN_BYTES;
        }
    }
    state.stack.push_flagged_word(
        FlaggedWord::pointer(address).with_comment(format!("Allocated {size_in_bytes} bytes")),
    );
}

fn evaluate_allocate(state: &mut EvaluatorState, instruction: Instruction) {
    let size_in_bytes = instruction.arg;
    let address = state.reallocate(0, size_in_bytes);
    if address == 0 {
        runtime_error!(
            state,
            no_heap_memory_left(instruction.location, size_in_bytes)
        );
    }
    state.stack.push_flagged_word(
        FlaggedWord::pointer(address).with_comment(format!("User allocated {size_in_bytes} bytes")),
    );
}

fn evaluate_read_byte_with_offset(
    state: &mut EvaluatorState,
    instruction: Instruction,
) -> Result<(), InstructionError> {
    let address = state
        .stack
        .pop()
        .as_pointer()
        .map_err(|err| InstructionError::value(err, line!(), instruction.location, state.pc))?;
    let offset = instruction.arg;
    let address = address + offset;
    let value = state
        .read_pointer_byte_safe(address)
        .ok_or(InstructionError::value(
            0,
            line!(),
            instruction.location,
            state.pc,
        ))?;
    state.stack.push_flagged_word(value.into());
    Ok(())
}

fn evaluate_read_word_with_offset(
    state: &mut EvaluatorState,
    instruction: Instruction,
) -> Result<(), InstructionError> {
    let address = state
        .stack
        .pop()
        .as_pointer()
        .map_err(|err| InstructionError::value(err, line!(), instruction.location, state.pc))?;
    let offset = instruction.arg;
    let address = address + offset;
    let value = state.read_pointer_word(address);
    state.stack.push_flagged_word(value.into_owned());
    Ok(())
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
        let buffer = state.read_pointer_word(src);
        state.write_pointer_word(dest, buffer.into_owned());
    }
}

fn evaluate_bitwise_twos_complement(state: &mut EvaluatorState, _: Instruction) {
    let mut value = state.stack.pop();
    value.value = -(value.unwrap_value() as i64) as u64;
    state.stack.push_flagged_word(value);
}

fn evaluate_bitwise_xor(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop().unwrap_value();
    let lhs = state.stack.pop().unwrap_value();
    state.stack.push(lhs ^ rhs);
}

fn evaluate_bitwise_nxor(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop();
    let lhs = state.stack.pop();
    state.stack.push_flagged_word(
        FlaggedWord::value(!(lhs.unwrap_value() ^ rhs.unwrap_value()))
            .with_comment(format!("({}) NXOR ({})", lhs.comment, rhs.comment)),
    );
}

fn evaluate_addition(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop();
    let lhs = state.stack.pop();
    let result = FlaggedWord {
        value: lhs.value.wrapping_add(rhs.value),
        flags: lhs.flags,
        comment: format!("({}) + ({})", lhs.comment, rhs.comment),
    };
    state.stack.push_flagged_word(result);
}

fn evaluate_subtraction(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop();
    let lhs = state.stack.pop();
    state.stack.push_flagged_word(
        FlaggedWord::value(lhs.unwrap_value().wrapping_sub(rhs.unwrap_value()))
            .with_comment(format!("({}) - ({})", lhs.comment, rhs.comment)),
    );
}

fn evaluate_multiplication(state: &mut EvaluatorState, _: Instruction) {
    let rhs = state.stack.pop();
    let lhs = state.stack.pop();
    state.stack.push_flagged_word(
        FlaggedWord::value(lhs.unwrap_value().wrapping_mul(rhs.unwrap_value()))
            .with_comment(format!("({}) - ({})", lhs.comment, rhs.comment)),
    );
}

fn evaluate_division(state: &mut EvaluatorState, instruction: Instruction) {
    let rhs = state.stack.pop();
    let lhs = state.stack.pop();
    state.stack.push_flagged_word(if rhs.unwrap_value() == 0 {
        runtime_error!(state, division_by_zero(instruction.location));
        FlaggedWord::value(0).with_comment("Division by zero error!")
    } else {
        FlaggedWord::value(lhs.unwrap_value().wrapping_div(rhs.unwrap_value()))
            .with_comment(format!("({}) / ({})", lhs.comment, rhs.comment))
    });
}

fn evaluate_equals(state: &mut EvaluatorState, _: Instruction) {
    // FIXME: As soon as their are generic types for many things, you could
    // overwrite $equals, and this will only need to compare primitive value, so
    // that you can use unwrap_value() again.
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
    // unimplemented!("Arrays have a $equals function! but strings might not!");
    let rhs = state.stack.pop().unwrap_pointer();
    let lhs = state.stack.pop().unwrap_pointer();

    let lhs_length_in_bytes = state.read_pointer_word(lhs).unwrap_value();
    let lhs_length_in_words = memory::bytes_to_word(lhs_length_in_bytes);
    let rhs_length_in_bytes = state.read_pointer_word(rhs).unwrap_value();
    let _rhs_length_in_words = memory::bytes_to_word(rhs_length_in_bytes);
    // If two arrays are not equal in length, we don't compare their elements.
    // But when we compare their elements we expect a true result and only
    // change it if its false.
    let mut result = lhs_length_in_bytes == rhs_length_in_bytes;
    if lhs != rhs && lhs_length_in_bytes == rhs_length_in_bytes {
        for i in (0..lhs_length_in_words * WORD_SIZE_IN_BYTES).step_by(WORD_SIZE_IN_BYTES as _) {
            let lhs_index = lhs + i + WORD_SIZE_IN_BYTES;
            let rhs_index = rhs + i + WORD_SIZE_IN_BYTES;
            let lhs = state.read_pointer_word(lhs_index).unwrap_value();
            let rhs = state.read_pointer_word(rhs_index).unwrap_value();
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

#[must_use]
fn evaluate_less_than(
    state: &mut EvaluatorState,
    instruction: Instruction,
) -> Result<(), InstructionError> {
    match instruction.arg {
        0 => {
            let rhs = state.stack.pop().as_value().map_err(|err| {
                InstructionError::pointer(err, line!(), instruction.location, state.pc)
            })? as i64;
            let lhs = state.stack.pop().as_value().map_err(|err| {
                InstructionError::pointer(err, line!(), instruction.location, state.pc)
            })? as i64;
            state.stack.push((lhs < rhs) as _);
        }
        1 => {
            let rhs = state.stack.pop().as_value().map_err(|err| {
                InstructionError::pointer(err, line!(), instruction.location, state.pc)
            })?;
            let lhs = state.stack.pop().as_value().map_err(|err| {
                InstructionError::pointer(err, line!(), instruction.location, state.pc)
            })?;
            state.stack.push((lhs < rhs) as _);
        }
        arg => {
            panic!("unhandled argument {arg:X} to instruction!")
        }
    }
    Ok(())
}

fn evaluate_greater_than(state: &mut EvaluatorState, instruction: Instruction) {
    match instruction.arg {
        0 => {
            let rhs = state.stack.pop().unwrap_value() as i64;
            let lhs = state.stack.pop().unwrap_value() as i64;
            state.stack.push((lhs > rhs) as _);
        }
        1 => {
            let rhs = state.stack.pop().unwrap_value();
            let lhs = state.stack.pop().unwrap_value();
            state.stack.push((lhs > rhs) as _);
        }
        arg => {
            panic!("unhandled argument {arg:X} to instruction!")
        }
    }
}

fn evaluate_less_than_equals(state: &mut EvaluatorState, instruction: Instruction) {
    match instruction.arg {
        0 => {
            let rhs = state.stack.pop().unwrap_value() as i64;
            let lhs = state.stack.pop().unwrap_value() as i64;
            state.stack.push((lhs <= rhs) as _);
        }
        1 => {
            let rhs = state.stack.pop().unwrap_value();
            let lhs = state.stack.pop().unwrap_value();
            state.stack.push((lhs <= rhs) as _);
        }
        arg => {
            panic!("unhandled argument {arg:X} to instruction!")
        }
    }
}

fn evaluate_greater_than_equals(state: &mut EvaluatorState, instruction: Instruction) {
    match instruction.arg {
        0 => {
            let rhs = state.stack.pop().unwrap_value() as i64;
            let lhs = state.stack.pop().unwrap_value() as i64;
            state.stack.push((lhs >= rhs) as _);
        }
        1 => {
            let rhs = state.stack.pop().unwrap_value();
            let lhs = state.stack.pop().unwrap_value();
            state.stack.push((lhs >= rhs) as _);
        }
        arg => {
            panic!("unhandled argument {arg:X} to instruction!")
        }
    }
}

fn evaluate_string_concat(
    state: &mut EvaluatorState,
    instruction: Instruction,
) -> Result<(), InstructionError> {
    let rhs = state
        .stack
        .pop()
        .as_pointer()
        .map_err(|err| InstructionError::value(err, line!(), instruction.location, state.pc))?;
    let lhs = state
        .stack
        .pop()
        .as_pointer()
        .map_err(|err| InstructionError::value(err, line!(), instruction.location, state.pc))?;

    let lhs_length = state
        .read_pointer_word(lhs)
        .as_value()
        .map_err(|err| InstructionError::pointer(err, line!(), instruction.location, state.pc))?;
    let rhs_length = state
        .read_pointer_word(rhs)
        .as_value()
        .map_err(|err| InstructionError::pointer(err, line!(), instruction.location, state.pc))?;
    let result_length = lhs_length + rhs_length;
    // This protects these strings on the heap, since reallocate may call
    // garbage_collect, which otherwise would overwrite the string with itself.
    state.protect_pointer(rhs);
    state.protect_pointer(lhs);
    let pointer = state.reallocate(0, result_length + WORD_SIZE_IN_BYTES);
    if pointer == 0 {
        runtime_error!(
            state,
            no_heap_memory_left(instruction.location, result_length + WORD_SIZE_IN_BYTES)
        );
    } else {
        let mut writing_pointer = pointer;
        state.heap.write_flagged_word(
            writing_pointer,
            FlaggedWord::value(result_length).with_comment("String length from string concat"),
        );
        writing_pointer += WORD_SIZE_IN_BYTES;
        for i in 0..lhs_length {
            let lhs_byte = {
                let address = lhs + i + WORD_SIZE_IN_BYTES;
                let addr = address & !(WORD_SIZE_IN_BYTES - 1);
                let address = address as usize;
                let word = state.read_pointer_word(addr).unwrap_value();
                let bytes = word.to_le_bytes();
                bytes[address % WORD_SIZE_IN_BYTES as usize]
            };
            state.heap.write_byte(writing_pointer, lhs_byte);
            writing_pointer += 1;
        }
        for i in 0..rhs_length {
            let address = rhs + i + WORD_SIZE_IN_BYTES;
            let address = address as usize;
            let rhs_byte = {
                let addr = address as u64 & !(WORD_SIZE_IN_BYTES - 1);
                let word = state.read_pointer_word(addr).unwrap_value();
                let bytes = word.to_le_bytes();
                bytes[address % WORD_SIZE_IN_BYTES as usize]
            };
            state.heap.write_byte(writing_pointer, rhs_byte);
            writing_pointer += 1;
        }
    }
    state.stack.push_pointer(pointer);
    Ok(())
}

fn evaluate_noneable_or_value(
    state: &mut EvaluatorState,
    instruction: Instruction,
) -> Result<(), InstructionError> {
    let rhs = state.stack.pop();
    let lhs = state.stack.pop();
    let rhs =
        state
            .read_pointer_word_safe(rhs.as_pointer().map_err(|err| {
                InstructionError::value(err, line!(), instruction.location, state.pc)
            })?)
            .map_err(|err| {
                InstructionError::out_of_bounds(err, line!(), instruction.location, state.pc)
            })?;
    let lhs =
        state
            .read_pointer_word_safe(lhs.as_pointer().map_err(|err| {
                InstructionError::value(err, line!(), instruction.location, state.pc)
            })?)
            .map_err(|err| {
                InstructionError::out_of_bounds(err, line!(), instruction.location, state.pc)
            })?;
    let is_none = lhs
        .as_pointer()
        .map_err(|err| InstructionError::value(err, line!(), instruction.location, state.pc))?
        == 0;
    let needs_dereferencing = instruction.arg != 0;
    let result = if needs_dereferencing && !is_none {
        state
            .read_pointer_word_safe(lhs.as_pointer().map_err(|err| {
                InstructionError::value(err, line!(), instruction.location, state.pc)
            })?)
            .map_err(|err| {
                InstructionError::out_of_bounds(err, line!(), instruction.location, state.pc)
            })?
    } else {
        if is_none {
            rhs
        } else {
            lhs
        }
    };
    state.stack.push_flagged_word(result.clone());
    Ok(())
}

fn evaluate_jump(state: &mut EvaluatorState, instruction: Instruction) {
    state.pc = instruction.arg as _;
}

fn evaluate_jump_dynamically(state: &mut EvaluatorState, _: Instruction) {
    state.pc = state.stack.pop().unwrap_pointer() as _;
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
    state.stack_trace.push(instruction.location);
    let sys_call_kind = SystemCallKind::try_from_primitive(instruction.arg as u8).unwrap();
    let argument_count = state.stack.pop().unwrap_value() as usize;
    let mut arguments = Vec::with_capacity(argument_count);

    for _ in 0..argument_count {
        arguments.push(state.stack.pop());
    }
    match sys_call_kind {
        SystemCallKind::Print => sys_calls::print(&arguments[0], state),
        SystemCallKind::ArrayLength => sys_calls::array_length(&arguments[0], state),
        SystemCallKind::ToString => sys_calls::to_string(&arguments[0], state),
        SystemCallKind::HeapDump => sys_calls::heap_dump(&arguments[0], state),
        SystemCallKind::Reallocate => sys_calls::reallocate(&arguments[1], &arguments[0], state),
        SystemCallKind::RuntimeError => sys_calls::runtime_error(&arguments[0], state),
        SystemCallKind::AddressOf => sys_calls::address_of(&arguments[0], state),
        SystemCallKind::GarbageCollect => {
            state.garbage_collect();
        }
        SystemCallKind::Break => unreachable!(),
        SystemCallKind::Hash => sys_calls::hash(&arguments[0], state),
        SystemCallKind::ByteToChar => sys_calls::byte_to_char(&arguments[0], state),
    }
    state.stack_trace.pop();
}

fn evaluate_function_call(state: &mut EvaluatorState, instruction: Instruction) {
    state.stack_trace.push(instruction.location);
    let print_stack_value = state.stack.pop_print_stack();
    let argument_count = state.stack.pop().unwrap_value();
    let base = state.stack.pop().unwrap_pointer();
    let return_address = if state.is_main_call {
        state.is_main_call = false;
        // We need to make sure, that this fake return program counter gets not
        // mistaken as pointer on the heap memory!
        ((u64::MAX - 1) & !memory::HEAP_POINTER) as usize
    } else {
        state.pc
    };
    let mut argument_values = vec![];
    for _ in 0..argument_count {
        argument_values.push(state.stack.pop());
    }
    state.stack.push_flagged_word(
        FlaggedWord::pointer(return_address as _).with_comment("Return address"),
    );

    for register in state.registers.iter().skip(state.protected_registers) {
        state
            .stack
            .push_flagged_word(register.clone().replace_comment(|c| format!("{c} saved")));
    }

    for v in argument_values.into_iter().rev() {
        state
            .stack
            .push_flagged_word(v.replace_comment(|c| format!("{c} as argument")));
    }
    state.stack.push_print_stack(print_stack_value);
    state.pc = base as _;
}

fn evaluate_return(state: &mut EvaluatorState, instruction: Instruction) {
    state.stack_trace.pop();
    let print_stack_value = state.stack.pop_print_stack();
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

        let return_address = state.stack.pop().unwrap_pointer();
        state.pc = return_address as _;
        state.stack.push_flagged_word(result);
    } else {
        for register in state
            .registers
            .iter_mut()
            .skip(state.protected_registers)
            .rev()
        {
            // Protect registers after main function returns, just in case we
            // want to check them later!
            if instruction.arg & 0x2 == 0x2 {
                state.stack.pop();
            } else {
                *register = state.stack.pop();
            }
        }

        let return_address = state.stack.pop().unwrap_pointer();
        state.pc = return_address as _;
    }
    state.stack.push_print_stack(print_stack_value);
}

fn evaluate_decode_closure(state: &mut EvaluatorState, instruction: Instruction) {
    let has_function_pointer = instruction.arg & 1 == 1;
    let argument_count = instruction.arg >> 1;
    let mut closure_pointer = state.stack.pop().unwrap_pointer();

    let closure_length_in_bytes = state.read_pointer_word(closure_pointer).unwrap_value();
    // let argument_count = memory::bytes_to_word(closure_length_in_bytes) - 1 + argument_count;
    closure_pointer += WORD_SIZE_IN_BYTES;
    let end_address = closure_pointer + closure_length_in_bytes;

    let function_pointer = if has_function_pointer {
        closure_pointer += WORD_SIZE_IN_BYTES;
        Some(
            state
                .read_pointer_word(closure_pointer - WORD_SIZE_IN_BYTES)
                .unwrap_pointer(),
        )
    } else {
        None
    };

    while closure_pointer < end_address {
        let argument = state.read_pointer_word(closure_pointer);
        state.stack.push_flagged_word(argument.into_owned());
        closure_pointer += WORD_SIZE_IN_BYTES;
    }
    if let Some(function_pointer) = function_pointer {
        state
            .stack
            .push_pointer_with_comment(function_pointer, "Function Pointer");
    }
    state
        .stack
        .push_with_comment(argument_count, "Argument count");
}
