mod allocator;
pub mod stack;
mod sys_calls;

use crate::{
    binder::typing::{SystemCallKind, Type},
    instruction_converter::{
        instruction::{op_codes::OpCode, Instruction},
        Program,
    },
    value::Value,
    DebugFlags, DiagnosticBag,
};
use num_enum::TryFromPrimitive;

use self::{allocator::Allocator, stack::Stack};

macro_rules! runtime_error {
    ($evaluator:ident, $($fn_call:tt)*) => {
        $evaluator.runtime_diagnostics.$($fn_call)*;
        $evaluator.runtime_diagnostics.clone().flush_to_console();
        $evaluator.runtime_diagnostics.diagnostics.clear();
    };
}

type ResultType = Value;

const HEAP_POINTER: u64 = 0x80_00_00_00_00_00_00_00;
pub const WORD_SIZE_IN_BYTES: u64 = 8;

pub const fn bytes_to_word(bytes: u64) -> u64 {
    (bytes + WORD_SIZE_IN_BYTES - 1) / WORD_SIZE_IN_BYTES
}

pub struct EvaluatorState<'a> {
    stack: Stack,
    heap: Allocator,
    registers: Vec<TypedU64>,
    protected_registers: usize,
    pc: usize,
    is_main_call: bool,
    runtime_diagnostics: DiagnosticBag<'a>,
}

impl EvaluatorState<'_> {
    fn set_variable(&mut self, variable: u64, value: u64, is_pointer: bool) {
        let variable = variable as usize;
        self.registers[variable] = TypedU64 { value, is_pointer };
    }

    fn is_pointer(&mut self, address: usize) -> bool {
        is_heap_pointer(address as _) || self.stack.is_pointer(address)
    }
}

pub fn evaluate(
    program: Program,
    source_text: &crate::text::SourceText<'_>,
    debug_flags: DebugFlags,
) -> ResultType {
    let mut state = EvaluatorState {
        stack: program.stack,
        heap: Allocator::new(1024, debug_flags),
        registers: vec![TypedU64::default(); program.max_used_variables],
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

fn is_heap_pointer(address: u64) -> bool {
    address & HEAP_POINTER > 0
}

fn evaluate_duplicate(state: &mut EvaluatorState, _: Instruction) {
    let value = state.stack.pop();
    if value.is_pointer {
        state.stack.push_pointer(value.value);
        state.stack.push_pointer(value.value);
    } else {
        state.stack.push(value.value);
        state.stack.push(value.value);
    }
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

fn evaluate_array_index(state: &mut EvaluatorState, instruction: Instruction) {
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
    let index_in_bytes = array as usize
        + index_in_words as usize * WORD_SIZE_IN_BYTES as usize
        + WORD_SIZE_IN_BYTES as usize;
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
    let index = array + index;
    let index = index as _;
    if is_heap_pointer(array) {
        state.heap.write_word(index, value);
    } else {
        state.stack.write_word(index, value);
    }
}

fn evaluate_write_to_stack(state: &mut EvaluatorState, instruction: Instruction) {
    let address = instruction.arg as _;
    let value = state.stack.pop();
    state.stack.write_word(address, value.value);
    if value.is_pointer {
        state.stack.set_pointer(address);
    }
}

fn evaluate_write_to_heap(state: &mut EvaluatorState, instruction: Instruction) {
    let size_in_bytes = instruction.arg * WORD_SIZE_IN_BYTES;
    let address = state.heap.allocate(size_in_bytes);
    let mut writing_pointer = address;
    for _ in 0..instruction.arg {
        let value = state.stack.pop();
        state.heap.write_word(writing_pointer as _, value.value);
        writing_pointer += WORD_SIZE_IN_BYTES;
    }
    state.stack.push_pointer(address);
}

fn evaluate_read_word_with_offset(state: &mut EvaluatorState, instruction: Instruction) {
    let address = state.stack.pop().value;
    let offset = instruction.arg;
    let address = address + offset;
    let value = if is_heap_pointer(address) {
        todo!();
        // TypedU64 {
        //     value: state.heap.read_word(address),
        //     is_pointer: false,
        // }
    } else {
        TypedU64 {
            value: state.stack.read_word(address as _),
            is_pointer: state.stack.is_pointer(address as _),
        }
    };
    if value.is_pointer {
        state.stack.push_pointer(value.value);
    } else {
        state.stack.push(value.value);
    }
}

fn evaluate_memory_copy(state: &mut EvaluatorState, instruction: Instruction) {
    let size_in_bytes = if instruction.arg == 0 {
        state.stack.pop().value
    } else {
        instruction.arg
    };
    // FIXME: Currently there are only complete words and no single bytes
    // supported!
    assert_eq!(size_in_bytes % WORD_SIZE_IN_BYTES, 0);
    let size_in_words = bytes_to_word(size_in_bytes);
    let dest = state.stack.pop();
    assert!(dest.is_pointer);
    let dest = dest.value;
    let src = state.stack.pop();
    assert!(src.is_pointer);
    let src = src.value;
    for word_index in 0..size_in_words {
        let src = src + word_index * WORD_SIZE_IN_BYTES;
        let src = src as usize;
        let dest = dest + word_index * WORD_SIZE_IN_BYTES;
        let dest = dest as usize;
        let buffer = if is_heap_pointer(src as _) {
            state.heap.read_word(src)
        } else {
            state.stack.read_word(src)
        };
        if is_heap_pointer(dest as _) {
            state.heap.write_word(dest, buffer);
        } else {
            state.stack.write_word(dest, buffer);
        }
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

fn evaluate_division(state: &mut EvaluatorState, instruction: Instruction) {
    let rhs = state.stack.pop().value;
    let lhs = state.stack.pop().value;
    if rhs == 0 {
        runtime_error!(state, division_by_zero(instruction.span));
        state.stack.push(0);
    } else {
        state.stack.push(lhs.wrapping_div(rhs));
    }
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
    let mut result = lhs_length_in_bytes == rhs_length_in_bytes;
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

fn evaluate_string_concat(state: &mut EvaluatorState, instruction: Instruction) {
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
        runtime_error!(
            state,
            no_heap_memory_left(instruction.span, result_length + WORD_SIZE_IN_BYTES)
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
                state.heap.read_byte(address)
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
                state.heap.read_byte(address)
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

fn evaluate_pointer_addition(state: &mut EvaluatorState, instruction: Instruction) {
    let lhs = state.stack.pop();
    assert!(lhs.is_pointer);
    let lhs = lhs.value;
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
    let result = if lhs.value == 0 {
        rhs
    } else {
        lhs
    };
    let result = if instruction.arg != 0 {
        let value = if is_heap_pointer(result.value) {
            state.heap.read_word(result.value as _)
        } else {
            state.stack.read_word(result.value as _)
        };
        TypedU64 {
            value,
            is_pointer: false, // Otherwise we would not need to dereference the pointer.
        }
    } else {
        result
    };
    if result.is_pointer {
        state.stack.push_pointer(result.value);
    } else {
        state.stack.push(result.value);
    }
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

#[derive(Clone, Copy, Debug, Default)]
pub struct TypedU64 {
    value: u64,
    is_pointer: bool,
}

fn evaluate_sys_call(state: &mut EvaluatorState, instruction: Instruction) {
    let sys_call_kind = SystemCallKind::try_from_primitive(instruction.arg as u8).unwrap();
    let argument_count = state.stack.pop().value as usize;
    let argument_count = argument_count / 2;
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

fn evaluate_function_call(state: &mut EvaluatorState, _: Instruction) {
    let argument_count = state.stack.pop().value;
    let base = state.stack.pop();
    assert!(base.is_pointer);
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

    for register in state.registers.iter().skip(state.protected_registers) {
        if register.is_pointer {
            state.stack.push_pointer(register.value);
        } else {
            state.stack.push(register.value);
        }
    }

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

        let return_address = state.stack.pop().value;
        state.pc = return_address as _;
        if result.is_pointer {
            state.stack.push_pointer(result.value);
        } else {
            state.stack.push(result.value);
        }
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

        let return_address = state.stack.pop().value;
        state.pc = return_address as _;
    }
}

fn evaluate_decode_closure(state: &mut EvaluatorState, instruction: Instruction) {
    let closure_pointer = state.stack.pop();
    assert!(closure_pointer.is_pointer);
    let mut closure_pointer = closure_pointer.value;

    let closure_length_in_bytes = if is_heap_pointer(closure_pointer) {
        state.heap.read_word(closure_pointer as _)
    } else {
        state.stack.read_word(closure_pointer as _)
    };
    let argument_count = bytes_to_word(closure_length_in_bytes) - 1 + instruction.arg;
    closure_pointer += WORD_SIZE_IN_BYTES;
    let end_address = closure_pointer + closure_length_in_bytes;

    let function_pointer = if is_heap_pointer(closure_pointer) {
        state.heap.read_word(closure_pointer as _)
    } else {
        state.stack.read_word(closure_pointer as _)
    };
    closure_pointer += WORD_SIZE_IN_BYTES;

    while closure_pointer < end_address {
        let argument = if is_heap_pointer(closure_pointer) {
            state.heap.read_word(closure_pointer as _)
        } else {
            state.stack.read_word(closure_pointer as _)
        };
        state.stack.push(argument);
        closure_pointer += WORD_SIZE_IN_BYTES;
    }

    state.stack.push_pointer(function_pointer);
    state.stack.push(argument_count);
}

fn evaluate_check_array_bounds(state: &mut EvaluatorState, instruction: Instruction) {
    let index = state.stack.pop().value;
    let array = state.stack.pop();
    assert!(array.is_pointer);
    let array = array.value;
    let array_length_in_bytes = if is_heap_pointer(array) {
        state.heap.read_word(array as _)
    } else {
        state.stack.read_word(array as _)
    };
    if index - WORD_SIZE_IN_BYTES >= array_length_in_bytes {
        let array_length_in_words = bytes_to_word(array_length_in_bytes);
        let index = (index - WORD_SIZE_IN_BYTES) as _;
        runtime_error!(state, index_out_of_bounds(instruction.span, index, array_length_in_words));
    }
    state.stack.push_pointer(array);
    state.stack.push(index);
}
