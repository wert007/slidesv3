use num_enum::TryFromPrimitive;

use crate::{binder::typing::SystemCallKind, text::TextLocation};

use self::op_codes::OpCode;

pub mod op_codes;

#[derive(Clone, Copy)]
pub struct Instruction {
    pub op_code: OpCode,
    pub arg: u64,
    pub location: TextLocation,
}

#[allow(dead_code)]
impl Instruction {
    pub const fn noop(location: TextLocation) -> Instruction {
        Self {
            op_code: OpCode::NoOp,
            arg: 0,
            location,
        }
    }

    pub const fn load_immediate(immediate: u64, location: TextLocation) -> Self {
        Self {
            op_code: OpCode::LoadImmediate,
            arg: immediate,
            location,
        }
    }

    pub const fn load_pointer(pointer: u64, location: TextLocation) -> Self {
        Self {
            op_code: OpCode::LoadPointer,
            arg: pointer,
            location,
        }
    }

    pub const fn load_none_pointer(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::LoadPointer,
            arg: u64::MAX,
            location,
        }
    }

    pub const fn duplicate(location: TextLocation) -> Self {
        Self::duplicate_over(0, location)
    }

    pub const fn duplicate_over(words_over: u64, location: TextLocation) -> Self {
        Self {
            op_code: OpCode::DuplicateOver,
            arg: words_over,
            location,
        }
    }

    pub const fn pop(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::Pop,
            arg: 0,
            location,
        }
    }

    pub const fn load_register(register: u64, location: TextLocation) -> Self {
        Self {
            op_code: OpCode::LoadRegister,
            arg: register,
            location,
        }
    }

    pub const fn store_in_register(register: u64, location: TextLocation) -> Self {
        Self {
            op_code: OpCode::StoreInRegister,
            arg: register,
            location,
        }
    }

    pub const fn store_in_memory(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::StoreInMemory,
            arg: 0,
            location,
        }
    }

    pub const fn write_to_stack(address: u64, location: TextLocation) -> Self {
        Self {
            op_code: OpCode::WriteToStack,
            arg: address,
            location,
        }
    }

    pub fn write_to_heap(word_count: u64, location: TextLocation) -> Self {
        assert_ne!(word_count, 0);
        Self {
            op_code: OpCode::WriteToHeap,
            arg: word_count,
            location,
        }
    }

    pub const fn write_to_heap_runtime(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::WriteToHeap,
            arg: 0,
            location,
        }
    }

    pub const fn allocate(size_in_bytes: u64, location: TextLocation) -> Self {
        Self {
            op_code: OpCode::Allocate,
            arg: size_in_bytes,
            location,
        }
    }

    pub const fn read_word_with_offset(offset: u64, location: TextLocation) -> Self {
        Self {
            op_code: OpCode::ReadWordWithOffset,
            arg: offset,
            location,
        }
    }

    pub const fn memory_copy(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::MemoryCopy,
            arg: 0,
            location,
        }
    }

    pub fn memory_copy_fixed_size(size_in_bytes: u64, location: TextLocation) -> Self {
        assert_ne!(size_in_bytes, 0);
        Self {
            op_code: OpCode::MemoryCopy,
            arg: size_in_bytes,
            location,
        }
    }

    pub const fn type_identifier(type_identifier: u64, location: TextLocation) -> Self {
        Self {
            op_code: OpCode::TypeIdentifier,
            arg: type_identifier,
            location,
        }
    }

    pub const fn twos_complement(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::BitwiseTwosComplement,
            arg: 0,
            location,
        }
    }

    pub const fn xor(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::BitwiseXor,
            arg: 0,
            location,
        }
    }

    pub const fn nxor(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::BitwiseNxor,
            arg: 0,
            location,
        }
    }

    pub const fn addition(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::Addition,
            arg: 0,
            location,
        }
    }

    pub const fn subtraction(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::Subtraction,
            arg: 0,
            location,
        }
    }

    pub const fn multiplication(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::Multiplication,
            arg: 0,
            location,
        }
    }

    pub const fn division(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::Division,
            arg: 0,
            location,
        }
    }

    pub const fn equals(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::Equals,
            arg: 0,
            location,
        }
    }

    pub const fn not_equals(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::NotEquals,
            arg: 0,
            location,
        }
    }

    pub const fn array_equals(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::ArrayEquals,
            arg: 0,
            location,
        }
    }

    pub const fn array_not_equals(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::ArrayNotEquals,
            arg: 0,
            location,
        }
    }

    pub const fn noneable_equals(size_in_bytes: u64, location: TextLocation) -> Self {
        Self {
            op_code: OpCode::NoneableEquals,
            arg: size_in_bytes,
            location,
        }
    }

    pub const fn type_identifier_equals(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::TypeIdentifierEquals,
            arg: 0,
            location,
        }
    }

    pub const fn less_than(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::LessThan,
            arg: 0,
            location,
        }
    }

    pub const fn greater_than(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::GreaterThan,
            arg: 0,
            location,
        }
    }

    pub const fn less_than_equals(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::LessThanEquals,
            arg: 0,
            location,
        }
    }

    pub const fn greater_than_equals(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::GreaterThanEquals,
            arg: 0,
            location,
        }
    }

    pub const fn string_concat(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::StringConcat,
            arg: 0,
            location,
        }
    }

    pub const fn noneable_or_value(needs_dereferencing: bool, location: TextLocation) -> Self {
        Self {
            op_code: OpCode::NoneableOrValue,
            arg: needs_dereferencing as _,
            location,
        }
    }

    pub const fn jump_to_label(label_index: usize, location: TextLocation) -> Self {
        Self {
            op_code: OpCode::Jump,
            arg: label_index as _,
            location,
        }
    }

    pub const fn jump(address: u64, location: TextLocation) -> Self {
        Self {
            op_code: OpCode::Jump,
            arg: address,
            location,
        }
    }

    pub const fn jump_to_label_conditionally(label_index: usize, jump_if_true: bool, location: TextLocation) -> Self {
        let op_code = if jump_if_true {
            OpCode::JumpIfTrue
        } else {
            OpCode::JumpIfFalse
        };
        Self {
            op_code,
            arg: label_index as _,
            location,
        }
    }

    pub const fn jump_if_false(address: u64, location: TextLocation) -> Self {
        Self {
            op_code: OpCode::JumpIfFalse,
            arg: address,
            location,
        }
    }

    pub const fn jump_if_true(address: u64, location: TextLocation) -> Self {
        Self {
            op_code: OpCode::JumpIfTrue,
            arg: address,
            location,
        }
    }

    pub const fn system_call(call_kind: SystemCallKind, location: TextLocation) -> Self {
        let arg = call_kind as u64;
        Self {
            op_code: OpCode::SysCall,
            arg,
            location,
        }
    }

    pub const fn function_call(location: TextLocation, argument_count: u64, returns_value: bool) -> Self {
        let arg = argument_count << 1 | if returns_value { 1 } else { 0 };
        Self {
            op_code: OpCode::FunctionCall,
            arg,
            location,
        }
    }

    pub const fn label(index: usize, location: TextLocation) -> Self {
        Self {
            op_code: OpCode::Label,
            arg: index as _,
            location,
        }
    }

    pub fn return_from_function(returns_value: bool, restores_registers: bool, location: TextLocation) -> Self {
        let mut arg = 0;
        if returns_value {
            arg += 1;
        }
        if !restores_registers {
            arg += 2;
        }
        Self {
            op_code: OpCode::Return,
            arg,
            location,
        }
    }

    pub const fn decode_closure(argument_count: u64, has_function_pointer: bool, location: TextLocation) -> Self {
        Self {
            op_code: OpCode::DecodeClosure,
            arg: (argument_count << 1) + has_function_pointer as u64,
            location,
        }
    }

    pub const fn breakpoint(location: TextLocation) -> Self {
        Self {
            op_code: OpCode::Breakpoint,
            arg: 0,
            location,
        }
    }

    pub(crate) fn rotate(offset: u64, location: TextLocation) -> Self {
        Self {
            op_code: OpCode::Rotate,
            arg: offset,
            location,
        }
    }
}

impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.op_code {
            OpCode::SysCall => {
                // let arg = (argument_count << 8) as u64 | call_kind as u64;
                let kind = (self.arg & 0xFF) as u8;
                let kind = SystemCallKind::try_from_primitive(kind).map_err(|_| kind);
                let kind = match kind {
                    Ok(v) => v.to_string(),
                    Err(v) => v.to_string(),
                };
                let arg_count = self.arg >> 8;
                write!(
                    f,
                    "{:?} arg: sys call {}, arg_count {}",
                    self.op_code, kind, arg_count
                )
            }
            _ => {
                write!(
                    f,
                    "{:?} arg: {} ({})",
                    self.op_code, self.arg, self.arg as i64
                )
            }
        }
    }
}
