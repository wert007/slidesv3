use num_enum::TryFromPrimitive;

use crate::binder::typing::SystemCallKind;

use self::op_codes::OpCode;

pub mod op_codes;

#[derive(Clone, Copy)]
pub struct Instruction {
    pub op_code: OpCode,
    pub arg: u64,
}

#[allow(dead_code)]
impl Instruction {
    pub const fn noop() -> Instruction {
        Self {
            op_code: OpCode::NoOp,
            arg: 0,
        }
    }

    pub const fn load_immediate(immediate: u64) -> Self {
        Self {
            op_code: OpCode::LoadImmediate,
            arg: immediate,
        }
    }

    pub const fn load_pointer(pointer: u64) -> Self {
        Self {
            op_code: OpCode::LoadPointer,
            arg: pointer,
        }
    }

    pub const fn pop() -> Self {
        Self {
            op_code: OpCode::Pop,
            arg: 0,
        }
    }

    pub const fn load_register(register: u64) -> Self {
        Self {
            op_code: OpCode::LoadRegister,
            arg: register,
        }
    }

    pub const fn store_in_register(register: u64) -> Self {
        Self {
            op_code: OpCode::StoreInRegister,
            arg: register,
        }
    }

    pub const fn create_stack_pointer(offset: u64) -> Self {
        Self {
            op_code: OpCode::CreateStackPointer,
            arg: offset,
        }
    }

    pub const fn array_index() -> Self {
        Self {
            op_code: OpCode::ArrayIndex,
            arg: 0,
        }
    }

    pub const fn store_in_memory() -> Self {
        Self {
            op_code: OpCode::StoreInMemory,
            arg: 0,
        }
    }

    pub const fn write_to_stack(address: u64) -> Self {
        Self {
            op_code: OpCode::WriteToStack,
            arg: address,
        }
    }

    pub const fn write_registers_to_stack(start_register: usize) -> Self {
        Self {
            op_code: OpCode::WriteRegistersToStack,
            arg: start_register as _,
        }
    }

    pub const fn read_registers_from_stack(start_register: usize) -> Self {
        Self {
            op_code: OpCode::ReadRegistersFromStack,
            arg: start_register as _,
        }
    }

    pub const fn type_identifier(type_identifier: u64) -> Self {
        Self {
            op_code: OpCode::TypeIdentifier,
            arg: type_identifier,
        }
    }

    pub const fn twos_complement() -> Self {
        Self {
            op_code: OpCode::BitwiseTwosComplement,
            arg: 0,
        }
    }

    pub const fn xor() -> Self {
        Self {
            op_code: OpCode::BitwiseXor,
            arg: 0,
        }
    }

    pub const fn nxor() -> Self {
        Self {
            op_code: OpCode::BitwiseNxor,
            arg: 0,
        }
    }

    pub const fn addition() -> Self {
        Self {
            op_code: OpCode::Addition,
            arg: 0,
        }
    }

    pub const fn subtraction() -> Self {
        Self {
            op_code: OpCode::Subtraction,
            arg: 0,
        }
    }

    pub const fn multiplication() -> Self {
        Self {
            op_code: OpCode::Multiplication,
            arg: 0,
        }
    }

    pub const fn division() -> Self {
        Self {
            op_code: OpCode::Division,
            arg: 0,
        }
    }

    pub const fn equals() -> Self {
        Self {
            op_code: OpCode::Equals,
            arg: 0,
        }
    }

    pub const fn not_equals() -> Self {
        Self {
            op_code: OpCode::NotEquals,
            arg: 0,
        }
    }


    pub const fn array_equals() -> Self {
        Self {
            op_code: OpCode::ArrayEquals,
            arg: 0,
        }
    }

    pub const fn array_not_equals() -> Self {
        Self {
            op_code: OpCode::ArrayNotEquals,
            arg: 0,
        }
    }

    pub const fn less_than() -> Self {
        Self {
            op_code: OpCode::LessThan,
            arg: 0,
        }
    }

    pub const fn greater_than() -> Self {
        Self {
            op_code: OpCode::GreaterThan,
            arg: 0,
        }
    }

    pub const fn less_than_equals() -> Self {
        Self {
            op_code: OpCode::LessThanEquals,
            arg: 0,
        }
    }

    pub const fn greater_than_equals() -> Self {
        Self {
            op_code: OpCode::GreaterThanEquals,
            arg: 0,
        }
    }

    pub const fn string_concat() -> Self {
        Self {
            op_code: OpCode::StringConcat,
            arg: 0,
        }
    }

    pub const fn jump_to_label(label_index: usize) -> Self {
        Self {
            op_code: OpCode::Jump,
            arg: label_index as _,
        }
    }

    pub const fn jump(address: u64) -> Self {
        Self {
            op_code: OpCode::Jump,
            arg: address,
        }
    }

    pub const fn jump_to_label_conditionally(label_index: usize, jump_if_true: bool) -> Self {
        let op_code = if jump_if_true {
            OpCode::JumpIfTrue
        } else {
            OpCode::JumpIfFalse
        };
        Self {
            op_code,
            arg: label_index as _,
        }
    }

    pub const fn jump_if_false(address: u64) -> Self {
        Self {
            op_code: OpCode::JumpIfFalse,
            arg: address,
        }
    }

    pub const fn jump_if_true(address: u64) -> Self {
        Self {
            op_code: OpCode::JumpIfTrue,
            arg: address,
        }
    }

    pub const fn system_call(call_kind: SystemCallKind, argument_count: usize) -> Self {
        let arg = (argument_count << 8) as u64 | call_kind as u64;
        Self {
            op_code: OpCode::SysCall,
            arg,
        }
    }

    pub const fn function_call(argument_count: usize) -> Self {
        Self {
            op_code: OpCode::FunctionCall,
            arg: argument_count as _,
        }
    }

    pub const fn label(index: usize) -> Self {
        Self {
            op_code: OpCode::Label,
            arg: index as _,
        }
    }

    pub const fn return_from_function(returns_value: bool) -> Self {
        Self {
            op_code: OpCode::Return,
            arg: returns_value as _,
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
                    Err(v) =>v.to_string(),
                };
                let arg_count = self.arg >> 8;
                write!(f, "{:?} arg: sys call {}, arg_count {}", self.op_code, kind, arg_count)
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
