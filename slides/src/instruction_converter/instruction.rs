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
    pub const fn load_immediate(immediate: u64) -> Self {
        Self {
            op_code: OpCode::LoadImmediate,
            arg: immediate,
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

    pub const fn array_length(count_in_bytes: usize) -> Self {
        Self {
            op_code: OpCode::ArrayLength,
            arg: count_in_bytes as u64,
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

    pub const fn jump_relative(relative_address: i64) -> Self {
        Self {
            op_code: OpCode::JmpRelative,
            arg: relative_address as _,
        }
    }

    pub const fn jump_if_false(relative_address: i64) -> Self {
        Self {
            op_code: OpCode::JmpIfFalse,
            arg: relative_address as _,
        }
    }

    pub const fn system_call(call_kind: SystemCallKind, argument_count: usize) -> Self {
        let arg = (argument_count << 8) as u64 | call_kind as u64;
        Self {
            op_code: OpCode::SysCall,
            arg,
        }
    }
}

impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} arg: {} ({})",
            self.op_code, self.arg, self.arg as i64
        )
    }
}
