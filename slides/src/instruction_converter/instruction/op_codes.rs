#[repr(u8)]
#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub enum OpCode {
    NoOp = 0,
    LoadImmediate = 1,
    Pop = 2,
    LoadRegister = 3,
    StoreInRegister = 4,
    // Binary Operators
    BitwiseTwosComplement = 5,
    BitwiseXor = 6,
    BitwiseNxor = 7,
    Addition = 8,
    Subtraction = 9,
    Multiplication = 10,
    Division = 11,
    Equals = 12,
    NotEquals = 13,
    LessThan = 14,
    GreaterThan = 15,
    LessThanEquals = 16,
    GreaterThanEquals = 17,
    //Jumps
    JmpRelative = 18,
    JmpIfFalse = 19,
    SysCall = 20,
}
