#[repr(u8)]
#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub enum OpCode {
    NoOp = 0,
    LoadImmediate = 1,
    Pop = 2,
    LoadRegister = 3,
    StoreInRegister = 4,
    CreateStackPointer = 5,
    ArrayIndex = 6,
    StoreInMemory = 7,
    TypeIdentifier = 8,
    // Binary Operators
    BitwiseTwosComplement = 25,
    BitwiseXor = 26,
    BitwiseNxor = 27,
    Addition = 28,
    Subtraction = 29,
    Multiplication = 30,
    Division = 31,
    Equals = 32,
    NotEquals = 33,
    ArrayEquals = 34,
    ArrayNotEquals = 35,
    LessThan = 36,
    GreaterThan = 37,
    LessThanEquals = 38,
    GreaterThanEquals = 39,
    //Jumps
    JmpRelative = 58,
    JmpIfFalse = 59,
    SysCall = 60,
}
