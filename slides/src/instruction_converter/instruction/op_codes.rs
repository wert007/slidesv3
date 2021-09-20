#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum OpCode {
    NoOp = 0,
    LoadImmediate = 1,
    LoadPointer = 2,
    Pop = 3,
    LoadRegister = 4,
    StoreInRegister = 5,
    CreateStackPointer = 6,
    ArrayIndex = 7,
    StoreInMemory = 8,
    TypeIdentifier = 9,
    Label = 10,
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
    StringConcat = 40,
    //Jumps
    JmpRelative = 58,
    JmpIfFalse = 59,
    SysCall = 60,
    FunctionCall = 61,
    Return = 62,
}
