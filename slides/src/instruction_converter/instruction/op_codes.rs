#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum OpCode {
    NoOp = 0,
    LoadImmediate = 1,
    LoadPointer = 2,
    DuplicateOver = 3,
    Pop = 4,
    LoadRegister = 5,
    StoreInRegister = 6,
    StoreInMemory = 8,
    WriteToStack = 9,
    WriteToHeap = 10,
    Allocate = 11,
    ReadWordWithOffset = 12,
    MemoryCopy = 13,
    TypeIdentifier = 14,
    Label = 15,
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
    NoneableEquals = 36,
    TypeIdentifierEquals = 37,
    LessThan = 38,
    GreaterThan = 39,
    LessThanEquals = 40,
    GreaterThanEquals = 41,
    StringConcat = 42,
    NoneableOrValue = 44,
    //Jumps
    Jump = 60,
    JumpIfFalse = 61,
    JumpIfTrue = 62,
    SysCall = 63,
    FunctionCall = 64,
    Return = 65,
    DecodeClosure = 66,
    Breakpoint = 67,
    // Trivia
}
