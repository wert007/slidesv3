#[derive(Debug, Clone, Copy)]
pub enum BoundUnaryOperator {
    ArithmeticNegate,
    ArithmeticIdentity,
}

#[derive(Debug, Clone, Copy)]
pub enum BoundBinaryOperator {
    ArithmeticAddition,
    ArithmeticSubtraction,
    ArithmeticMultiplication,
    ArithmeticDivision,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
}
