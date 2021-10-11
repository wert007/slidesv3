use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoundUnaryOperator {
    ArithmeticNegate,
    ArithmeticIdentity,
}

impl fmt::Display for BoundUnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BoundUnaryOperator::ArithmeticNegate => "-",
                BoundUnaryOperator::ArithmeticIdentity => "+",
            }
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    StringConcat,
    NoneableOrValue,
}

impl fmt::Display for BoundBinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BoundBinaryOperator::ArithmeticAddition => "+",
                BoundBinaryOperator::ArithmeticSubtraction => "-",
                BoundBinaryOperator::ArithmeticMultiplication => "*",
                BoundBinaryOperator::ArithmeticDivision => "/",
                BoundBinaryOperator::Equals => "==",
                BoundBinaryOperator::NotEquals => "!=",
                BoundBinaryOperator::LessThan => "<",
                BoundBinaryOperator::GreaterThan => ">",
                BoundBinaryOperator::LessThanEquals => "<=",
                BoundBinaryOperator::GreaterThanEquals => ">=",
                BoundBinaryOperator::StringConcat => "+",
                BoundBinaryOperator::NoneableOrValue => "??",
            }
        )
    }
}
