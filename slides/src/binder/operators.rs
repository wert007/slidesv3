use std::fmt;

use super::typing::TypeId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoundUnaryOperator {
    ArithmeticNegate,
    ArithmeticIdentity,
    LogicalNegation,
}

impl fmt::Display for BoundUnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BoundUnaryOperator::ArithmeticNegate => "-",
                BoundUnaryOperator::ArithmeticIdentity => "+",
                BoundUnaryOperator::LogicalNegation => "!",
            }
        )
    }
}

#[derive(Debug)]
pub struct BoundBinary {
    pub lhs: TypeId,
    pub op: BoundBinaryOperator,
    pub rhs: TypeId,
    pub result: TypeId,
}

impl BoundBinary {
    pub fn new(lhs: TypeId, op: BoundBinaryOperator, rhs: TypeId, result: TypeId) -> BoundBinary {
        Self {
            lhs,
            op,
            rhs,
            result,
        }
    }
    pub fn same_input(input: TypeId, op: BoundBinaryOperator, result: TypeId) -> BoundBinary {
        Self {
            lhs: input.clone(),
            op,
            rhs: input.clone(),
            result,
        }
    }

    pub fn same_output(op: BoundBinaryOperator, type_: TypeId) -> BoundBinary {
        Self {
            lhs: type_.clone(),
            op,
            rhs: type_.clone(),
            result: type_,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoundBinaryOperator {
    ArithmeticAddition,
    ArithmeticSubtraction,
    ArithmeticMultiplication,
    ArithmeticDivision,
    LogicalAnd,
    LogicalOr,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
    StringConcat,
    NoneableOrValue,
    Range,
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
                BoundBinaryOperator::LogicalAnd => "&&",
                BoundBinaryOperator::LogicalOr => "||",
                BoundBinaryOperator::Equals => "==",
                BoundBinaryOperator::NotEquals => "!=",
                BoundBinaryOperator::LessThan => "<",
                BoundBinaryOperator::GreaterThan => ">",
                BoundBinaryOperator::LessThanEquals => "<=",
                BoundBinaryOperator::GreaterThanEquals => ">=",
                BoundBinaryOperator::StringConcat => "+",
                BoundBinaryOperator::NoneableOrValue => "??",
                BoundBinaryOperator::Range => "..",
            }
        )
    }
}
