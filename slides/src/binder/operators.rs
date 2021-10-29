use std::fmt;

use super::typing::Type;

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

pub struct BoundBinary {
    pub lhs: Type,
    pub op: BoundBinaryOperator,
    pub rhs: Type,
    pub result: Type,
}

impl BoundBinary {
    pub fn same_input(input: &Type, op: BoundBinaryOperator, result: Type) -> BoundBinary {
        Self {
            lhs: input.clone(),
            op,
            rhs: input.clone(),
            result,
        }
    }

    pub fn same_output(op: BoundBinaryOperator, type_: Type) -> BoundBinary {
        Self {
            lhs: type_.clone(),
            op,
            rhs: type_.clone(),
            result: type_,
        }
    }

    pub fn new(lhs: &Type, op: BoundBinaryOperator, rhs: &Type, result: Type) -> BoundBinary {
        Self {
            lhs: lhs.clone(),
            op,
            rhs: rhs.clone(),
            result,
        }
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
