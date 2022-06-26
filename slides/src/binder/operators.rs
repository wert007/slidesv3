use std::fmt;

use super::{symbols::FunctionSymbol, typing::Type};

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
    pub lhs: Type,
    pub op: BoundBinaryOperator,
    pub rhs: Type,
    pub result: Type,
    pub function_symbol: Option<FunctionSymbol>,
    pub reverse_args: bool,
}

impl BoundBinary {
    pub fn same_input(input: &Type, op: BoundBinaryOperator, result: Type) -> BoundBinary {
        Self {
            lhs: input.clone(),
            op,
            rhs: input.clone(),
            result,
            function_symbol: None,
            reverse_args: false,
        }
    }

    pub fn same_output(op: BoundBinaryOperator, type_: Type) -> BoundBinary {
        Self {
            lhs: type_.clone(),
            op,
            rhs: type_.clone(),
            result: type_,
            function_symbol: None,
            reverse_args: false,
        }
    }

    pub fn new(lhs: &Type, op: BoundBinaryOperator, rhs: &Type, result: Type) -> BoundBinary {
        Self {
            lhs: lhs.clone(),
            op,
            rhs: rhs.clone(),
            result,
            function_symbol: None,
            reverse_args: false,
        }
    }

    pub fn from_function_symbol(
        lhs: &Type,
        op: BoundBinaryOperator,
        rhs: &Type,
        function: &FunctionSymbol,
        reverse_args: bool,
    ) -> BoundBinary {
        Self {
            lhs: lhs.clone(),
            op,
            rhs: rhs.clone(),
            result: function.function_type.return_type.clone(),
            function_symbol: Some(function.clone()),
            reverse_args,
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
