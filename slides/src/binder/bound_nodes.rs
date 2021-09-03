use crate::{parser::syntax_nodes::LiteralNodeKind, text::TextSpan, value::Value};

use super::{
    operators::{BoundBinaryOperator, BoundUnaryOperator},
    typing::{SystemCallKind, Type},
};

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct BoundConstant {
    value: Value,
}

impl From<Value> for BoundConstant {
    fn from(value: Value) -> Self {
        Self { value }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct BoundNode<'a> {
    pub span: TextSpan,
    pub kind: BoundNodeKind<'a>,
    pub type_: Type,
    constant_value: Option<BoundConstant>,
}

impl<'a> BoundNode<'a> {
    pub fn error(span: TextSpan) -> Self {
        Self {
            span,
            kind: BoundNodeKind::ErrorExpression,
            type_: Type::Error,
            constant_value: None,
        }
    }

    pub fn literal(span: TextSpan, literal: LiteralNodeKind<'a>) -> Self {
        let value = literal.value;
        Self {
            span,
            kind: BoundNodeKind::LiteralExpression(literal),
            type_: value.into(),
            constant_value: Some(value.into()),
        }
    }

    pub fn array_literal(span: TextSpan, children: Vec<BoundNode<'a>>, type_: Type) -> Self {
        Self {
            span,
            kind: BoundNodeKind::ArrayLiteralExpression(BoundArrayLiteralNodeKind { children }),
            type_,
            constant_value: None,
        }
    }

    pub fn variable(span: TextSpan, variable_index: u64, type_: Type) -> Self {
        Self {
            span,
            kind: BoundNodeKind::VariableExpression(BoundVariableNodeKind { variable_index }),
            type_,
            constant_value: None,
        }
    }

    pub fn binary(
        span: TextSpan,
        lhs: BoundNode<'a>,
        operator_token: BoundBinaryOperator,
        rhs: BoundNode<'a>,
        type_: Type,
    ) -> Self {
        Self {
            span,
            kind: BoundNodeKind::BinaryExpression(BoundBinaryNodeKind {
                lhs: Box::new(lhs),
                operator_token,
                rhs: Box::new(rhs),
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn unary(
        span: TextSpan,
        operator_token: BoundUnaryOperator,
        operand: BoundNode<'a>,
        type_: Type,
    ) -> Self {
        Self {
            span,
            kind: BoundNodeKind::UnaryExpression(BoundUnaryNodeKind {
                operator_token,
                operand: Box::new(operand),
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn function_call(
        span: TextSpan,
        base: BoundNode<'a>,
        arguments: Vec<BoundNode<'a>>,
        type_: Type,
    ) -> Self {
        Self {
            span,
            kind: BoundNodeKind::FunctionCall(BoundFunctionCallNodeKind {
                base: Box::new(base),
                arguments,
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn system_call(
        span: TextSpan,
        base: SystemCallKind,
        arguments: Vec<BoundNode<'a>>,
        type_: Type,
    ) -> Self {
        Self {
            span,
            kind: BoundNodeKind::SystemCall(BoundSystemCallNodeKind { base, arguments }),
            type_,
            constant_value: None,
        }
    }

    pub fn while_statement(span: TextSpan, condition: BoundNode<'a>, body: BoundNode<'a>) -> Self {
        Self {
            span,
            kind: BoundNodeKind::WhileStatement(BoundWhileStatementNodeKind {
                condition: Box::new(condition),
                body: Box::new(body),
            }),
            type_: Type::Void,
            constant_value: None,
        }
    }

    pub fn variable_declaration(
        span: TextSpan,
        variable_index: u64,
        initializer: BoundNode<'a>,
    ) -> Self {
        Self {
            span,
            kind: BoundNodeKind::VariableDeclaration(BoundVariableDeclarationNodeKind {
                variable_index,
                initializer: Box::new(initializer),
            }),
            type_: Type::Void,
            constant_value: None,
        }
    }

    pub fn if_statement(span: TextSpan, condition: BoundNode<'a>, body: BoundNode<'a>) -> Self {
        Self {
            span,
            kind: BoundNodeKind::IfStatement(BoundIfStatementNodeKind {
                condition: Box::new(condition),
                body: Box::new(body),
            }),
            type_: Type::Void,
            constant_value: None,
        }
    }

    pub fn assignment(span: TextSpan, variable: BoundNode<'a>, expression: BoundNode<'a>) -> Self {
        Self {
            span,
            kind: BoundNodeKind::Assignment(BoundAssignmentNodeKind {
                variable: Box::new(variable),
                expression: Box::new(expression),
            }),
            type_: Type::Void,
            constant_value: None,
        }
    }

    pub fn block_statement(span: TextSpan, statements: Vec<BoundNode<'a>>) -> Self {
        Self {
            span,
            kind: BoundNodeKind::BlockStatement(BoundBlockStatementNodeKind { statements }),
            type_: Type::Void,
            constant_value: None,
        }
    }

    pub fn expression_statement(span: TextSpan, expression: BoundNode<'a>) -> Self {
        Self {
            span,
            kind: BoundNodeKind::ExpressionStatement(BoundExpressionStatementNodeKind {
                expression: Box::new(expression),
            }),
            type_: Type::Void,
            constant_value: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum BoundNodeKind<'a> {
    // Expressions
    ErrorExpression,
    LiteralExpression(LiteralNodeKind<'a>),
    ArrayLiteralExpression(BoundArrayLiteralNodeKind<'a>),
    VariableExpression(BoundVariableNodeKind),
    UnaryExpression(BoundUnaryNodeKind<'a>),
    BinaryExpression(BoundBinaryNodeKind<'a>),
    FunctionCall(BoundFunctionCallNodeKind<'a>),
    SystemCall(BoundSystemCallNodeKind<'a>),

    // Statements
    BlockStatement(BoundBlockStatementNodeKind<'a>),
    IfStatement(BoundIfStatementNodeKind<'a>),
    VariableDeclaration(BoundVariableDeclarationNodeKind<'a>),
    WhileStatement(BoundWhileStatementNodeKind<'a>),
    Assignment(BoundAssignmentNodeKind<'a>),
    ExpressionStatement(BoundExpressionStatementNodeKind<'a>),
}

#[derive(Debug, Clone)]
pub struct BoundArrayLiteralNodeKind<'a> {
    pub children: Vec<BoundNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct BoundVariableNodeKind {
    pub variable_index: u64,
}

#[derive(Debug, Clone)]
pub struct BoundUnaryNodeKind<'a> {
    pub operator_token: BoundUnaryOperator,
    pub operand: Box<BoundNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct BoundBinaryNodeKind<'a> {
    pub lhs: Box<BoundNode<'a>>,
    pub operator_token: BoundBinaryOperator,
    pub rhs: Box<BoundNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct BoundFunctionCallNodeKind<'a> {
    pub base: Box<BoundNode<'a>>,
    pub arguments: Vec<BoundNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct BoundSystemCallNodeKind<'a> {
    pub base: SystemCallKind,
    pub arguments: Vec<BoundNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct BoundIfStatementNodeKind<'a> {
    pub condition: Box<BoundNode<'a>>,
    pub body: Box<BoundNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct BoundVariableDeclarationNodeKind<'a> {
    pub variable_index: u64,
    pub initializer: Box<BoundNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct BoundWhileStatementNodeKind<'a> {
    pub condition: Box<BoundNode<'a>>,
    pub body: Box<BoundNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct BoundAssignmentNodeKind<'a> {
    pub variable: Box<BoundNode<'a>>,
    pub expression: Box<BoundNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct BoundBlockStatementNodeKind<'a> {
    pub statements: Vec<BoundNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct BoundExpressionStatementNodeKind<'a> {
    pub expression: Box<BoundNode<'a>>,
}
