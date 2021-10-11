use crate::{parser::syntax_nodes::LiteralNodeKind, text::TextSpan, value::Value};

use super::{operators::{BoundBinaryOperator, BoundUnaryOperator}, typing::{FunctionKind, StructType, SystemCallKind, Type}};

pub mod is_same_expression;

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
    pub byte_width: u64,
    constant_value: Option<BoundConstant>,
}

impl<'a> BoundNode<'a> {
    pub fn error(span: TextSpan) -> Self {
        Self {
            span,
            kind: BoundNodeKind::ErrorExpression,
            type_: Type::Error,
            byte_width: 0,
            constant_value: None,
        }
    }

    pub fn literal_from_value(value: Value) -> Self {
        let type_ = value.infer_type();
        let byte_width = match &value {
            Value::Integer(_) | Value::Boolean(_) | Value::SystemCall(_) => 4,
            Value::String(value) => 8 + value.len(),
            Value::None => 4,
        } as _;
        Self {
            span: TextSpan::zero(),
            kind: BoundNodeKind::LiteralExpression(LiteralNodeKind {
                token: crate::lexer::syntax_token::SyntaxToken {
                    kind: crate::lexer::syntax_token::SyntaxTokenKind::Eoi,
                    lexeme: "",
                    start: 0,
                },
                value: value.clone(),
            }),
            type_,
            byte_width,
            constant_value: Some(value.into()),
        }
    }

    pub fn literal(span: TextSpan, literal: LiteralNodeKind<'a>) -> Self {
        let value = literal.value.clone();
        let type_ = value.infer_type();
        let byte_width = match &value {
            Value::Integer(_) | Value::Boolean(_) | Value::SystemCall(_) => 4,
            Value::String(value) => 8 + value.len(),
            Value::None => 4,
        } as _;
        Self {
            span,
            kind: BoundNodeKind::LiteralExpression(literal),
            type_,
            byte_width,
            constant_value: Some(value.into()),
        }
    }

    pub fn array_literal(span: TextSpan, children: Vec<BoundNode<'a>>, type_: Type) -> Self {
        let byte_width = 8 + children.iter().map(|b| (b.byte_width + 3) / 4).sum::<u64>() * 4;
        Self {
            span,
            kind: BoundNodeKind::ArrayLiteralExpression(BoundArrayLiteralNodeKind { children }),
            type_,
            byte_width,
            constant_value: None,
        }
    }

    pub fn constructor_call(
        span: TextSpan,
        arguments: Vec<BoundNode<'a>>,
        base_type: StructType,
    ) -> Self {
        Self {
            span,
            kind: BoundNodeKind::ConstructorCall(BoundConstructorCallNodeKind {
                arguments,
                base_type: base_type.clone(),
            }),
            type_: Type::Struct(Box::new(base_type)),
            byte_width: 4,
            constant_value: None,
        }
    }

    pub fn variable(span: TextSpan, variable_index: u64, type_: Type) -> Self {
        Self {
            span,
            kind: BoundNodeKind::VariableExpression(BoundVariableNodeKind { variable_index }),
            type_,
            byte_width: 4,
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
        let byte_width = 4;
        Self {
            span,
            kind: BoundNodeKind::BinaryExpression(BoundBinaryNodeKind {
                lhs: Box::new(lhs),
                operator_token,
                rhs: Box::new(rhs),
            }),
            type_,
            byte_width,
            constant_value: None,
        }
    }

    pub fn unary(
        span: TextSpan,
        operator_token: BoundUnaryOperator,
        operand: BoundNode<'a>,
        type_: Type,
    ) -> Self {
        let byte_width = operand.byte_width;
        Self {
            span,
            kind: BoundNodeKind::UnaryExpression(BoundUnaryNodeKind {
                operator_token,
                operand: Box::new(operand),
            }),
            type_,
            byte_width,
            constant_value: None,
        }
    }

    pub fn function_call(
        span: TextSpan,
        base: BoundNode<'a>,
        arguments: Vec<BoundNode<'a>>,
        has_this_argument: bool,
        type_: Type,
    ) -> Self {
        Self {
            span,
            kind: BoundNodeKind::FunctionCall(BoundFunctionCallNodeKind {
                base: Box::new(base),
                arguments,
                has_this_argument,
            }),
            type_,
            byte_width: 4,
            constant_value: None,
        }
    }

    pub fn array_index(
        span: TextSpan,
        base: BoundNode<'a>,
        index: BoundNode<'a>,
        type_: Type,
    ) -> Self {
        let byte_width = index.byte_width + base.byte_width;
        Self {
            span,
            kind: BoundNodeKind::ArrayIndex(BoundArrayIndexNodeKind {
                base: Box::new(base),
                index: Box::new(index),
            }),
            type_,
            byte_width,
            constant_value: None,
        }
    }

    pub fn field_access(span: TextSpan, base: BoundNode<'a>, offset: u64, type_: Type) -> Self {
        let byte_width = base.byte_width;
        Self {
            span,
            kind: BoundNodeKind::FieldAccess(BoundFieldAccessNodeKind {
                base: Box::new(base),
                offset,
                type_: type_.clone(),
            }),
            type_,
            byte_width,
            constant_value: None,
        }
    }

    pub fn closure(span: TextSpan, base: BoundNode<'a>, id: u64, type_: Type) -> Self {
        Self {
            span,
            kind: BoundNodeKind::Closure(BoundClosureNodeKind {
                base: Box::new(base),
                function: FunctionKind::FunctionId(id),
            }),
            type_,
            byte_width: 4,
            constant_value: None,
        }
    }

    pub fn system_call_closure(span: TextSpan, base: BoundNode<'a>, system_call_kind: SystemCallKind, type_: Type) -> Self {
        Self {
            span,
            kind: BoundNodeKind::Closure(BoundClosureNodeKind {
                base: Box::new(base),
                function: FunctionKind::SystemCall(system_call_kind),
            }),
            type_,
            byte_width: 4,
            constant_value: None,
        }
    }

    pub fn conversion(span: TextSpan, base: BoundNode<'a>, type_: Type) -> Self {
        Self {
            span,
            kind: BoundNodeKind::Conversion(BoundConversionNodeKind {
                base: Box::new(base),
                type_: type_.clone(),
            }),
            type_,
            byte_width: 4,
            constant_value: None,
        }
    }

    pub fn system_call(
        span: TextSpan,
        base: SystemCallKind,
        arguments: Vec<BoundNode<'a>>,
        type_: Type,
    ) -> Self {
        let byte_width =
            4 * (arguments.len() as u64 + 1) + arguments.iter().map(|b| b.byte_width).sum::<u64>();
        Self {
            span,
            kind: BoundNodeKind::SystemCall(BoundSystemCallNodeKind { base, arguments }),
            type_,
            byte_width,
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
            byte_width: 0,
            constant_value: None,
        }
    }

    pub fn for_statement(
        span: TextSpan,
        index_variable: u64,
        collection_variable: u64,
        variable: BoundNode<'a>,
        collection: BoundNode<'a>,
        body: BoundNode<'a>,
    ) -> Self {
        let while_condition = BoundNode::binary(
            span,
            BoundNode::variable(span, index_variable, Type::Integer),
            BoundBinaryOperator::LessThan,
            BoundNode::system_call(
                span,
                SystemCallKind::ArrayLength,
                vec![BoundNode::variable(
                    span,
                    collection_variable,
                    collection.type_.clone(),
                )],
                Type::Integer,
            ),
            Type::Boolean,
        );
        let while_body = BoundNode::block_statement(
            span,
            vec![
                // variable = $collection[$index];
                BoundNode::assignment(
                    span,
                    variable,
                    BoundNode::array_index(
                        span,
                        BoundNode::variable(span, collection_variable, collection.type_.clone()),
                        BoundNode::variable(span, index_variable, Type::Integer),
                        collection.type_.array_base_type().unwrap().clone(),
                    ),
                ),
                // {
                body,
                // }
                // $index = $index + 1;
                BoundNode::assignment(
                    span,
                    BoundNode::variable(span, index_variable, Type::Integer),
                    BoundNode::binary(
                        span,
                        BoundNode::variable(span, index_variable, Type::Integer),
                        BoundBinaryOperator::ArithmeticAddition,
                        BoundNode::literal_from_value(Value::Integer(1)),
                        Type::Integer,
                    ),
                ),
            ],
        );
        BoundNode::block_statement(
            span,
            vec![
                BoundNode::variable_declaration(
                    span,
                    index_variable,
                    BoundNode::literal_from_value(Value::Integer(0)),
                    None,
                ), // let $index = 0;
                BoundNode::variable_declaration(span, collection_variable, collection, None), // let $collection = collection;
                BoundNode::while_statement(span, while_condition, while_body), // while $index < array length($collection)
            ],
        )
    }

    pub fn variable_declaration(
        span: TextSpan,
        variable_index: u64,
        initializer: BoundNode<'a>,
        variable_type: Option<Type>,
    ) -> Self {
        Self {
            span,
            kind: BoundNodeKind::VariableDeclaration(BoundVariableDeclarationNodeKind {
                variable_index,
                variable_type: variable_type.unwrap_or(initializer.type_.clone()),
                initializer: Box::new(initializer),
            }),
            type_: Type::Void,
            byte_width: 0,
            constant_value: None,
        }
    }

    pub fn if_statement(
        span: TextSpan,
        condition: BoundNode<'a>,
        body: BoundNode<'a>,
        else_body: Option<BoundNode<'a>>,
    ) -> Self {
        Self {
            span,
            kind: BoundNodeKind::IfStatement(BoundIfStatementNodeKind {
                condition: Box::new(condition),
                body: Box::new(body),
                else_body: else_body.map(Box::new),
            }),
            type_: Type::Void,
            byte_width: 0,
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
            byte_width: 0,
            constant_value: None,
        }
    }

    pub fn block_statement(span: TextSpan, statements: Vec<BoundNode<'a>>) -> Self {
        Self {
            span,
            kind: BoundNodeKind::BlockStatement(BoundBlockStatementNodeKind { statements }),
            type_: Type::Void,
            byte_width: 0,
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
            byte_width: 0,
            constant_value: None,
        }
    }

    pub fn function_declaration(
        index: usize,
        is_main: bool,
        body: BoundNode<'a>,
        parameters: Vec<u64>,
    ) -> Self {
        Self {
            span: body.span,
            kind: BoundNodeKind::FunctionDeclaration(BoundFunctionDeclarationNodeKind {
                index,
                is_main,
                body: Box::new(body),
                parameters,
            }),
            type_: Type::Void,
            byte_width: 0,
            constant_value: None,
        }
    }

    pub fn label_reference(index: usize) -> Self {
        Self {
            span: TextSpan::zero(),
            kind: BoundNodeKind::LabelReference(index),
            type_: Type::Integer, // FIXME: Type::Pointer
            byte_width: 4,
            constant_value: None,
        }
    }

    pub fn label(index: usize) -> Self {
        Self {
            span: TextSpan::zero(),
            kind: BoundNodeKind::Label(index),
            type_: Type::Void,
            byte_width: 4,
            constant_value: None,
        }
    }

    pub fn jump(target: BoundNode<'a>) -> Self {
        Self {
            span: TextSpan::zero(),
            kind: BoundNodeKind::Jump(BoundJumpNodeKind {
                condition: None,
                target: Box::new(target),
                jump_if_true: true,
            }),
            type_: Type::Void,
            byte_width: 0,
            constant_value: None,
        }
    }

    pub fn jump_if_true(condition: BoundNode<'a>, target: BoundNode<'a>) -> Self {
        Self {
            span: TextSpan::zero(),
            kind: BoundNodeKind::Jump(BoundJumpNodeKind {
                condition: Some(Box::new(condition)),
                target: Box::new(target),
                jump_if_true: true,
            }),
            type_: Type::Void,
            byte_width: 0,
            constant_value: None,
        }
    }

    pub fn jump_if_false(condition: BoundNode<'a>, target: BoundNode<'a>) -> Self {
        Self {
            span: TextSpan::zero(),
            kind: BoundNodeKind::Jump(BoundJumpNodeKind {
                condition: Some(Box::new(condition)),
                target: Box::new(target),
                jump_if_true: false,
            }),
            type_: Type::Void,
            byte_width: 0,
            constant_value: None,
        }
    }

    pub fn return_statement(
        span: TextSpan,
        expression: Option<BoundNode<'a>>,
        restores_variables: bool,
    ) -> Self {
        Self {
            span,
            kind: BoundNodeKind::ReturnStatement(BoundReturnStatementNodeKind {
                expression: expression.map(Box::new),
                restores_variables,
            }),
            type_: Type::Void,
            byte_width: 0,
            constant_value: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum BoundNodeKind<'a> {
    // Top Level Statements
    FunctionDeclaration(BoundFunctionDeclarationNodeKind<'a>),
    // Generated
    ErrorExpression,
    Label(usize),
    LabelReference(usize),
    Jump(BoundJumpNodeKind<'a>),
    // Expressions
    LiteralExpression(LiteralNodeKind<'a>),
    ArrayLiteralExpression(BoundArrayLiteralNodeKind<'a>),
    ConstructorCall(BoundConstructorCallNodeKind<'a>),
    VariableExpression(BoundVariableNodeKind),
    UnaryExpression(BoundUnaryNodeKind<'a>),
    BinaryExpression(BoundBinaryNodeKind<'a>),
    FunctionCall(BoundFunctionCallNodeKind<'a>),
    SystemCall(BoundSystemCallNodeKind<'a>),
    ArrayIndex(BoundArrayIndexNodeKind<'a>),
    FieldAccess(BoundFieldAccessNodeKind<'a>),
    Closure(BoundClosureNodeKind<'a>),
    Conversion(BoundConversionNodeKind<'a>),

    // Statements
    BlockStatement(BoundBlockStatementNodeKind<'a>),
    IfStatement(BoundIfStatementNodeKind<'a>),
    VariableDeclaration(BoundVariableDeclarationNodeKind<'a>),
    WhileStatement(BoundWhileStatementNodeKind<'a>),
    Assignment(BoundAssignmentNodeKind<'a>),
    ExpressionStatement(BoundExpressionStatementNodeKind<'a>),
    ReturnStatement(BoundReturnStatementNodeKind<'a>),
}

#[derive(Debug, Clone)]
pub struct BoundFunctionDeclarationNodeKind<'a> {
    pub index: usize,
    pub is_main: bool,
    pub body: Box<BoundNode<'a>>,
    pub parameters: Vec<u64>,
}

#[derive(Debug, Clone)]
pub struct BoundJumpNodeKind<'a> {
    pub condition: Option<Box<BoundNode<'a>>>,
    pub target: Box<BoundNode<'a>>,
    pub jump_if_true: bool,
}

#[derive(Debug, Clone)]
pub struct BoundArrayLiteralNodeKind<'a> {
    pub children: Vec<BoundNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct BoundConstructorCallNodeKind<'a> {
    pub arguments: Vec<BoundNode<'a>>,
    pub base_type: StructType,
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
    pub has_this_argument: bool,
}

#[derive(Debug, Clone)]
pub struct BoundSystemCallNodeKind<'a> {
    pub base: SystemCallKind,
    pub arguments: Vec<BoundNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct BoundArrayIndexNodeKind<'a> {
    pub base: Box<BoundNode<'a>>,
    pub index: Box<BoundNode<'a>>,
}

#[derive(Debug, Clone)]
pub struct BoundFieldAccessNodeKind<'a> {
    pub base: Box<BoundNode<'a>>,
    pub offset: u64,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub struct BoundClosureNodeKind<'a> {
    pub base: Box<BoundNode<'a>>,
    pub function: FunctionKind,
}

impl<'a> BoundClosureNodeKind<'a> {
    pub fn arguments(&self) -> Vec<BoundNode<'a>> {
        vec![*self.base.clone()]
    }
}

#[derive(Debug, Clone)]
pub struct BoundConversionNodeKind<'a> {
    pub base: Box<BoundNode<'a>>,
    pub type_: Type,
}

#[derive(Debug, Clone, Copy)]
pub enum ConversionKind {
    None,
    Boxing,
    Deboxing,
}

impl BoundConversionNodeKind<'_> {
    pub fn kind(&self) -> ConversionKind {
        match (&self.type_, &self.base.type_) {
            (Type::Void, _) |
            (_, Type::Void) |
            (_, Type::Error) |
            (Type::Error, _) => unreachable!(),
            (Type::Any, _) => todo!(),
            (Type::Boolean, Type::None) |
            (Type::Boolean, Type::Noneable(_)) |
            (Type::SystemCall(_), Type::None) |
            (Type::SystemCall(_), Type::Noneable(_)) |
            (Type::Function(_), Type::None) |
            (Type::Function(_), Type::Noneable(_)) |
            (Type::Integer, Type::None) |
            (Type::Integer, Type::Noneable(_)) => ConversionKind::Deboxing,
            (Type::None, Type::Integer) |
            (Type::None, Type::Boolean) |
            (Type::None, Type::SystemCall(_)) |
            (Type::None, Type::Function(_)) |
            (Type::Noneable(_), Type::Integer) |
            (Type::Noneable(_), Type::Boolean) |
            (Type::Noneable(_), Type::SystemCall(_)) |
            (Type::Noneable(_), Type::Function(_)) => ConversionKind::Boxing,
            _ => ConversionKind::None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BoundIfStatementNodeKind<'a> {
    pub condition: Box<BoundNode<'a>>,
    pub body: Box<BoundNode<'a>>,
    pub else_body: Option<Box<BoundNode<'a>>>,
}

#[derive(Debug, Clone)]
pub struct BoundVariableDeclarationNodeKind<'a> {
    pub variable_index: u64,
    pub initializer: Box<BoundNode<'a>>,
    pub variable_type: Type,
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

#[derive(Debug, Clone)]
pub struct BoundReturnStatementNodeKind<'a> {
    pub expression: Option<Box<BoundNode<'a>>>,
    pub restores_variables: bool,
}
