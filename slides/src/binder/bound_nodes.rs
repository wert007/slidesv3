use crate::{text::TextSpan, value::Value};

use super::{
    operators::{BoundBinaryOperator, BoundUnaryOperator},
    typing::{FunctionKind, StructType, SystemCallKind, Type},
};

pub mod is_same_expression;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct BoundConstant {
    pub value: Value,
}

impl From<Value> for BoundConstant {
    fn from(value: Value) -> Self {
        Self { value }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct BoundNode {
    pub span: TextSpan,
    pub kind: BoundNodeKind,
    pub type_: Type,
    pub constant_value: Option<BoundConstant>,
}

impl BoundNode {
    pub fn error(span: TextSpan) -> Self {
        Self {
            span,
            kind: BoundNodeKind::ErrorExpression,
            type_: Type::Error,
            constant_value: None,
        }
    }

    pub fn literal_from_value(value: Value) -> Self {
        let type_ = value.infer_type();
        Self {
            span: TextSpan::zero(),
            kind: BoundNodeKind::LiteralExpression(BoundLiteralNodeKind {
                value: value.clone(),
            }),
            type_,
            constant_value: Some(value.into()),
        }
    }

    pub fn literal(span: TextSpan, value: Value) -> Self {
        let type_ = value.infer_type();
        Self {
            span,
            kind: BoundNodeKind::LiteralExpression(BoundLiteralNodeKind {
                value: value.clone(),
            }),
            type_,
            constant_value: Some(value.into()),
        }
    }

    pub fn array_literal(span: TextSpan, children: Vec<BoundNode>, type_: Type) -> Self {
        Self {
            span,
            kind: BoundNodeKind::ArrayLiteralExpression(BoundArrayLiteralNodeKind { children }),
            type_,
            constant_value: None,
        }
    }

    pub fn constructor_call(
        span: TextSpan,
        arguments: Vec<BoundNode>,
        base_type: StructType,
        function: Option<u64>,
    ) -> Self {
        Self {
            span,
            kind: BoundNodeKind::ConstructorCall(Box::new(BoundConstructorCallNodeKind {
                arguments,
                base_type: base_type.clone(),
                function,
            })),
            type_: Type::Struct(Box::new(base_type)),
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
        lhs: BoundNode,
        operator_token: BoundBinaryOperator,
        rhs: BoundNode,
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
        operand: BoundNode,
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
        base: BoundNode,
        arguments: Vec<BoundNode>,
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
            constant_value: None,
        }
    }

    pub fn array_index(
        span: TextSpan,
        base: BoundNode,
        index: BoundNode,
        type_: Type,
    ) -> Self {
        Self {
            span,
            kind: BoundNodeKind::ArrayIndex(BoundArrayIndexNodeKind {
                base: Box::new(base),
                index: Box::new(index),
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn field_access(span: TextSpan, base: BoundNode, offset: u64, type_: Type) -> Self {
        Self {
            span,
            kind: BoundNodeKind::FieldAccess(BoundFieldAccessNodeKind {
                base: Box::new(base),
                offset,
                type_: type_.clone(),
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn closure(span: TextSpan, arguments: Vec<BoundNode>, id: u64, type_: Type) -> Self {
        Self {
            span,
            kind: BoundNodeKind::Closure(BoundClosureNodeKind {
                arguments,
                function: FunctionKind::FunctionId(id),
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn closure_label(
        span: TextSpan,
        arguments: Vec<BoundNode>,
        label_index: usize,
        type_: Type,
    ) -> Self {
        Self {
            span,
            kind: BoundNodeKind::Closure(BoundClosureNodeKind {
                arguments,
                function: FunctionKind::LabelReference(label_index),
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn system_call_closure(
        span: TextSpan,
        arguments: Vec<BoundNode>,
        system_call_kind: SystemCallKind,
        type_: Type,
    ) -> Self {
        Self {
            span,
            kind: BoundNodeKind::Closure(BoundClosureNodeKind {
                arguments,
                function: FunctionKind::SystemCall(system_call_kind),
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn conversion(span: TextSpan, base: BoundNode, type_: Type) -> Self {
        Self {
            span,
            constant_value: base.constant_value.clone(),
            kind: BoundNodeKind::Conversion(BoundConversionNodeKind {
                base: Box::new(base),
                type_: type_.clone(),
            }),
            type_,
        }
    }

    pub fn system_call(
        span: TextSpan,
        base: SystemCallKind,
        arguments: Vec<BoundNode>,
        type_: Type,
    ) -> Self {
        Self {
            span,
            kind: BoundNodeKind::SystemCall(BoundSystemCallNodeKind { base, arguments }),
            type_,
            constant_value: None,
        }
    }

    pub fn while_statement(span: TextSpan, condition: BoundNode, body: BoundNode) -> Self {
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

    pub fn for_statement_array(
        span: TextSpan,
        index_variable: u64,
        collection_variable: u64,
        variable: BoundNode,
        collection: BoundNode,
        body: BoundNode,
    ) -> Self {
        let while_condition = BoundNode::binary(
            span,
            BoundNode::variable(span, index_variable, Type::Integer),
            BoundBinaryOperator::LessThan,
            BoundNode::system_call(
                span,
                SystemCallKind::ArrayLength,
                vec![BoundNode::conversion(
                    span,
                    BoundNode::variable(span, collection_variable, collection.type_.clone()),
                    Type::Any,
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

    /// for i in range {
    ///     body
    /// }
    ///
    /// i = range.start;
    /// i$index = 0;
    /// while range.hasNextValue(i) {
    ///     {
    ///         body
    ///     }
    ///     i += range.step_by;
    ///     i$index += 1;
    /// }

    pub fn for_statement_range(
        span: TextSpan,
        index_variable: u64,
        collection_variable: u64,
        variable: BoundNode,
        collection: BoundNode,
        body: BoundNode,
        range_has_next_value_function_base: BoundNode,
    ) -> Self {
        let while_condition = BoundNode::function_call(
            span,
            range_has_next_value_function_base,
            vec![
                variable.clone(),
                BoundNode::variable(span, collection_variable, collection.type_.clone()),
            ],
            // FIXME: Actually true, but the this argument is normally not in
            // the parameter list.
            false,
            Type::Boolean,
        );
        let while_body = BoundNode::block_statement(
            span,
            vec![
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
                // variable = variable + $collection.step_by;
                BoundNode::assignment(
                    span,
                    variable.clone(),
                    BoundNode::binary(
                        span,
                        variable.clone(),
                        BoundBinaryOperator::ArithmeticAddition,
                        BoundNode::field_access(
                            span,
                            BoundNode::variable(
                                span,
                                collection_variable,
                                collection.type_.clone(),
                            ),
                            16,
                            Type::Integer,
                        ),
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
                    collection_variable,
                    collection.clone(),
                    None,
                ), // let $collection = collection;
                BoundNode::assignment(
                    span,
                    variable,
                    BoundNode::field_access(
                        span,
                        BoundNode::variable(span, collection_variable, collection.type_.clone()),
                        0,
                        Type::Integer,
                    ),
                ),
                BoundNode::variable_declaration(
                    span,
                    index_variable,
                    BoundNode::literal_from_value(Value::Integer(0)),
                    None,
                ), // let $index = 0;
                BoundNode::while_statement(span, while_condition, while_body), // while $index < array length($collection)
            ],
        )
    }

    pub fn variable_declaration(
        span: TextSpan,
        variable_index: u64,
        initializer: BoundNode,
        variable_type: Option<Type>,
    ) -> Self {
        Self {
            span,
            kind: BoundNodeKind::VariableDeclaration(BoundVariableDeclarationNodeKind {
                variable_index,
                variable_type: variable_type.unwrap_or_else(|| initializer.type_.clone()),
                initializer: Box::new(initializer),
            }),
            type_: Type::Void,
            constant_value: None,
        }
    }

    pub fn if_statement(
        span: TextSpan,
        condition: BoundNode,
        body: BoundNode,
        else_body: Option<BoundNode>,
    ) -> Self {
        Self::if_expression(span, condition, body, else_body, Type::Void)
    }

    pub fn if_expression(
        span: TextSpan,
        condition: BoundNode,
        body: BoundNode,
        else_body: Option<BoundNode>,
        type_: Type,
    ) -> Self {
        if else_body.is_none() {
            assert_eq!(type_, Type::Void);
        }
        Self {
            span,
            kind: BoundNodeKind::IfStatement(BoundIfStatementNodeKind {
                condition: Box::new(condition),
                body: Box::new(body),
                else_body: else_body.map(Box::new),
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn assignment(span: TextSpan, variable: BoundNode, expression: BoundNode) -> Self {
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

    pub fn block_statement(span: TextSpan, statements: Vec<BoundNode>) -> Self {
        Self::block_expression(span, statements, Type::Void)
    }

    pub fn block_expression(span: TextSpan, expressions: Vec<BoundNode>, type_: Type) -> Self {
        Self {
            span,
            kind: BoundNodeKind::BlockStatement(BoundBlockStatementNodeKind {
                statements: expressions,
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn expression_statement(span: TextSpan, expression: BoundNode) -> Self {
        Self {
            span,
            kind: BoundNodeKind::ExpressionStatement(BoundExpressionStatementNodeKind {
                expression: Box::new(expression),
            }),
            type_: Type::Void,
            constant_value: None,
        }
    }

    pub fn function_declaration(
        label: usize,
        is_main: bool,
        body: BoundNode,
        parameters: Vec<u64>,
    ) -> Self {
        Self {
            span: body.span,
            kind: BoundNodeKind::FunctionDeclaration(BoundFunctionDeclarationNodeKind {
                label,
                is_main,
                body: Box::new(body),
                parameters,
            }),
            type_: Type::Void,
            constant_value: None,
        }
    }

    pub fn label_reference(index: usize, type_: Type) -> Self {
        Self {
            span: TextSpan::zero(),
            kind: BoundNodeKind::LabelReference(index),
            type_,
            constant_value: None,
        }
    }

    pub fn label(index: usize) -> Self {
        Self {
            span: TextSpan::zero(),
            kind: BoundNodeKind::Label(index),
            type_: Type::Void,
            constant_value: None,
        }
    }

    pub fn jump(target: BoundNode) -> Self {
        Self {
            span: TextSpan::zero(),
            kind: BoundNodeKind::Jump(BoundJumpNodeKind {
                condition: None,
                target: Box::new(target),
                jump_if_true: true,
            }),
            type_: Type::Void,
            constant_value: None,
        }
    }

    pub fn jump_if_true(condition: BoundNode, target: BoundNode) -> Self {
        Self {
            span: TextSpan::zero(),
            kind: BoundNodeKind::Jump(BoundJumpNodeKind {
                condition: Some(Box::new(condition)),
                target: Box::new(target),
                jump_if_true: true,
            }),
            type_: Type::Void,
            constant_value: None,
        }
    }

    pub fn jump_if_false(condition: BoundNode, target: BoundNode) -> Self {
        Self {
            span: TextSpan::zero(),
            kind: BoundNodeKind::Jump(BoundJumpNodeKind {
                condition: Some(Box::new(condition)),
                target: Box::new(target),
                jump_if_true: false,
            }),
            type_: Type::Void,
            constant_value: None,
        }
    }

    pub fn return_statement(
        span: TextSpan,
        expression: Option<BoundNode>,
        restores_variables: bool,
    ) -> Self {
        Self {
            span,
            kind: BoundNodeKind::ReturnStatement(BoundReturnStatementNodeKind {
                expression: expression.map(Box::new),
                restores_variables,
            }),
            type_: Type::Void,
            constant_value: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum BoundNodeKind {
    // Top Level Statements
    FunctionDeclaration(BoundFunctionDeclarationNodeKind),
    // Generated
    ErrorExpression,
    Label(usize),
    LabelReference(usize),
    Jump(BoundJumpNodeKind),
    // Expressions
    LiteralExpression(BoundLiteralNodeKind),
    ArrayLiteralExpression(BoundArrayLiteralNodeKind),
    ConstructorCall(Box<BoundConstructorCallNodeKind>),
    VariableExpression(BoundVariableNodeKind),
    UnaryExpression(BoundUnaryNodeKind),
    BinaryExpression(BoundBinaryNodeKind),
    FunctionCall(BoundFunctionCallNodeKind),
    SystemCall(BoundSystemCallNodeKind),
    ArrayIndex(BoundArrayIndexNodeKind),
    FieldAccess(BoundFieldAccessNodeKind),
    Closure(BoundClosureNodeKind),
    Conversion(BoundConversionNodeKind),

    // Statements
    BlockStatement(BoundBlockStatementNodeKind),
    IfStatement(BoundIfStatementNodeKind),
    VariableDeclaration(BoundVariableDeclarationNodeKind),
    WhileStatement(BoundWhileStatementNodeKind),
    Assignment(BoundAssignmentNodeKind),
    ExpressionStatement(BoundExpressionStatementNodeKind),
    ReturnStatement(BoundReturnStatementNodeKind),
}

#[derive(Debug, Clone)]
pub struct BoundFunctionDeclarationNodeKind {
    pub label: usize,
    pub is_main: bool,
    pub body: Box<BoundNode>,
    pub parameters: Vec<u64>,
}

#[derive(Debug, Clone)]
pub struct BoundJumpNodeKind {
    pub condition: Option<Box<BoundNode>>,
    pub target: Box<BoundNode>,
    pub jump_if_true: bool,
}


#[derive(Debug, Clone)]
pub struct BoundLiteralNodeKind {
    pub value: Value,
}


#[derive(Debug, Clone)]
pub struct BoundArrayLiteralNodeKind {
    pub children: Vec<BoundNode>,
}

#[derive(Debug, Clone)]
pub struct BoundConstructorCallNodeKind {
    pub arguments: Vec<BoundNode>,
    pub base_type: StructType,
    pub function: Option<u64>,
}

#[derive(Debug, Clone)]
pub struct BoundVariableNodeKind {
    pub variable_index: u64,
}

#[derive(Debug, Clone)]
pub struct BoundUnaryNodeKind {
    pub operator_token: BoundUnaryOperator,
    pub operand: Box<BoundNode>,
}

#[derive(Debug, Clone)]
pub struct BoundBinaryNodeKind {
    pub lhs: Box<BoundNode>,
    pub operator_token: BoundBinaryOperator,
    pub rhs: Box<BoundNode>,
}

#[derive(Debug, Clone)]
pub struct BoundFunctionCallNodeKind {
    pub base: Box<BoundNode>,
    pub arguments: Vec<BoundNode>,
    pub has_this_argument: bool,
}

#[derive(Debug, Clone)]
pub struct BoundSystemCallNodeKind {
    pub base: SystemCallKind,
    pub arguments: Vec<BoundNode>,
}

#[derive(Debug, Clone)]
pub struct BoundArrayIndexNodeKind {
    pub base: Box<BoundNode>,
    pub index: Box<BoundNode>,
}

#[derive(Debug, Clone)]
pub struct BoundFieldAccessNodeKind {
    pub base: Box<BoundNode>,
    pub offset: u64,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub struct BoundClosureNodeKind {
    pub arguments: Vec<BoundNode>,
    pub function: FunctionKind,
}

#[derive(Debug, Clone)]
pub struct BoundConversionNodeKind {
    pub base: Box<BoundNode>,
    pub type_: Type,
}

#[derive(Debug, Clone, Copy)]
pub enum ConversionKind {
    None,
    Boxing,
    Unboxing,
    TypeBoxing,
    TypeUnboxing,
}

impl BoundConversionNodeKind {
    pub fn kind(&self) -> ConversionKind {
        match (&self.type_, &self.base.type_) {
            (Type::Void, _) | (_, Type::Void) | (_, Type::Error) | (Type::Error, _) => {
                unreachable!()
            }
            (_, Type::Any) => ConversionKind::TypeUnboxing,
            (Type::Any, _) => ConversionKind::TypeBoxing,
            (Type::Boolean, Type::None)
            | (Type::Boolean, Type::Noneable(_))
            | (Type::SystemCall(_), Type::None)
            | (Type::SystemCall(_), Type::Noneable(_))
            | (Type::Function(_), Type::None)
            | (Type::Function(_), Type::Noneable(_))
            | (Type::Integer, Type::None)
            | (Type::Integer, Type::Noneable(_)) => ConversionKind::Unboxing,
            (Type::None, Type::Integer)
            | (Type::None, Type::Boolean)
            | (Type::None, Type::SystemCall(_))
            | (Type::None, Type::Function(_))
            | (Type::Noneable(_), Type::Integer)
            | (Type::Noneable(_), Type::Boolean)
            | (Type::Noneable(_), Type::SystemCall(_))
            | (Type::Noneable(_), Type::Function(_)) => ConversionKind::Boxing,
            _ => ConversionKind::None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BoundIfStatementNodeKind {
    pub condition: Box<BoundNode>,
    pub body: Box<BoundNode>,
    pub else_body: Option<Box<BoundNode>>,
}

#[derive(Debug, Clone)]
pub struct BoundVariableDeclarationNodeKind {
    pub variable_index: u64,
    pub initializer: Box<BoundNode>,
    pub variable_type: Type,
}

#[derive(Debug, Clone)]
pub struct BoundWhileStatementNodeKind {
    pub condition: Box<BoundNode>,
    pub body: Box<BoundNode>,
}

#[derive(Debug, Clone)]
pub struct BoundAssignmentNodeKind {
    pub variable: Box<BoundNode>,
    pub expression: Box<BoundNode>,
}

#[derive(Debug, Clone)]
pub struct BoundBlockStatementNodeKind {
    pub statements: Vec<BoundNode>,
}

#[derive(Debug, Clone)]
pub struct BoundExpressionStatementNodeKind {
    pub expression: Box<BoundNode>,
}

#[derive(Debug, Clone)]
pub struct BoundReturnStatementNodeKind {
    pub expression: Option<Box<BoundNode>>,
    pub restores_variables: bool,
}
