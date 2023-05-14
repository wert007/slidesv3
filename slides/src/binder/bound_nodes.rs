use crate::{text::TextLocation, value::Value};

use super::{
    operators::{BoundBinaryOperator, BoundUnaryOperator},
    symbols::StructFunctionTable,
    typing::{FunctionKind, SystemCallKind, Type, TypeCollection, TypeId},
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
    pub location: TextLocation,
    pub kind: BoundNodeKind,
    pub type_: TypeId,
    pub constant_value: Option<BoundConstant>,
}

impl BoundNode {
    pub fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        self.kind.for_each_child_mut(function);
    }

    pub fn error(span: TextLocation) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::ErrorExpression,
            type_: typeid!(Type::Error),
            constant_value: None,
        }
    }

    pub fn literal(span: TextLocation, value: Value) -> Self {
        let type_ = value.infer_type();
        Self {
            location: span,
            kind: BoundNodeKind::LiteralExpression(BoundLiteralNodeKind {
                value: value.clone(),
            }),
            type_,
            constant_value: Some(value.into()),
        }
    }

    pub fn array_literal(span: TextLocation, children: Vec<BoundNode>, type_: TypeId) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::ArrayLiteralExpression(BoundArrayLiteralNodeKind { children }),
            type_,
            constant_value: None,
        }
    }

    pub fn constructor_call(
        span: TextLocation,
        arguments: Vec<BoundNode>,
        base_type: TypeId,
        function: Option<u64>,
    ) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::ConstructorCall(Box::new(BoundConstructorCallNodeKind {
                arguments,
                base_type,
                function,
            })),
            type_: base_type,
            constant_value: None,
        }
    }

    pub fn variable(span: TextLocation, variable_index: u64, type_: TypeId) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::VariableExpression(BoundVariableNodeKind { variable_index }),
            type_,
            constant_value: None,
        }
    }

    pub fn binary(
        span: TextLocation,
        lhs: BoundNode,
        operator_token: BoundBinaryOperator,
        rhs: BoundNode,
        type_: TypeId,
    ) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::BinaryExpression(BoundBinaryNodeKind {
                lhs: Box::new(lhs),
                operator_token,
                rhs: Box::new(rhs),
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn logical_and(span: TextLocation, lhs: BoundNode, rhs: BoundNode) -> Self {
        let false_span = lhs.location;
        Self::if_expression(
            span,
            lhs,
            rhs,
            Some(Self::literal(false_span, Value::Boolean(false))),
            typeid!(Type::Boolean),
        )
    }

    pub fn logical_or(span: TextLocation, lhs: BoundNode, rhs: BoundNode) -> Self {
        let true_span = lhs.location;
        Self::if_expression(
            span,
            lhs,
            Self::literal(true_span, Value::Boolean(true)),
            Some(rhs),
            typeid!(Type::Boolean),
        )
    }

    pub fn unary(
        span: TextLocation,
        operator_token: BoundUnaryOperator,
        operand: BoundNode,
        type_: TypeId,
    ) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::UnaryExpression(BoundUnaryNodeKind {
                operator_token,
                operand: Box::new(operand),
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn function_call(
        span: TextLocation,
        base: BoundNode,
        arguments: Vec<BoundNode>,
        has_this_argument: bool,
        type_: TypeId,
    ) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::FunctionCall(BoundFunctionCallNodeKind {
                base: Box::new(base),
                arguments,
                has_this_argument,
                returns_value: type_ != typeid!(Type::Void),
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn array_index(
        span: TextLocation,
        base: BoundNode,
        index: BoundNode,
        type_: TypeId,
    ) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::ArrayIndex(BoundArrayIndexNodeKind {
                base: Box::new(base),
                index: Box::new(index),
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn field_access(span: TextLocation, base: BoundNode, offset: u64, type_: TypeId) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::FieldAccess(BoundFieldAccessNodeKind {
                base: Box::new(base),
                offset,
                type_: type_.clone(),
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn closure(span: TextLocation, arguments: Vec<BoundNode>, id: u64, type_: TypeId) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::Closure(BoundClosureNodeKind {
                arguments,
                function: FunctionKind::FunctionId(id),
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn closure_abstract(
        span: TextLocation,
        arguments: Vec<BoundNode>,
        vtable_index: usize,
        type_: TypeId,
    ) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::Closure(BoundClosureNodeKind {
                arguments,
                function: FunctionKind::VtableIndex(vtable_index),
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn closure_label(
        span: TextLocation,
        arguments: Vec<BoundNode>,
        label_index: usize,
        type_: TypeId,
    ) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::Closure(BoundClosureNodeKind {
                arguments,
                function: FunctionKind::LabelReference(label_index),
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn system_call_closure(
        span: TextLocation,
        arguments: Vec<BoundNode>,
        system_call_kind: SystemCallKind,
        type_: TypeId,
    ) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::Closure(BoundClosureNodeKind {
                arguments,
                function: FunctionKind::SystemCall(system_call_kind),
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn conversion(span: TextLocation, base: BoundNode, type_: TypeId) -> Self {
        Self {
            location: span,
            constant_value: base.constant_value.clone(),
            kind: BoundNodeKind::Conversion(BoundConversionNodeKind {
                base: Box::new(base),
                type_: type_.clone(),
            }),
            type_,
        }
    }

    pub fn system_call(
        span: TextLocation,
        base: SystemCallKind,
        arguments: Vec<BoundNode>,
        type_: TypeId,
    ) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::SystemCall(BoundSystemCallNodeKind { base, arguments }),
            type_,
            constant_value: None,
        }
    }

    pub fn while_statement(span: TextLocation, condition: BoundNode, body: BoundNode) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::WhileStatement(BoundWhileStatementNodeKind {
                condition: Box::new(condition),
                body: Box::new(body),
            }),
            type_: typeid!(Type::Void),
            constant_value: None,
        }
    }

    /// for i in iterator {
    ///     body
    /// }
    ///
    /// i$index = 0;
    /// while i$index < iterator.$elementCount() {
    ///     i = iterator.$get(i$index);
    ///     {
    ///         body
    ///     }
    ///     i$index += 1;
    /// }

    pub fn for_statement(
        span: TextLocation,
        index_variable: u64,
        collection_variable: u64,
        variable: BoundNode,
        collection: BoundNode,
        body: BoundNode,
        function_table: StructFunctionTable,
        _types: &TypeCollection,
    ) -> Self {
        let while_condition = BoundNode::binary(
            span,
            BoundNode::variable(
                span,
                index_variable,
                typeid!(Type::Integer(IntegerType::Unsigned64)),
            ),
            BoundBinaryOperator::LessThan,
            BoundNode::function_call(
                span,
                BoundNode::label_reference(
                    span,
                    function_table
                        .element_count_function
                        .as_ref()
                        .unwrap()
                        .function_label as usize,
                    function_table
                        .element_count_function
                        .as_ref()
                        .unwrap()
                        .function_type,
                ),
                vec![BoundNode::variable(
                    span,
                    collection_variable,
                    collection.type_.clone(),
                )],
                false,
                typeid!(Type::Integer(IntegerType::Unsigned64)),
            ),
            typeid!(Type::Boolean),
        );
        let while_body = BoundNode::block_statement(
            span,
            vec![
                BoundNode::assignment(
                    span,
                    variable.clone(),
                    BoundNode::function_call(
                        span,
                        BoundNode::label_reference(
                            span,
                            function_table.get_function.as_ref().unwrap().function_label as usize,
                            function_table.get_function.as_ref().unwrap().function_type,
                        ),
                        vec![
                            BoundNode::variable(
                                span,
                                index_variable,
                                typeid!(Type::Integer(IntegerType::Unsigned64)),
                            ),
                            BoundNode::variable(
                                span,
                                collection_variable,
                                collection.type_.clone(),
                            ),
                        ],
                        false,
                        variable.type_,
                    ),
                ),
                // {
                body,
                // }
                // $index = $index + 1;
                BoundNode::assignment(
                    span,
                    BoundNode::variable(
                        span,
                        index_variable,
                        typeid!(Type::Integer(IntegerType::Unsigned64)),
                    ),
                    BoundNode::binary(
                        span,
                        BoundNode::variable(
                            span,
                            index_variable,
                            typeid!(Type::Integer(IntegerType::Unsigned64)),
                        ),
                        BoundBinaryOperator::ArithmeticAddition,
                        BoundNode::literal(span, Value::Integer(1)),
                        typeid!(Type::Integer(IntegerType::Unsigned64)),
                    ),
                ),
            ],
        );
        BoundNode::block_statement(
            span,
            vec![
                BoundNode::variable_declaration(span, collection_variable, collection, None), // let $collection = collection;
                BoundNode::variable_declaration(
                    span,
                    index_variable,
                    BoundNode::literal(span, Value::Integer(0)),
                    None,
                ), // let $index = 0;
                BoundNode::while_statement(span, while_condition, while_body), // while $index < array length($collection)
            ],
        )
    }

    pub fn variable_declaration(
        span: TextLocation,
        variable_index: u64,
        initializer: BoundNode,
        variable_type: Option<TypeId>,
    ) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::VariableDeclaration(BoundVariableDeclarationNodeKind {
                variable_index,
                variable_type: variable_type.unwrap_or_else(|| initializer.type_.clone()),
                initializer: Box::new(initializer),
            }),
            type_: typeid!(Type::Void),
            constant_value: None,
        }
    }

    pub fn if_statement(
        span: TextLocation,
        condition: BoundNode,
        body: BoundNode,
        else_body: Option<BoundNode>,
    ) -> Self {
        Self::if_expression(span, condition, body, else_body, typeid!(Type::Void))
    }

    pub fn if_expression(
        span: TextLocation,
        condition: BoundNode,
        body: BoundNode,
        else_body: Option<BoundNode>,
        type_: TypeId,
    ) -> Self {
        if else_body.is_none() {
            assert_eq!(type_, typeid!(Type::Void));
        }
        Self {
            location: span,
            kind: BoundNodeKind::IfStatement(BoundIfStatementNodeKind {
                condition: Box::new(condition),
                body: Box::new(body),
                else_body: else_body.map(Box::new),
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn assignment(span: TextLocation, variable: BoundNode, expression: BoundNode) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::Assignment(BoundAssignmentNodeKind {
                variable: Box::new(variable),
                expression: Box::new(expression),
            }),
            type_: typeid!(Type::Void),
            constant_value: None,
        }
    }

    pub fn block_statement(span: TextLocation, statements: Vec<BoundNode>) -> Self {
        Self::block_expression(span, statements, typeid!(Type::Void))
    }

    pub fn block_expression(
        span: TextLocation,
        expressions: Vec<BoundNode>,
        type_: TypeId,
    ) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::BlockStatement(BoundBlockStatementNodeKind {
                statements: expressions,
            }),
            type_,
            constant_value: None,
        }
    }

    pub fn expression_statement(span: TextLocation, expression: BoundNode) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::ExpressionStatement(BoundExpressionStatementNodeKind {
                expression: Box::new(expression),
            }),
            type_: typeid!(Type::Void),
            constant_value: None,
        }
    }

    pub fn function_declaration(
        label: usize,
        is_main: bool,
        body: BoundNode,
        parameters: Vec<u64>,
        base_register: Option<u64>,
        function_type: TypeId,
    ) -> Self {
        Self {
            location: body.location,
            kind: BoundNodeKind::FunctionDeclaration(BoundFunctionDeclarationNodeKind {
                label,
                is_main,
                body: Box::new(body),
                parameters,
                base_register,
                function_type,
            }),
            type_: typeid!(Type::Void),
            constant_value: None,
        }
    }

    pub fn label_reference(location: TextLocation, index: usize, type_: TypeId) -> Self {
        Self {
            location,
            kind: BoundNodeKind::LabelReference(index),
            type_,
            constant_value: None,
        }
    }

    pub fn label(location: TextLocation, index: usize) -> Self {
        Self {
            location,
            kind: BoundNodeKind::Label(index),
            type_: typeid!(Type::Void),
            constant_value: None,
        }
    }

    pub fn jump(location: TextLocation, target: BoundNode) -> Self {
        Self {
            location,
            kind: BoundNodeKind::Jump(BoundJumpNodeKind {
                condition: None,
                target: Box::new(target),
                jump_if_true: true,
            }),
            type_: typeid!(Type::Void),
            constant_value: None,
        }
    }

    pub fn jump_if_true(location: TextLocation, condition: BoundNode, target: BoundNode) -> Self {
        Self {
            location,
            kind: BoundNodeKind::Jump(BoundJumpNodeKind {
                condition: Some(Box::new(condition)),
                target: Box::new(target),
                jump_if_true: true,
            }),
            type_: typeid!(Type::Void),
            constant_value: None,
        }
    }

    pub fn jump_if_false(location: TextLocation, condition: BoundNode, target: BoundNode) -> Self {
        Self {
            location,
            kind: BoundNodeKind::Jump(BoundJumpNodeKind {
                condition: Some(Box::new(condition)),
                target: Box::new(target),
                jump_if_true: false,
            }),
            type_: typeid!(Type::Void),
            constant_value: None,
        }
    }

    pub fn return_statement(
        span: TextLocation,
        expression: Option<BoundNode>,
        restores_variables: bool,
    ) -> Self {
        Self {
            location: span,
            kind: BoundNodeKind::ReturnStatement(BoundReturnStatementNodeKind {
                expression: expression.map(Box::new),
                restores_variables,
            }),
            type_: typeid!(Type::Void),
            constant_value: None,
        }
    }

    pub(crate) fn repetition_node(
        location: TextLocation,
        counting_variable: u64,
        expression: BoundNode,
        repetition: BoundNode,
    ) -> BoundNode {
        Self {
            location,
            type_: expression.type_,
            kind: BoundNodeKind::RepetitionNode(BoundRepetitionNodeKind {
                counting_variable,
                expression: Box::new(expression),
                repetition: Box::new(repetition),
            }),
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

    // Trivia
    RepetitionNode(BoundRepetitionNodeKind),

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

impl BoundNodeKind {
    pub fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        match self {
            BoundNodeKind::FunctionDeclaration(base) => base.for_each_child_mut(function),
            BoundNodeKind::ErrorExpression => {}
            BoundNodeKind::Label(_) => {}
            BoundNodeKind::LabelReference(_) => {}
            BoundNodeKind::Jump(base) => base.for_each_child_mut(function),
            BoundNodeKind::LiteralExpression(_) => {}
            BoundNodeKind::ArrayLiteralExpression(base) => base.for_each_child_mut(function),
            BoundNodeKind::ConstructorCall(base) => base.for_each_child_mut(function),
            BoundNodeKind::VariableExpression(_) => {}
            BoundNodeKind::UnaryExpression(base) => base.for_each_child_mut(function),
            BoundNodeKind::BinaryExpression(base) => base.for_each_child_mut(function),
            BoundNodeKind::FunctionCall(base) => base.for_each_child_mut(function),
            BoundNodeKind::SystemCall(base) => base.for_each_child_mut(function),
            BoundNodeKind::ArrayIndex(base) => base.for_each_child_mut(function),
            BoundNodeKind::FieldAccess(base) => base.for_each_child_mut(function),
            BoundNodeKind::Closure(base) => base.for_each_child_mut(function),
            BoundNodeKind::Conversion(base) => base.for_each_child_mut(function),
            BoundNodeKind::BlockStatement(base) => base.for_each_child_mut(function),
            BoundNodeKind::IfStatement(base) => base.for_each_child_mut(function),
            BoundNodeKind::VariableDeclaration(base) => base.for_each_child_mut(function),
            BoundNodeKind::WhileStatement(base) => base.for_each_child_mut(function),
            BoundNodeKind::Assignment(base) => base.for_each_child_mut(function),
            BoundNodeKind::ExpressionStatement(base) => base.for_each_child_mut(function),
            BoundNodeKind::ReturnStatement(base) => base.for_each_child_mut(function),
            BoundNodeKind::RepetitionNode(base) => base.for_each_child_mut(function),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BoundFunctionDeclarationNodeKind {
    pub label: usize,
    pub is_main: bool,
    pub body: Box<BoundNode>,
    pub parameters: Vec<u64>,
    pub base_register: Option<u64>,
    pub function_type: TypeId,
}

impl BoundFunctionDeclarationNodeKind {
    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        function(&mut self.body);
        self.body.for_each_child_mut(function);
    }
}

#[derive(Debug, Clone)]
pub struct BoundJumpNodeKind {
    pub condition: Option<Box<BoundNode>>,
    pub target: Box<BoundNode>,
    pub jump_if_true: bool,
}

impl BoundJumpNodeKind {
    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        if let Some(condition) = self.condition.as_mut() {
            function(condition);
            condition.for_each_child_mut(function);
        }
        function(&mut self.target);
        self.target.for_each_child_mut(function);
    }
}

#[derive(Debug, Clone)]
pub struct BoundRepetitionNodeKind {
    pub counting_variable: u64,
    pub expression: Box<BoundNode>,
    pub repetition: Box<BoundNode>,
}
impl BoundRepetitionNodeKind {
    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        function(&mut self.expression);
        self.expression.for_each_child_mut(function);
        function(&mut self.repetition);
        self.repetition.for_each_child_mut(function);
    }
}

#[derive(Debug, Clone)]
pub struct BoundLiteralNodeKind {
    pub value: Value,
}

#[derive(Debug, Clone)]
pub struct BoundArrayLiteralNodeKind {
    pub children: Vec<BoundNode>,
}

impl BoundArrayLiteralNodeKind {
    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        for child in self.children.iter_mut() {
            function(child);
            child.for_each_child_mut(function);
        }
    }
}

#[derive(Debug, Clone)]
pub struct BoundConstructorCallNodeKind {
    pub arguments: Vec<BoundNode>,
    pub base_type: TypeId,
    pub function: Option<u64>,
}

impl BoundConstructorCallNodeKind {
    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        for arg in self.arguments.iter_mut() {
            function(arg);
            arg.for_each_child_mut(function);
        }
    }
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

impl BoundUnaryNodeKind {
    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        function(&mut self.operand);
        self.operand.for_each_child_mut(function);
    }
}

#[derive(Debug, Clone)]
pub struct BoundBinaryNodeKind {
    pub lhs: Box<BoundNode>,
    pub operator_token: BoundBinaryOperator,
    pub rhs: Box<BoundNode>,
}

impl BoundBinaryNodeKind {
    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        function(&mut self.lhs);
        self.lhs.for_each_child_mut(function);
        function(&mut self.rhs);
        self.rhs.for_each_child_mut(function);
    }
}

#[derive(Debug, Clone)]
pub struct BoundFunctionCallNodeKind {
    pub base: Box<BoundNode>,
    pub arguments: Vec<BoundNode>,
    pub has_this_argument: bool,
    pub returns_value: bool,
}

impl BoundFunctionCallNodeKind {
    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        function(&mut self.base);
        self.base.for_each_child_mut(function);
        for arg in self.arguments.iter_mut() {
            function(arg);
            arg.for_each_child_mut(function);
        }
    }
}

#[derive(Debug, Clone)]
pub struct BoundSystemCallNodeKind {
    pub base: SystemCallKind,
    pub arguments: Vec<BoundNode>,
}

impl BoundSystemCallNodeKind {
    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        for arg in self.arguments.iter_mut() {
            function(arg);
            arg.for_each_child_mut(function);
        }
    }
}

#[derive(Debug, Clone)]
pub struct BoundArrayIndexNodeKind {
    pub base: Box<BoundNode>,
    pub index: Box<BoundNode>,
}

impl BoundArrayIndexNodeKind {
    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        function(&mut self.base);
        self.base.for_each_child_mut(function);
        function(&mut self.index);
        self.index.for_each_child_mut(function);
    }
}

#[derive(Debug, Clone)]
pub struct BoundFieldAccessNodeKind {
    pub base: Box<BoundNode>,
    pub offset: u64,
    pub type_: TypeId,
}

impl BoundFieldAccessNodeKind {
    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        function(&mut self.base);
        self.base.for_each_child_mut(function);
    }
}

#[derive(Debug, Clone)]
pub struct BoundClosureNodeKind {
    pub arguments: Vec<BoundNode>,
    pub function: FunctionKind,
}

impl BoundClosureNodeKind {
    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        for arg in self.arguments.iter_mut() {
            function(arg);
            arg.for_each_child_mut(function);
        }
    }
}

#[derive(Debug, Clone)]
pub struct BoundConversionNodeKind {
    pub base: Box<BoundNode>,
    pub type_: TypeId,
}

#[derive(Debug, Clone, Copy)]
pub enum ConversionKind {
    /// Conversion only on type level with no implications at runtime.
    None,
    /// Conversion ensures that current value always is a pointer.
    Boxing,
    /// Conversion negates previous [`ConversionKind::Boxing`] conversion.
    Unboxing,
    /// Conversion turns value into an any type, with a type identifier.
    TypeBoxing,
    /// Reads the type of an any type and gives also the raw value.
    TypeUnboxing,
    /// Turns signed ints to unsigned ints.
    IntToUint,
    /// Conversion turns value into its parent type with a vtable.
    AbstractTypeBoxing,
}

impl BoundConversionNodeKind {
    pub fn kind(&self, type_collection: &TypeCollection) -> ConversionKind {
        match (
            &type_collection[self.type_],
            &type_collection[self.base.type_],
        ) {
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
            | (Type::Integer(_), Type::None)
            | (Type::Integer(_), Type::Noneable(_))
            | (Type::IntegerLiteral, Type::None)
            | (Type::IntegerLiteral, Type::Noneable(_)) => ConversionKind::Unboxing,
            (Type::None, Type::Integer(_))
            | (Type::None, Type::IntegerLiteral)
            | (Type::None, Type::Boolean)
            | (Type::None, Type::SystemCall(_))
            | (Type::None, Type::Function(_))
            | (Type::Noneable(_), Type::IntegerLiteral)
            | (Type::Noneable(_), Type::Boolean)
            | (Type::Noneable(_), Type::SystemCall(_))
            | (Type::Noneable(_), Type::Function(_)) => ConversionKind::Boxing,
            (Type::Noneable(to), Type::Integer(from)) => {
                if let Type::Integer(to) = &type_collection[*to] {
                    if from.is_signed() && !to.is_signed() {
                        ConversionKind::IntToUint
                    } else {
                        ConversionKind::Boxing
                    }
                } else {
                    ConversionKind::Boxing
                }
            }
            (Type::Struct(_), Type::Struct(from)) => {
                if from.has_parent(self.type_) {
                    ConversionKind::AbstractTypeBoxing
                } else {
                    ConversionKind::None
                }
            }
            _ => ConversionKind::None,
        }
    }

    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        function(&mut self.base);
        self.base.for_each_child_mut(function);
    }
}

#[derive(Debug, Clone)]
pub struct BoundIfStatementNodeKind {
    pub condition: Box<BoundNode>,
    pub body: Box<BoundNode>,
    pub else_body: Option<Box<BoundNode>>,
}

impl BoundIfStatementNodeKind {
    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        function(&mut self.condition);
        self.condition.for_each_child_mut(function);
        function(&mut self.body);
        self.body.for_each_child_mut(function);
        if let Some(else_body) = self.else_body.as_mut() {
            function(else_body);
            else_body.for_each_child_mut(function);
        }
    }
}

#[derive(Debug, Clone)]
pub struct BoundVariableDeclarationNodeKind {
    pub variable_index: u64,
    pub initializer: Box<BoundNode>,
    pub variable_type: TypeId,
}

impl BoundVariableDeclarationNodeKind {
    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        function(&mut self.initializer);
        self.initializer.for_each_child_mut(function);
    }
}

#[derive(Debug, Clone)]
pub struct BoundWhileStatementNodeKind {
    pub condition: Box<BoundNode>,
    pub body: Box<BoundNode>,
}

impl BoundWhileStatementNodeKind {
    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        function(&mut self.condition);
        self.condition.for_each_child_mut(function);
        function(&mut self.body);
        self.body.for_each_child_mut(function);
    }
}

#[derive(Debug, Clone)]
pub struct BoundAssignmentNodeKind {
    pub variable: Box<BoundNode>,
    pub expression: Box<BoundNode>,
}

impl BoundAssignmentNodeKind {
    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        function(&mut self.variable);
        self.variable.for_each_child_mut(function);
        function(&mut self.expression);
        self.expression.for_each_child_mut(function);
    }
}

#[derive(Debug, Clone)]
pub struct BoundBlockStatementNodeKind {
    pub statements: Vec<BoundNode>,
}

impl BoundBlockStatementNodeKind {
    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        for statement in self.statements.iter_mut() {
            function(statement);
            statement.for_each_child_mut(function);
        }
    }
}

#[derive(Debug, Clone)]
pub struct BoundExpressionStatementNodeKind {
    pub expression: Box<BoundNode>,
}

impl BoundExpressionStatementNodeKind {
    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        function(&mut self.expression);
        self.expression.for_each_child_mut(function);
    }
}

#[derive(Debug, Clone)]
pub struct BoundReturnStatementNodeKind {
    pub expression: Option<Box<BoundNode>>,
    pub restores_variables: bool,
}

impl BoundReturnStatementNodeKind {
    fn for_each_child_mut(&mut self, function: &mut dyn FnMut(&mut BoundNode)) {
        if let Some(expression) = &mut self.expression {
            function(expression);
            expression.for_each_child_mut(function);
        }
    }
}
