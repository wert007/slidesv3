use super::*;

pub trait IsSameExpression {
    fn is_same_expression(&self, other: &Self) -> bool;
}

fn is_iterator_all_same_expression<T: IsSameExpression>(it: &[T], other: &[T]) -> bool {
    it.len() == other.len()
        && it
            .iter()
            .zip(other)
            .map(|(it, other)| it.is_same_expression(other))
            .reduce(|a, e| a && e)
            .unwrap_or(true)
}

fn is_option_same_expression<T: AsRef<impl IsSameExpression>>(
    it: &Option<T>,
    other: &Option<T>,
) -> bool {
    match (it, other) {
        (None, None) => true,
        (None, Some(_)) => false,
        (Some(_), None) => false,
        (Some(it), Some(other)) => it.as_ref().is_same_expression(other.as_ref()),
    }
}

impl IsSameExpression for BoundNode<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.kind.is_same_expression(&other.kind)
    }
}

impl IsSameExpression for BoundNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        match (self, other) {
            (BoundNodeKind::ErrorExpression, BoundNodeKind::ErrorExpression) => true,
            (
                BoundNodeKind::FunctionDeclaration(this),
                BoundNodeKind::FunctionDeclaration(other),
            ) => this.is_same_expression(other),
            (BoundNodeKind::Label(this), BoundNodeKind::Label(other)) => this == other,
            (BoundNodeKind::LabelReference(this), BoundNodeKind::LabelReference(other)) => {
                this == other
            }
            (BoundNodeKind::Jump(this), BoundNodeKind::Jump(other)) => {
                this.is_same_expression(other)
            }
            (BoundNodeKind::LiteralExpression(this), BoundNodeKind::LiteralExpression(other)) => {
                this.is_same_expression(other)
            }
            (
                BoundNodeKind::ArrayLiteralExpression(this),
                BoundNodeKind::ArrayLiteralExpression(other),
            ) => this.is_same_expression(other),
            (BoundNodeKind::ConstructorCall(this), BoundNodeKind::ConstructorCall(other)) => {
                this.is_same_expression(other)
            }
            (BoundNodeKind::VariableExpression(this), BoundNodeKind::VariableExpression(other)) => {
                this.is_same_expression(other)
            }
            (BoundNodeKind::UnaryExpression(this), BoundNodeKind::UnaryExpression(other)) => {
                this.is_same_expression(other)
            }
            (BoundNodeKind::BinaryExpression(this), BoundNodeKind::BinaryExpression(other)) => {
                this.is_same_expression(other)
            }
            (BoundNodeKind::FunctionCall(this), BoundNodeKind::FunctionCall(other)) => {
                this.is_same_expression(other)
            }
            (BoundNodeKind::SystemCall(this), BoundNodeKind::SystemCall(other)) => {
                this.is_same_expression(other)
            }
            (BoundNodeKind::ArrayIndex(this), BoundNodeKind::ArrayIndex(other)) => {
                this.is_same_expression(other)
            }
            (BoundNodeKind::FieldAccess(this), BoundNodeKind::FieldAccess(other)) => {
                this.is_same_expression(other)
            }
            (BoundNodeKind::Closure(this), BoundNodeKind::Closure(other)) => {
                this.is_same_expression(other)
            }
            (BoundNodeKind::Conversion(this), BoundNodeKind::Conversion(other)) => {
                this.is_same_expression(other)
            }
            (BoundNodeKind::BlockStatement(this), BoundNodeKind::BlockStatement(other)) => {
                this.is_same_expression(other)
            }
            (BoundNodeKind::IfStatement(this), BoundNodeKind::IfStatement(other)) => {
                this.is_same_expression(other)
            }
            (
                BoundNodeKind::VariableDeclaration(this),
                BoundNodeKind::VariableDeclaration(other),
            ) => this.is_same_expression(other),
            (BoundNodeKind::WhileStatement(this), BoundNodeKind::WhileStatement(other)) => {
                this.is_same_expression(other)
            }
            (BoundNodeKind::Assignment(this), BoundNodeKind::Assignment(other)) => {
                this.is_same_expression(other)
            }
            (
                BoundNodeKind::ExpressionStatement(this),
                BoundNodeKind::ExpressionStatement(other),
            ) => this.is_same_expression(other),
            (BoundNodeKind::ReturnStatement(this), BoundNodeKind::ReturnStatement(other)) => {
                this.is_same_expression(other)
            }
            _ => false,
        }
    }
}

impl IsSameExpression for BoundFunctionDeclarationNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.body.is_same_expression(&other.body)
            && self.index == other.index
            && self.is_main == other.is_main
            && self.parameters == other.parameters
    }
}

impl IsSameExpression for BoundJumpNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        is_option_same_expression(&self.condition, &other.condition)
            && self.jump_if_true == other.jump_if_true
            && self.target.is_same_expression(&other.target)
    }
}

impl IsSameExpression for LiteralNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl IsSameExpression for BoundArrayLiteralNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.children.len() == other.children.len()
            && is_iterator_all_same_expression(&self.children, &other.children)
    }
}

impl IsSameExpression for BoundConstructorCallNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.base_type == other.base_type
            && is_iterator_all_same_expression(&self.arguments, &other.arguments)
    }
}

impl IsSameExpression for BoundVariableNodeKind {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.variable_index == other.variable_index
    }
}

impl IsSameExpression for BoundUnaryNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.operand.is_same_expression(&other.operand)
            && self.operator_token == other.operator_token
    }
}

impl IsSameExpression for BoundBinaryNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.operator_token == other.operator_token
            && self.lhs.is_same_expression(&other.lhs)
            && self.rhs.is_same_expression(&other.rhs)
    }
}

impl IsSameExpression for BoundFunctionCallNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.has_this_argument == other.has_this_argument
            && self.base.is_same_expression(&other.base)
            && is_iterator_all_same_expression(&self.arguments, &other.arguments)
    }
}

impl IsSameExpression for BoundSystemCallNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.base == other.base
            && is_iterator_all_same_expression(&self.arguments, &other.arguments)
    }
}

impl IsSameExpression for BoundArrayIndexNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.base.is_same_expression(&other.base) && self.index.is_same_expression(&other.index)
    }
}

impl IsSameExpression for BoundFieldAccessNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.offset == other.offset
            && self.type_ == other.type_
            && self.base.is_same_expression(&other.base)
    }
}

impl IsSameExpression for BoundClosureNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.function == other.function && self.base.is_same_expression(&other.base)
    }
}

impl IsSameExpression for BoundConversionNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.type_ == other.type_ && self.base.is_same_expression(&other.base)
    }
}

impl IsSameExpression for BoundBlockStatementNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        is_iterator_all_same_expression(&self.statements, &other.statements)
    }
}

impl IsSameExpression for BoundIfStatementNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.condition.is_same_expression(&other.condition)
            && self.body.is_same_expression(&other.body)
            && is_option_same_expression(&self.else_body, &other.else_body)
    }
}

impl IsSameExpression for BoundVariableDeclarationNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.variable_type == other.variable_type
            && self.variable_index == other.variable_index
            && self.initializer.is_same_expression(&other.initializer)
    }
}

impl IsSameExpression for BoundWhileStatementNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.condition.is_same_expression(&other.condition)
            && self.body.is_same_expression(&other.body)
    }
}

impl IsSameExpression for BoundAssignmentNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.variable.is_same_expression(&other.variable)
            && self.expression.is_same_expression(&other.expression)
    }
}

impl IsSameExpression for BoundExpressionStatementNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.expression.is_same_expression(&other.expression)
    }
}

impl IsSameExpression for BoundReturnStatementNodeKind<'_> {
    fn is_same_expression(&self, other: &Self) -> bool {
        self.restores_variables == other.restores_variables
            && is_option_same_expression(&self.expression, &other.expression)
    }
}
