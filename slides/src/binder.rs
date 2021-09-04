pub mod bound_nodes;
pub mod operators;
#[cfg(test)]
mod tests;
pub mod typing;

use std::collections::HashMap;

use crate::{
    binder::{operators::BoundUnaryOperator, typing::Type},
    diagnostics::DiagnosticBag,
    lexer::syntax_token::{SyntaxToken, SyntaxTokenKind},
    parser::{
        self,
        syntax_nodes::{
            ArrayIndexNodeKind, ArrayLiteralNodeKind, AssignmentNodeKind, BinaryNodeKind,
            BlockStatementNodeKind, ExpressionStatementNodeKind, FunctionCallNodeKind,
            IfStatementNodeKind, LiteralNodeKind, ParenthesizedNodeKind, SyntaxNode,
            SyntaxNodeKind, UnaryNodeKind, VariableDeclarationNodeKind, VariableNodeKind,
            WhileStatementNodeKind,
        },
    },
    text::{SourceText, TextSpan},
    value::Value,
    DebugFlags,
};

use self::{
    bound_nodes::BoundNode,
    operators::BoundBinaryOperator,
    typing::{FunctionType, SystemCallKind},
};

struct BoundVariableName<'a> {
    pub identifier: &'a str,
    pub type_: Type,
}

struct BindingState<'a, 'b> {
    diagnostic_bag: &'b mut DiagnosticBag<'a>,
    variable_table: HashMap<u64, BoundVariableName<'a>>,
    print_variable_table: bool,
}

impl<'a> BindingState<'a, '_> {
    fn register_variable(&mut self, name: &'a str, type_: Type) -> Option<u64> {
        let index = self.variable_table.len() as u64;
        let variable_already_registered = self
            .variable_table
            .values()
            .any(|variable| variable.identifier == name);
        if variable_already_registered {
            None
        } else {
            self.variable_table.insert(
                index,
                BoundVariableName {
                    identifier: name,
                    type_,
                },
            );
            Some(index)
        }
    }

    fn delete_variables_until(&mut self, index: usize) {
        if self.variable_table.is_empty() {
            return;
        }
        let current = self.variable_table.len() - 1;
        for i in index..=current {
            self.variable_table.remove(&(i as _));
        }
    }

    fn look_up_variable_by_name(&self, name: &str) -> Option<(u64, Type)> {
        self.variable_table
            .iter()
            .find(|(_, v)| v.identifier == name)
            .map(|(&i, v)| (i, v.type_.clone()))
    }
}

fn print_variable_table(variable_table: &HashMap<u64, BoundVariableName>) {
    let mut variable_table: Vec<_> = variable_table.iter().collect();
    variable_table.sort_unstable_by_key(|f| f.0);
    for (index, variable) in variable_table {
        println!(
            "  {:00}: {} : {}",
            index, variable.identifier, variable.type_
        );
    }
    println!();
}

pub fn bind<'a>(
    source_text: &'a SourceText<'a>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
    debug_flags: DebugFlags,
) -> BoundNode<'a> {
    let node = parser::parse(source_text, diagnostic_bag, debug_flags);
    if diagnostic_bag.has_errors() {
        return BoundNode::error(TextSpan::new(0, 0));
    }
    let mut binder = BindingState {
        diagnostic_bag,
        variable_table: HashMap::new(),
        print_variable_table: debug_flags.print_variable_table(),
    };
    let mut statements = default_statements(&mut binder);
    let span = node.span();
    statements.push(bind_node(node, &mut binder));
    BoundNode::block_statement(span, statements)
}

fn default_statements<'a, 'b>(binder: &mut BindingState<'a, 'b>) -> Vec<BoundNode<'a>> {
    let span = TextSpan::new(0, 0);
    let variable_index = binder
        .register_variable("print", Type::SystemCall(SystemCallKind::Print))
        .unwrap();
    let token = SyntaxToken {
        kind: SyntaxTokenKind::Eoi,
        lexeme: "",
        start: 0,
    };
    let print_statement = BoundNode::variable_declaration(
        span,
        variable_index,
        BoundNode::literal(
            span,
            LiteralNodeKind {
                token,
                value: Value::SystemCall(SystemCallKind::Print),
            },
        ),
    );
    vec![print_statement]
}

fn bind_node<'a, 'b>(node: SyntaxNode<'a>, binder: &mut BindingState<'a, 'b>) -> BoundNode<'a> {
    match node.kind {
        SyntaxNodeKind::Literal(literal) => bind_literal(node.span, literal, binder),
        SyntaxNodeKind::ArrayLiteral(array_literal) => {
            bind_array_literal(node.span, array_literal, binder)
        }
        SyntaxNodeKind::Variable(variable) => bind_variable(node.span, variable, binder),
        SyntaxNodeKind::Binary(binary) => bind_binary(node.span, binary, binder),
        SyntaxNodeKind::Unary(unary) => bind_unary(node.span, unary, binder),
        SyntaxNodeKind::Parenthesized(parenthesized) => {
            bind_parenthesized(node.span, parenthesized, binder)
        }
        SyntaxNodeKind::FunctionCall(function_call) => {
            bind_function_call(node.span, function_call, binder)
        }
        SyntaxNodeKind::ArrayIndex(array_index) => bind_array_index(node.span, array_index, binder),
        SyntaxNodeKind::BlockStatement(block_statement) => {
            bind_block_statement(node.span, block_statement, binder)
        }
        SyntaxNodeKind::IfStatement(if_statement) => {
            bind_if_statement(node.span, if_statement, binder)
        }
        SyntaxNodeKind::VariableDeclaration(variable_declaration) => {
            bind_variable_declaration(node.span, variable_declaration, binder)
        }
        SyntaxNodeKind::WhileStatement(while_statement) => {
            bind_while_statement(node.span, while_statement, binder)
        }
        SyntaxNodeKind::Assignment(assignment) => bind_assignment(node.span, assignment, binder),
        SyntaxNodeKind::ExpressionStatement(expression_statement) => {
            bind_expression_statement(node.span, expression_statement, binder)
        }
    }
}

fn bind_literal<'a>(
    span: TextSpan,
    literal: LiteralNodeKind<'a>,
    _: &mut BindingState,
) -> BoundNode<'a> {
    BoundNode::literal(span, literal)
}

fn bind_array_literal<'a, 'b>(
    span: TextSpan,
    mut array_literal: ArrayLiteralNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let first_element = array_literal.children.remove(0);
    let first_element = bind_node(first_element, binder);
    let type_ = first_element.type_.clone();
    let mut children = vec![first_element];
    for child in array_literal.children {
        let child_span = child.span;
        let child = bind_node(child, binder);
        if child.type_.can_be_converted_to(&type_) {
            children.push(child);
        } else {
            binder
                .diagnostic_bag
                .report_cannot_convert(child_span, &child.type_, &type_);
            children.push(BoundNode::error(child_span));
        }
    }
    BoundNode::array_literal(span, children, Type::array(type_))
}

fn bind_variable<'a, 'b>(
    span: TextSpan,
    variable: VariableNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    match binder.look_up_variable_by_name(variable.token.lexeme) {
        Some((index, type_)) => BoundNode::variable(span, index, type_),
        None => {
            binder
                .diagnostic_bag
                .report_variable_not_found(span, variable.token.lexeme);
            BoundNode::error(span)
        }
    }
}

fn bind_binary<'a, 'b>(
    span: TextSpan,
    binary: BinaryNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let lhs = bind_node(*binary.lhs, binder);
    let rhs = bind_node(*binary.rhs, binder);
    match bind_binary_operator(span, &lhs, binary.operator_token, &rhs, binder) {
        Some((operator_token, type_)) => BoundNode::binary(span, lhs, operator_token, rhs, type_),
        None => BoundNode::error(span),
    }
}

fn bind_binary_operator<'a, 'b>(
    span: TextSpan,
    lhs: &BoundNode,
    operator_token: SyntaxToken,
    rhs: &BoundNode,
    binder: &mut BindingState<'a, 'b>,
) -> Option<(BoundBinaryOperator, Type)> {
    let result = match operator_token.kind {
        SyntaxTokenKind::Plus => BoundBinaryOperator::ArithmeticAddition,
        SyntaxTokenKind::Minus => BoundBinaryOperator::ArithmeticSubtraction,
        SyntaxTokenKind::Star => BoundBinaryOperator::ArithmeticMultiplication,
        SyntaxTokenKind::Slash => BoundBinaryOperator::ArithmeticDivision,
        SyntaxTokenKind::EqualsEquals => BoundBinaryOperator::Equals,
        SyntaxTokenKind::BangEquals => BoundBinaryOperator::NotEquals,
        SyntaxTokenKind::LessThan => BoundBinaryOperator::LessThan,
        SyntaxTokenKind::GreaterThan => BoundBinaryOperator::GreaterThan,
        SyntaxTokenKind::LessThanEquals => BoundBinaryOperator::LessThanEquals,
        SyntaxTokenKind::GreaterThanEquals => BoundBinaryOperator::GreaterThanEquals,
        _ => unreachable!(),
    };
    match (&lhs.type_, result, &rhs.type_) {
        (lhs, BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals, rhs)
            if lhs == rhs && lhs != &Type::Void =>
        {
            Some((result, Type::Boolean))
        }
        (
            Type::Integer,
            BoundBinaryOperator::ArithmeticAddition
            | BoundBinaryOperator::ArithmeticSubtraction
            | BoundBinaryOperator::ArithmeticMultiplication
            | BoundBinaryOperator::ArithmeticDivision,
            Type::Integer,
        ) => Some((result, Type::Integer)),
        (
            Type::Integer,
            BoundBinaryOperator::LessThan
            | BoundBinaryOperator::GreaterThan
            | BoundBinaryOperator::LessThanEquals
            | BoundBinaryOperator::GreaterThanEquals,
            Type::Integer,
        ) => Some((result, Type::Boolean)),
        (Type::Error, _, _) | (_, _, Type::Error) => None,
        _ => {
            binder.diagnostic_bag.report_no_binary_operator(
                span,
                &lhs.type_,
                operator_token.lexeme,
                &rhs.type_,
            );
            None
        }
    }
}

fn bind_unary<'a, 'b>(
    span: TextSpan,
    unary: UnaryNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let operand = bind_node(*unary.operand, binder);
    match bind_unary_operator(span, &operand, unary.operator_token, binder) {
        Some((operator_token, type_)) => BoundNode::unary(span, operator_token, operand, type_),
        None => BoundNode::error(span),
    }
}

fn bind_unary_operator<'a, 'b>(
    span: TextSpan,
    operand: &BoundNode,
    operator_token: SyntaxToken,
    binder: &mut BindingState<'a, 'b>,
) -> Option<(BoundUnaryOperator, Type)> {
    let result = match &operator_token.kind {
        SyntaxTokenKind::Plus => BoundUnaryOperator::ArithmeticIdentity,
        SyntaxTokenKind::Minus => BoundUnaryOperator::ArithmeticNegate,
        SyntaxTokenKind::Bang => todo!(),
        _ => unreachable!(),
    };
    match operand.type_ {
        Type::Integer => Some((result, Type::Integer)),
        Type::Error
        | Type::Void
        | Type::Any
        | Type::SystemCall(_)
        | Type::Boolean
        | Type::String
        | Type::Array(_) => {
            binder.diagnostic_bag.report_no_unary_operator(
                span,
                operator_token.lexeme,
                &operand.type_,
            );
            None
        }
    }
}

fn bind_parenthesized<'a, 'b>(
    _: TextSpan,
    parenthesized: ParenthesizedNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    bind_node(*parenthesized.expression, binder)
}

fn function_type(type_: &Type) -> FunctionType {
    match type_ {
        Type::SystemCall(SystemCallKind::Print) => FunctionType {
            parameter_types: vec![Type::Any],
            return_type: Type::Void,
            system_call_kind: type_.as_system_call(),
        },

        _ => unimplemented!(),
    }
}

fn bind_function_call<'a, 'b>(
    span: TextSpan,
    function_call: FunctionCallNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let argument_span = function_call.argument_span();
    let base = bind_node(*function_call.base, binder);
    let function_type = function_type(&base.type_);
    let arguments = bind_arguments_for_function(
        argument_span,
        function_call.arguments,
        &function_type,
        binder,
    );
    if let Type::SystemCall(system_call) = base.type_ {
        return BoundNode::system_call(span, system_call, arguments, function_type.return_type);
    }
    BoundNode::function_call(span, base, arguments, function_type.return_type)
}

fn bind_array_index<'a, 'b>(
    span: TextSpan,
    array_index: ArrayIndexNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let index_span = array_index.index.span;
    let index = bind_node(*array_index.index, binder);
    if !index.type_.can_be_converted_to(&Type::Integer) {
        binder
            .diagnostic_bag
            .report_cannot_convert(index_span, &index.type_, &Type::Integer);
    }
    let base_span = array_index.base.span;
    let base = bind_node(*array_index.base, binder);
    let type_ = match base.type_.clone() {
        Type::Array(base_type) => *base_type,
        error => {
            binder.diagnostic_bag.report_cannot_convert(
                base_span,
                &base.type_,
                &Type::array(error),
            );
            Type::Error
        }
    };
    BoundNode::array_index(span, base, index, type_)
}

fn bind_arguments_for_function<'a, 'b>(
    span: TextSpan,
    arguments: Vec<SyntaxNode<'a>>,
    function_type: &FunctionType,
    binder: &mut BindingState<'a, 'b>,
) -> Vec<BoundNode<'a>> {
    let mut result = vec![];
    if matches!(function_type.system_call_kind, Some(SystemCallKind::Print)) && arguments.len() != 1
    {
        binder
            .diagnostic_bag
            .report_unexpected_argument_count(span, arguments.len(), 1);
    }
    for argument in arguments {
        let argument = bind_node(argument, binder);
        if matches!(argument.type_, Type::Void) {
            binder
                .diagnostic_bag
                .report_invalid_void_expression(argument.span);
        }
        result.push(argument);
    }
    result
}

fn bind_if_statement<'a, 'b>(
    span: TextSpan,
    if_statement: IfStatementNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let condition = bind_node(*if_statement.condition, binder);
    if !matches!(condition.type_, Type::Boolean) {
        binder.diagnostic_bag.report_cannot_convert(
            condition.span,
            &condition.type_,
            &Type::Boolean,
        );
    }
    let body = bind_node(*if_statement.body, binder);
    BoundNode::if_statement(span, condition, body)
}

fn bind_variable_declaration<'a, 'b>(
    span: TextSpan,
    variable_declaration: VariableDeclarationNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let initializer = bind_node(*variable_declaration.initializer, binder);
    if matches!(initializer.type_, Type::Void) {
        binder
            .diagnostic_bag
            .report_invalid_void_expression(initializer.span);
    }
    let variable_index = binder.register_variable(
        variable_declaration.identifier.lexeme,
        initializer.type_.clone(),
    );
    if let Some(variable_index) = variable_index {
        BoundNode::variable_declaration(span, variable_index, initializer)
    } else {
        let span = TextSpan::bounds(
            variable_declaration.let_keyword.span(),
            variable_declaration.identifier.span(),
        );
        binder
            .diagnostic_bag
            .report_cannot_declare_variable(span, variable_declaration.identifier.lexeme);
        BoundNode::error(span)
    }
}

fn bind_while_statement<'a, 'b>(
    span: TextSpan,
    while_statement: WhileStatementNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let condition = bind_node(*while_statement.condition, binder);
    if !matches!(condition.type_, Type::Boolean) {
        binder.diagnostic_bag.report_cannot_convert(
            condition.span,
            &condition.type_,
            &Type::Boolean,
        );
    }
    let body = bind_node(*while_statement.body, binder);
    BoundNode::while_statement(span, condition, body)
}

fn bind_assignment<'a, 'b>(
    span: TextSpan,
    assignment: AssignmentNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let lhs_span = assignment.lhs.span();
    let lhs = match assignment.lhs.kind {
        SyntaxNodeKind::Variable(variable) => {
            bind_variable(lhs_span, variable, binder)
        }
        SyntaxNodeKind::ArrayIndex(array_index) => {
            bind_array_index(lhs_span, array_index, binder)
        }
        error => unreachable!("Unknown left hand side in assignment: {:#?}", error),
    };
    let expression = bind_node(*assignment.expression, binder);
    if !expression.type_.can_be_converted_to(&lhs.type_) {
        binder
            .diagnostic_bag
            .report_cannot_convert(span, &expression.type_, &lhs.type_);
    }
    BoundNode::assignment(span, lhs, expression)
}

fn bind_block_statement<'a, 'b>(
    span: TextSpan,
    block_statement: BlockStatementNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let variable_count = binder.variable_table.len();
    let mut statements = vec![];
    for node in block_statement.statements {
        statements.push(bind_node(node, binder));
    }
    if binder.print_variable_table {
        print_variable_table(&binder.variable_table);
    }
    binder.delete_variables_until(variable_count);
    BoundNode::block_statement(span, statements)
}

fn bind_expression_statement<'a, 'b>(
    span: TextSpan,
    expression_statement: ExpressionStatementNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let expression = bind_node(*expression_statement.expression, binder);
    BoundNode::expression_statement(span, expression)
}
