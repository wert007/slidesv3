pub mod bound_nodes;
pub mod operators;
#[cfg(test)]
mod tests;
pub mod typing;

use std::collections::HashMap;

use assert_matches::assert_matches;

use crate::{DebugFlags, binder::{operators::BoundUnaryOperator, typing::Type}, diagnostics::DiagnosticBag, lexer::syntax_token::{SyntaxToken, SyntaxTokenKind}, parser::{self, syntax_nodes::{AssignmentNodeKind, BinaryNodeKind, BlockStatementNodeKind, ExpressionStatementNodeKind, IfStatementNodeKind, LiteralNodeKind, ParenthesizedNodeKind, SyntaxNode, SyntaxNodeKind, UnaryNodeKind, VariableDeclarationNodeKind, VariableNodeKind, WhileStatementNodeKind}}, text::TextSpan};

use self::{bound_nodes::BoundNode, operators::BoundBinaryOperator};

struct BoundVariableName<'a> {
    pub identifier: &'a str,
    pub type_: Type,
}

struct BindingState<'b, 'c> {
    diagnostic_bag: &'b mut DiagnosticBag,
    registered_variables: HashMap<u64, BoundVariableName<'c>>,
    print_variable_table: bool,
}

impl<'b, 'c> BindingState<'b, 'c> {
    fn register_variable(&mut self, name: &'c str, type_: Type) -> Option<u64> {
        let index = self.registered_variables.len() as u64;
        let variable_already_registered = self
            .registered_variables
            .values()
            .any(|variable| variable.identifier == name);
        if variable_already_registered {
            None
        } else {
            self.registered_variables.insert(
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
        if self.registered_variables.is_empty() { return; }
        let current = self.registered_variables.len() - 1;
        for i in index..=current {
            self.registered_variables.remove(&(i as _));
        }
    }

    fn look_up_variable_by_name(&self, name: &str) -> Option<(u64, Type)> {
        self.registered_variables.iter().find(|(_, v)| v.identifier == name).map(|(&i, v)| (i, v.type_.clone()))
    }
}

fn print_variable_table(variable_table: &HashMap<u64, BoundVariableName>) {
    let mut variable_table : Vec<_> = variable_table.iter().collect();
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
    input: &'a str,
    diagnostic_bag: &mut DiagnosticBag,
    debug_flags: DebugFlags,
) -> BoundNode<'a> {
    let node = parser::parse(input, diagnostic_bag);
    if diagnostic_bag.has_errors() {
        return BoundNode::error(TextSpan::new(0, 0));
    }
    let mut binder = BindingState {
        diagnostic_bag,
        registered_variables: HashMap::new(),
        print_variable_table: debug_flags.print_variable_table(),
    };
    bind_node(node, &mut binder)
}

fn bind_node<'a, 'b>(node: SyntaxNode<'a>, binder: &mut BindingState<'b, 'a>) -> BoundNode<'a> {
    match node.kind {
        SyntaxNodeKind::Literal(literal) => bind_literal(node.span, literal, binder),
        SyntaxNodeKind::Variable(variable) => bind_variable(node.span, variable, binder),
        SyntaxNodeKind::Binary(binary) => bind_binary(node.span, binary, binder),
        SyntaxNodeKind::Unary(unary) => bind_unary(node.span, unary, binder),
        SyntaxNodeKind::Parenthesized(parenthesized) => {
            bind_parenthesized(node.span, parenthesized, binder)
        }
        SyntaxNodeKind::BlockStatement(block_statement) => {
            bind_block_statement(node.span, block_statement, binder)
        }
        SyntaxNodeKind::IfStatement(if_statement) => bind_if_statement(node.span, if_statement, binder),
        SyntaxNodeKind::VariableDeclaration(variable_declaration) => {
            bind_variable_declaration(node.span, variable_declaration, binder)
        }
        SyntaxNodeKind::WhileStatement(while_statement) => bind_while_statement(node.span, while_statement, binder),
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

fn bind_variable<'a, 'b>(
    span: TextSpan,
    variable: VariableNodeKind<'a>,
    binder: &mut BindingState<'b, 'a>,
) -> BoundNode<'a> {
    match binder.look_up_variable_by_name(variable.token.lexeme) {
        Some((index, type_)) => BoundNode::variable(span, index, type_),
        None => {
            binder.diagnostic_bag.report_variable_not_found(span, variable.token.lexeme);
            BoundNode::error(span)
        }
    }
}

fn bind_binary<'a, 'b>(
    span: TextSpan,
    binary: BinaryNodeKind<'a>,
    binder: &mut BindingState<'b, 'a>,
) -> BoundNode<'a> {
    let lhs = bind_node(*binary.lhs, binder);
    let rhs = bind_node(*binary.rhs, binder);
    match bind_binary_operator(span, &lhs, binary.operator_token, &rhs, binder) {
        Some((operator_token, type_)) => BoundNode::binary(span, lhs, operator_token, rhs, type_),
        None => BoundNode::error(span),
    }
}

fn bind_binary_operator(
    span: TextSpan,
    lhs: &BoundNode,
    operator_token: SyntaxToken,
    rhs: &BoundNode,
    binder: &mut BindingState,
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
        (lhs, BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals, rhs) if lhs == rhs => {
            Some((result, Type::Boolean))
        }
        (Type::Integer, BoundBinaryOperator::ArithmeticAddition 
                    | BoundBinaryOperator::ArithmeticSubtraction 
                    | BoundBinaryOperator::ArithmeticMultiplication 
                    | BoundBinaryOperator::ArithmeticDivision, Type::Integer) => Some((result, Type::Integer)),
        (Type::Integer, BoundBinaryOperator::LessThan
                    | BoundBinaryOperator::GreaterThan
                    | BoundBinaryOperator::LessThanEquals
                    | BoundBinaryOperator::GreaterThanEquals, Type::Integer) => Some((result, Type::Boolean)),
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
    binder: &mut BindingState<'b, 'a>,
) -> BoundNode<'a> {
    let operand = bind_node(*unary.operand, binder);
    match bind_unary_operator(span, &operand, unary.operator_token, binder) {
        Some((operator_token, type_)) => BoundNode::unary(span, operator_token, operand, type_),
        None => BoundNode::error(span),
    }
}

fn bind_unary_operator(
    span: TextSpan,
    operand: &BoundNode,
    operator_token: SyntaxToken,
    binder: &mut BindingState,
) -> Option<(BoundUnaryOperator, Type)> {
    let result = match &operator_token.kind {
        SyntaxTokenKind::Plus => BoundUnaryOperator::ArithmeticIdentity,
        SyntaxTokenKind::Minus => BoundUnaryOperator::ArithmeticNegate,
        SyntaxTokenKind::Bang => todo!(),
        _ => unreachable!(),
    };
    match operand.type_ {
        Type::Void | Type::Error => None,
        Type::Integer => Some((result, Type::Integer)),
        Type::Boolean => {
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
    binder: &mut BindingState<'b, 'a>,
) -> BoundNode<'a> {
    bind_node(*parenthesized.expression, binder)
}

fn bind_if_statement<'a, 'b>(
    span: TextSpan,
    if_statement: IfStatementNodeKind<'a>,
    binder: &mut BindingState<'b, 'a>,
) -> BoundNode<'a> {
    let condition = bind_node(*if_statement.condition, binder);
    if !matches!(condition.type_, Type::Boolean) {
        binder.diagnostic_bag.report_cannot_convert(condition.span, &condition.type_, &Type::Boolean);
    }
    let body = bind_node(*if_statement.body, binder);
    BoundNode::if_statement(span, condition, body)
}

fn bind_variable_declaration<'a, 'b>(
    span: TextSpan,
    variable_declaration: VariableDeclarationNodeKind<'a>,
    binder: &mut BindingState<'b, 'a>,
) -> BoundNode<'a> {
    let initializer = bind_node(*variable_declaration.initializer, binder);
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
    binder: &mut BindingState<'b, 'a>,
) -> BoundNode<'a> {
    let condition = bind_node(*while_statement.condition, binder);
    if !matches!(condition.type_, Type::Boolean) {
        binder.diagnostic_bag.report_cannot_convert(condition.span, &condition.type_, &Type::Boolean);
    }
    let body = bind_node(*while_statement.body, binder);
    BoundNode::while_statement(span, condition, body)
}

fn bind_assignment<'a, 'b>(
    span: TextSpan,
    assignment: AssignmentNodeKind<'a>,
    binder: &mut BindingState<'b, 'a>,
) -> BoundNode<'a> {
    let variable_span = assignment.lhs.span();
    let variable = assert_matches!(assignment.lhs.kind, SyntaxNodeKind::Variable(variable) => variable);
    let variable = bind_variable(variable_span, variable, binder);
    let expression = bind_node(*assignment.expression, binder);
    if !expression.type_.can_be_converted_to(&variable.type_) {
        binder.diagnostic_bag.report_cannot_convert(span, &expression.type_, &variable.type_);
    }
    BoundNode::assignment(span, variable, expression)
}

fn bind_block_statement<'a, 'b>(
    span: TextSpan,
    block_statement: BlockStatementNodeKind<'a>,
    binder: &mut BindingState<'b, 'a>,
) -> BoundNode<'a> {
    let variable_count = binder.registered_variables.len();
    let mut statements = vec![];
    for node in block_statement.statements {
        statements.push(bind_node(node, binder));
    }
    if binder.print_variable_table {
        print_variable_table(&binder.registered_variables);
    }
    binder.delete_variables_until(variable_count);
    BoundNode::block_statement(span, statements)
}

fn bind_expression_statement<'a, 'b>(
    span: TextSpan,
    expression_statement: ExpressionStatementNodeKind<'a>,
    binder: &mut BindingState<'b, 'a>,
) -> BoundNode<'a> {
    let expression = bind_node(*expression_statement.expression, binder);
    BoundNode::expression_statement(span, expression)
}