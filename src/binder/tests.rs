use assert_matches::assert_matches;

use crate::{binder::bound_nodes::BoundNodeKind, diagnostics::Diagnostic, text::SourceText, value::Value};

use super::*;

#[test]
fn successful_binding() {
    let node = bind_helper_expression("1 + 1;");
    assert_matches!(node.kind, BoundNodeKind::BinaryExpression(binary) => {
        assert_matches!(binary.lhs.kind, BoundNodeKind::LiteralExpression(LiteralNodeKind { value: Value::Integer(1), .. }));
        assert_matches!(binary.lhs.type_, Type::Integer);
        assert_matches!(binary.operator_token, BoundBinaryOperator::ArithmeticAddition);
        assert_matches!(binary.rhs.kind, BoundNodeKind::LiteralExpression(LiteralNodeKind { value: Value::Integer(1), .. }));
        assert_matches!(binary.rhs.type_, Type::Integer);
    });
    assert_matches!(node.type_, Type::Integer);

    let node = bind_helper_expression("true != false;");
    assert_matches!(node.kind, BoundNodeKind::BinaryExpression(binary) => {
        assert_matches!(binary.lhs.kind, BoundNodeKind::LiteralExpression(LiteralNodeKind { value: Value::Boolean(true), ..}));
        assert_matches!(binary.lhs.type_, Type::Boolean);
        assert_matches!(binary.operator_token, BoundBinaryOperator::NotEquals);
        assert_matches!(binary.rhs.kind, BoundNodeKind::LiteralExpression(LiteralNodeKind { value: Value::Boolean(false), .. }));
        assert_matches!(binary.rhs.type_, Type::Boolean);
    });
    assert_matches!(node.type_, Type::Boolean);

    let node = bind_helper_expression("(1 == 2) == (3 != 4);");
    assert_matches!(node.kind, BoundNodeKind::BinaryExpression(binary) => {
        assert_matches!(binary.lhs.kind, BoundNodeKind::BinaryExpression(binary) => {
            assert_matches!(binary.lhs.kind, BoundNodeKind::LiteralExpression(LiteralNodeKind { value: Value::Integer(1), .. }));
            assert_matches!(binary.lhs.type_, Type::Integer);
            assert_matches!(binary.operator_token, BoundBinaryOperator::Equals);
            assert_matches!(binary.rhs.kind, BoundNodeKind::LiteralExpression(LiteralNodeKind { value: Value::Integer(2), .. }));
            assert_matches!(binary.rhs.type_, Type::Integer);
        });
        assert_matches!(binary.lhs.type_, Type::Boolean);
        assert_matches!(binary.operator_token, BoundBinaryOperator::Equals);
        assert_matches!(binary.rhs.kind, BoundNodeKind::BinaryExpression(binary) => {
            assert_matches!(binary.lhs.kind, BoundNodeKind::LiteralExpression(LiteralNodeKind { value: Value::Integer(3), .. }));
            assert_matches!(binary.lhs.type_, Type::Integer);
            assert_matches!(binary.operator_token, BoundBinaryOperator::NotEquals);
            assert_matches!(binary.rhs.kind, BoundNodeKind::LiteralExpression(LiteralNodeKind { value: Value::Integer(4), .. }));
            assert_matches!(binary.rhs.type_, Type::Integer);
        });
        assert_matches!(binary.rhs.type_, Type::Boolean);
    });
    assert_matches!(node.type_, Type::Boolean);

    let node = bind_helper("let a = 99;");
    assert_matches!(node.kind, BoundNodeKind::VariableDeclaration(variable_declaration) => {
        assert_matches!(variable_declaration.variable_index, 0);
        assert_matches!(variable_declaration.initializer.kind, BoundNodeKind::LiteralExpression(LiteralNodeKind { value: Value::Integer(99), .. }));
        assert_matches!(variable_declaration.initializer.type_, Type::Integer);
    });
    assert_matches!(node.type_, Type::Void);

    let node = bind_helper("if 9 == 12 { let a = 14; }");
    assert_matches!(node.kind, BoundNodeKind::IfStatement(if_statement) => {
        assert_matches!(if_statement.condition.kind, BoundNodeKind::BinaryExpression(binary) => {
            assert_matches!(binary.lhs.kind, BoundNodeKind::LiteralExpression(LiteralNodeKind { value: Value::Integer(9), .. }));
            assert_matches!(binary.lhs.type_, Type::Integer);
            assert_matches!(binary.operator_token, BoundBinaryOperator::Equals);
            assert_matches!(binary.rhs.kind, BoundNodeKind::LiteralExpression(LiteralNodeKind { value: Value::Integer(12), .. }));
            assert_matches!(binary.rhs.type_, Type::Integer);
        });
        assert_matches!(if_statement.condition.type_, Type::Boolean);
        assert_matches!(if_statement.body.kind, BoundNodeKind::BlockStatement(block_statement) => {
            assert_eq!(block_statement.statements.len(), 1);
            assert_matches!(&block_statement.statements[0].kind, BoundNodeKind::VariableDeclaration(variable_declaration) => {
                assert_matches!(variable_declaration.variable_index, 0);
                assert_matches!(variable_declaration.initializer.kind, BoundNodeKind::LiteralExpression(LiteralNodeKind { value: Value::Integer(14), .. }));
                assert_matches!(variable_declaration.initializer.type_, Type::Integer);
            });
            assert_matches!(block_statement.statements[0].type_, Type::Void);
        });
        assert_matches!(if_statement.body.type_, Type::Void);
    });
    assert_matches!(node.type_, Type::Void);
}

fn bind_helper(input: &str) -> BoundNode {
    let mut diagnostic_bag = DiagnosticBag::new(SourceText::new(input, ""));
    let result = bind(input, &mut diagnostic_bag, DebugFlags::default());
    assert!(!diagnostic_bag.has_errors());
    result
}

fn bind_helper_expression(input: &str) -> BoundNode {
    let result = bind_helper(input);
    assert_matches!(result.kind, BoundNodeKind::ExpressionStatement(expression_statement) => *expression_statement.expression)
}

#[test]
fn failed_binding() {
    let (node, diagnostics) = bind_helper_errors_expression("1 == 2 == 3;");
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].to_string(), "Error at 0-11: No binary operator == for types bool and int.");
    assert_matches!(node.kind, BoundNodeKind::ErrorExpression);

    let (node, diagnostics) = bind_helper_errors_expression("(1 == 2 == 3) + 4;");
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].to_string(), "Error at 1-12: No binary operator == for types bool and int.");
    assert_matches!(node.kind, BoundNodeKind::ErrorExpression);

    let (node, diagnostics) = bind_helper_errors("let a = a + 3;");
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].to_string(), "Error at 8-9: No variable named 'a' could be found.");
    assert_matches!(node.kind, BoundNodeKind::VariableDeclaration(variable_declaration) => {
        assert_eq!(variable_declaration.variable_index, 0);
        assert_matches!(variable_declaration.initializer.kind, BoundNodeKind::ErrorExpression);
    });
}

fn bind_helper_errors(input: &str) -> (BoundNode, Vec<Diagnostic>) {
    let mut diagnostic_bag = DiagnosticBag::new(SourceText::new(input, ""));
    let node = bind(input, &mut diagnostic_bag, DebugFlags::default());
    assert!(diagnostic_bag.has_errors());
    (node, diagnostic_bag.diagnostics)
}

fn bind_helper_errors_expression(input: &str) -> (BoundNode, Vec<Diagnostic>) {
    let mut diagnostic_bag = DiagnosticBag::new(SourceText::new(input, ""));
    let node = bind(input, &mut diagnostic_bag, DebugFlags::default());
    let node = assert_matches!(node.kind, BoundNodeKind::ExpressionStatement(expression_statement) => *expression_statement.expression);
    assert!(diagnostic_bag.has_errors());
    (node, diagnostic_bag.diagnostics)
}