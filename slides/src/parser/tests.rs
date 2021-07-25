use super::{syntax_nodes::SyntaxNodeKind, *};
use crate::const_number_literal_syntax_token;
use crate::diagnostics::Diagnostic;
use crate::lexer::syntax_token::NumberLiteralKind;
use crate::parser::syntax_nodes::LiteralNodeKind;
use crate::parser::syntax_nodes::VariableNodeKind;
use crate::text::SourceText;
use crate::value::Value;
use assert_matches::assert_matches;

#[test]
fn parser_precedence() {
    parse_helper_expression("1 + 1;", |node| {
        assert_matches!(node.kind, SyntaxNodeKind::Binary(binary) => {
            assert_matches!(binary.lhs.kind, SyntaxNodeKind::Literal(LiteralNodeKind {token: const_number_literal_syntax_token!(0, 1), value: Value::Integer(1)}));
            assert_matches!(binary.operator_token, SyntaxToken { kind: SyntaxTokenKind::Plus, lexeme: "+", start: 2 });
            assert_matches!(binary.rhs.kind, SyntaxNodeKind::Literal(LiteralNodeKind {token: const_number_literal_syntax_token!(4, 1), value: Value::Integer(1)}));
        });
    });

    parse_helper_expression("true == false;", |node| {
        assert_matches!(node.kind, SyntaxNodeKind::Binary(binary) => {
            assert_matches!(binary.lhs.kind, SyntaxNodeKind::Literal(LiteralNodeKind {
                token: SyntaxToken {
                    kind: SyntaxTokenKind::TrueKeyword,
                    lexeme: "true",
                    start: 0 },
                value: Value::Boolean(true) }));
            assert_matches!(binary.operator_token, SyntaxToken { kind: SyntaxTokenKind::EqualsEquals, lexeme: "==", start: 5 });
            assert_matches!(binary.rhs.kind, SyntaxNodeKind::Literal(LiteralNodeKind {
                token: SyntaxToken {
                    kind: SyntaxTokenKind::FalseKeyword,
                    lexeme: "false",
                    start: 8 },
                value: Value::Boolean(false) }));
        });
    });

    parse_helper_expression("1 + 2 * 3;", |node| {
        assert_matches!(node.kind, SyntaxNodeKind::Binary(binary) => {
            assert_matches!(binary.lhs.kind, SyntaxNodeKind::Literal(LiteralNodeKind {token: const_number_literal_syntax_token!(0, 1), value: Value::Integer(1)}));
            assert_matches!(binary.operator_token, SyntaxToken { kind: SyntaxTokenKind::Plus, lexeme: "+", start: 2 });
            assert_matches!(&binary.rhs.kind, SyntaxNodeKind::Binary(binary) => {
                assert_matches!(binary.lhs.kind, SyntaxNodeKind::Literal(LiteralNodeKind {token: const_number_literal_syntax_token!(4, 2), value: Value::Integer(2)}));
                assert_matches!(binary.operator_token, SyntaxToken { kind: SyntaxTokenKind::Star, lexeme: "*", start: 6 });
                assert_matches!(binary.rhs.kind, SyntaxNodeKind::Literal(LiteralNodeKind {token: const_number_literal_syntax_token!(8, 3), value: Value::Integer(3)}));
            });
        });
    });

    parse_helper_expression("3 * 2 + 1;", |node| {
        assert_matches!(node.kind, SyntaxNodeKind::Binary(binary) => {
            assert_matches!(&binary.lhs.kind, SyntaxNodeKind::Binary(binary) => {
                assert_matches!(binary.lhs.kind, SyntaxNodeKind::Literal(LiteralNodeKind {token: const_number_literal_syntax_token!(0, 3), value: Value::Integer(3)}));
                assert_matches!(binary.operator_token, SyntaxToken { kind: SyntaxTokenKind::Star, lexeme: "*", start: 2 });
                assert_matches!(binary.rhs.kind, SyntaxNodeKind::Literal(LiteralNodeKind {token: const_number_literal_syntax_token!(4, 2), value: Value::Integer(2)}));
            });
            assert_matches!(binary.operator_token, SyntaxToken { kind: SyntaxTokenKind::Plus, lexeme: "+", start: 6 });
            assert_matches!(binary.rhs.kind, SyntaxNodeKind::Literal(LiteralNodeKind {token: const_number_literal_syntax_token!(8, 1), value: Value::Integer(1)}));
        });
    });

    parse_helper("let a = 99;", |node| {
        assert_matches!(node.kind, SyntaxNodeKind::VariableDeclaration(variable) => {
            assert_matches!(variable.identifier.lexeme, "a");
            assert_matches!(variable.initializer.kind, SyntaxNodeKind::Literal(LiteralNodeKind { token: const_number_literal_syntax_token!(8, 99), value: Value::Integer(99) }));
        });
    });

    parse_helper("let a = b + c;", |node| {
        assert_matches!(node.kind, SyntaxNodeKind::VariableDeclaration(variable) => {
            assert_matches!(variable.identifier.lexeme, "a");
            assert_matches!(variable.initializer.kind, SyntaxNodeKind::Binary(binary) => {
                assert_matches!(binary.lhs.kind, SyntaxNodeKind::Variable(VariableNodeKind {
                    token: SyntaxToken { kind: SyntaxTokenKind::Identifier, lexeme: "b", start: 8 }
                }));

                assert_matches!(binary.rhs.kind, SyntaxNodeKind::Variable(VariableNodeKind {
                    token: SyntaxToken { kind: SyntaxTokenKind::Identifier, lexeme: "c", start: 12 }
                }));
            });
        });
    });

    parse_helper("a = b + c;", |node| {
        assert_matches!(node.kind, SyntaxNodeKind::Assignment(assignment) => {
            assert_matches!(assignment.lhs.kind, SyntaxNodeKind::Variable(VariableNodeKind { token: SyntaxToken { kind: SyntaxTokenKind::Identifier, lexeme: "a", start: 0 } }));
            assert_matches!(assignment.expression.kind, SyntaxNodeKind::Binary(binary) => {
                assert_matches!(binary.lhs.kind, SyntaxNodeKind::Variable(VariableNodeKind {
                    token: SyntaxToken { kind: SyntaxTokenKind::Identifier, lexeme: "b", start: 4 }
                }));

                assert_matches!(binary.rhs.kind, SyntaxNodeKind::Variable(VariableNodeKind {
                    token: SyntaxToken { kind: SyntaxTokenKind::Identifier, lexeme: "c", start: 8 }
                }));
            });
        });
    });
}

fn parse_helper_expression(input: &str, callback: impl FnOnce(SyntaxNode) -> ()) {
    let source_text = SourceText::new(input, "");
    let mut diagnostic_bag = DiagnosticBag::new(&source_text);
    let result = parse(&source_text, &mut diagnostic_bag);
    assert!(!diagnostic_bag.has_errors());
    let result = assert_matches!(result.kind, SyntaxNodeKind::ExpressionStatement(expression_statement) => *expression_statement.expression);
    callback(result)
}

fn parse_helper(input: &str, callback: impl FnOnce(SyntaxNode) -> ()) {
    let source_text = SourceText::new(input, "");
    let mut diagnostic_bag = DiagnosticBag::new(&source_text);
    let result = parse(&source_text, &mut diagnostic_bag);
    assert!(!diagnostic_bag.has_errors());
    callback(result)
}

#[test]
fn parser_error() {
    parse_helper_error("", |node, diagnostics| {
        assert_matches!(
        node.kind,
        SyntaxNodeKind::ExpressionStatement(expression_statement) => {
            assert_matches!(
                expression_statement.expression.kind,
                SyntaxNodeKind::Literal(LiteralNodeKind {
                    token: const_number_literal_syntax_token!(0, 0),
                    value: Value::Integer(0) }));
            assert_matches!(expression_statement.semicolon_token.kind, SyntaxTokenKind::Semicolon);
        });
        assert_eq!(diagnostics.len(), 2);
        assert_eq!(diagnostics[0].to_string(), "Error at 0-0: Expected token kind NumberLiteralKind(0) but actually found End-of-Input-Token.");
        assert_eq!(
            diagnostics[1].to_string(),
            "Error at 0-0: Expected token kind SemicolonToken but actually found End-of-Input-Token."
        );
    });

    parse_helper_error_expression("1 +;", |node, diagnostics| {
        assert_matches!(node.kind, SyntaxNodeKind::Binary(binary) => {
            assert_matches!(binary.lhs.kind, SyntaxNodeKind::Literal(LiteralNodeKind {token: const_number_literal_syntax_token!(0, 1), value: Value::Integer(1)}));
            assert_matches!(binary.operator_token, SyntaxToken { kind: SyntaxTokenKind::Plus, lexeme: "+", start: 2 });
            assert_matches!(binary.rhs.kind, SyntaxNodeKind::Literal(LiteralNodeKind {token: const_number_literal_syntax_token!(3, 0), value: Value::Integer(0)}));
        });
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(
            diagnostics[0].to_string(),
            "Error at 3-4: Expected token kind NumberLiteralKind(0) but actually found SemicolonToken."
        );
    });

    parse_helper_error_expression("*42;", |node, diagnostics| {
        assert_matches!(node.kind, SyntaxNodeKind::Binary(binary) => {
            assert_matches!(binary.lhs.kind, SyntaxNodeKind::Literal(LiteralNodeKind { token: const_number_literal_syntax_token!(0, 0), value: Value::Integer(0) }));
            assert_matches!(binary.operator_token, SyntaxToken { kind: SyntaxTokenKind::Star, lexeme: "*", start: 0 });
            assert_matches!(binary.lhs.kind, SyntaxNodeKind::Literal(LiteralNodeKind { token: const_number_literal_syntax_token!(0, 0), value: Value::Integer(0) }));
        });
        // assert_matches!(
        //     node.kind,
        //     SyntaxNodeKind::Literal(LiteralNodeKind {
        //         token: const_number_literal_syntax_token!(0, 0),
        //         value: Value::Integer(0)
        //     })
        // );
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(
            diagnostics[0].to_string(),
            "Error at 0-1: Expected token kind NumberLiteralKind(0) but actually found StarToken."
        );
        // assert_eq!(
        //     diagnostics[1].to_string(),
        //     "Error at 1-3: Expected token kind End-of-Input-Token but actually found NumberLiteralKind(42)."
        // );
    });

    parse_helper_error_expression("1/*2;", |node, diagnostics| {
        // assert_matches!(node.kind, SyntaxNodeKind::Binary(binary) => {
        // });
        assert_matches!(node.kind, SyntaxNodeKind::Binary(binary) => {
                assert_matches!(binary.lhs.kind, SyntaxNodeKind::Binary(binary) => {
                    assert_matches!(binary.lhs.kind, SyntaxNodeKind::Literal(LiteralNodeKind {token: const_number_literal_syntax_token!(0, 1), value: Value::Integer(1)}));
                    assert_matches!(binary.operator_token, SyntaxToken { kind: SyntaxTokenKind::Slash, lexeme: "/", start: 1 });
                    assert_matches!(binary.rhs.kind, SyntaxNodeKind::Literal(LiteralNodeKind {token: const_number_literal_syntax_token!(2, 0), value: Value::Integer(0)}));
            });
            assert_matches!(binary.operator_token, SyntaxToken { kind: SyntaxTokenKind::Star, lexeme: "*", start: 2 });
            assert_matches!(binary.rhs.kind, SyntaxNodeKind::Literal(LiteralNodeKind {token: const_number_literal_syntax_token!(3, 2), value: Value::Integer(2)}));
        });
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(
            diagnostics[0].to_string(),
            "Error at 2-3: Expected token kind NumberLiteralKind(0) but actually found StarToken."
        );
    });

    parse_helper_error("let true = false;", |node, diagnostics| {
        assert_matches!(node.kind, SyntaxNodeKind::VariableDeclaration(variable_declaration) => {
            assert_matches!(variable_declaration.identifier.lexeme, "");
        });
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(
            diagnostics[0].to_string(),
            "Error at 4-8: 'true' is a keyword and cannot be overwritten."
        );
    });

    parse_helper_error("if a = 4 { a = 0; }", |node, diagnostics| {
        assert_matches!(node.kind, SyntaxNodeKind::IfStatement(if_statement) => {
            assert_matches!(if_statement.condition.kind, SyntaxNodeKind::Binary(binary) => {
                assert_matches!(binary.lhs.kind, SyntaxNodeKind::Variable(VariableNodeKind { token: SyntaxToken { kind: SyntaxTokenKind::Identifier, lexeme: "a", start:3 } }));
                assert_matches!(binary.operator_token, SyntaxToken { kind: SyntaxTokenKind::Equals, lexeme: "=", start:5 });
                assert_matches!(binary.rhs.kind, SyntaxNodeKind::Literal(LiteralNodeKind { token: SyntaxToken { kind: SyntaxTokenKind::NumberLiteral(NumberLiteralKind { value: 4 }), lexeme: "4", start:7 }, value: Value::Integer(4) }));
            });
            assert_matches!(if_statement.body.kind, SyntaxNodeKind::BlockStatement(block_statement) => {
                assert_eq!(block_statement.statements.len(), 1);
                assert_matches!(&block_statement.statements[0].kind, SyntaxNodeKind::Assignment(assignment) => {
                    assert_matches!(assignment.lhs.kind, SyntaxNodeKind::Variable(VariableNodeKind { token: SyntaxToken { kind: SyntaxTokenKind::Identifier, lexeme: "a", start: 11 } }));
                    assert_matches!(assignment.expression.kind, SyntaxNodeKind::Literal(LiteralNodeKind { token: SyntaxToken { kind: SyntaxTokenKind::NumberLiteral(NumberLiteralKind { value: 0 }), lexeme: "0", start: 15 }, value: Value::Integer(0) }));
                })
            })
        });
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(
            diagnostics[0].to_string(),
            "Error at 3-4: Only variables can be assigned to. Did you mean to use ==?"
        );
    });
}

fn parse_helper_error(input: &str, callback: impl FnOnce(SyntaxNode, Vec<Diagnostic>) -> ()) {
    let source_text = SourceText::new(input, "");
    let mut diagnostic_bag = DiagnosticBag::new(&source_text);
    let result = parse(&source_text, &mut diagnostic_bag);
    assert!(diagnostic_bag.has_errors());
    callback(result, diagnostic_bag.diagnostics)
}

fn parse_helper_error_expression(
    input: &str,
    callback: impl FnOnce(SyntaxNode, Vec<Diagnostic>) -> (),
) {
    let source_text = SourceText::new(input, "");
    let mut diagnostic_bag = DiagnosticBag::new(&source_text);
    let result = parse(&source_text, &mut diagnostic_bag);
    assert!(diagnostic_bag.has_errors());
    let result = assert_matches!(result.kind, SyntaxNodeKind::ExpressionStatement(expression_statement) => *expression_statement.expression);
    callback(result, diagnostic_bag.diagnostics)
}
