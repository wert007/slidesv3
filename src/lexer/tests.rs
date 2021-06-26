use assert_matches::assert_matches;

use crate::{lexer::syntax_token::{NumberLiteralKind, SyntaxTokenKind}, text::SourceText};

use super::*;

#[test]
fn lexer_successfull() {
    let token = lex_helper_successfull("        \n\n\r\r\t\r   \n");
    assert_eq!(token.len(), 1);
    
    let token = lex_helper_successfull("");
    assert_eq!(token.len(), 1);

    let token = lex_helper_successfull("true");
    assert_eq!(token.len(), 2);
    assert_matches!(token[0], SyntaxToken { kind: SyntaxTokenKind::TrueKeyword, lexeme: "true", start: 0 });
    assert_matches!(token[1], SyntaxToken { kind: SyntaxTokenKind::Eoi, lexeme: "", start: 4 });

    let token = lex_helper_successfull("!=");
    assert_eq!(token.len(), 2);
    assert_matches!(token[0], SyntaxToken { kind: SyntaxTokenKind::BangEquals, lexeme: "!=", start: 0 });
    assert_matches!(token[1], SyntaxToken { kind: SyntaxTokenKind::Eoi, lexeme: "", start: 2 });

    let token = lex_helper_successfull("= =");
    assert_eq!(token.len(), 3);
    assert_matches!(token[0], SyntaxToken { kind: SyntaxTokenKind::Equals, lexeme: "=", start: 0 });
    assert_matches!(token[1], SyntaxToken { kind: SyntaxTokenKind::Equals, lexeme: "=", start: 2 });
    assert_matches!(token[2], SyntaxToken { kind: SyntaxTokenKind::Eoi, lexeme: "", start: 3 });


    let token = lex_helper_successfull("false");
    assert_eq!(token.len(), 2);
    assert_matches!(token[0], SyntaxToken { kind: SyntaxTokenKind::FalseKeyword, lexeme: "false", start: 0 });
    assert_matches!(token[1], SyntaxToken { kind: SyntaxTokenKind::Eoi, lexeme: "", start: 5 });

    let token = lex_helper_successfull("1+1");
    assert_eq!(token.len(), 4);
    assert_matches!(token[0], SyntaxToken { kind: SyntaxTokenKind::NumberLiteral(NumberLiteralKind { value: 1 }), lexeme: "1", start: 0 });
    assert_matches!(token[1], SyntaxToken { kind: SyntaxTokenKind::Plus, lexeme: "+", start: 1 });
    assert_matches!(token[2], SyntaxToken { kind: SyntaxTokenKind::NumberLiteral(NumberLiteralKind { value: 1 }), lexeme: "1", start: 2 });
    assert_matches!(token[3], SyntaxToken { kind: SyntaxTokenKind::Eoi, lexeme: "", start: 3 });

    let token = lex_helper_successfull("1 + 1");
    assert_eq!(token.len(), 4);
    assert_matches!(token[0], SyntaxToken { kind: SyntaxTokenKind::NumberLiteral(NumberLiteralKind { value: 1 }), lexeme: "1", start: 0 });
    assert_matches!(token[1], SyntaxToken { kind: SyntaxTokenKind::Plus, lexeme: "+", start: 2 });
    assert_matches!(token[2], SyntaxToken { kind: SyntaxTokenKind::NumberLiteral(NumberLiteralKind { value: 1 }), lexeme: "1", start: 4 });
    assert_matches!(token[3], SyntaxToken { kind: SyntaxTokenKind::Eoi, lexeme: "", start: 5 });
    
    let token = lex_helper_successfull("-1");
    assert_eq!(token.len(), 3);
    assert_matches!(token[0], SyntaxToken { kind: SyntaxTokenKind::Minus, lexeme: "-", start: 0 });
    assert_matches!(token[1], SyntaxToken { kind: SyntaxTokenKind::NumberLiteral(NumberLiteralKind { value: 1 }), lexeme: "1", start: 1 });
    assert_matches!(token[2], SyntaxToken { kind: SyntaxTokenKind::Eoi, lexeme: "", start: 2 });
}

fn lex_helper_successfull(input: &str) -> VecDeque<SyntaxToken> {
    let mut diagnostic_bag = DiagnosticBag::new(SourceText::new(input, ""));
    let result = lex(input, &mut diagnostic_bag);
    assert!(!diagnostic_bag.has_errors());
    result
}

#[test]
fn lexer_error() {
    let diagnostics = lex_helper_error("-99999999999999999999", 3);
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(
        format!("{}", diagnostics[0]),
        "Error at 1-21: '99999999999999999999' is no valid u64 integer."
    );

    let diagnostics = lex_helper_error(" %", 1);
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(
        format!("{}", diagnostics[0]),
        "Error at 1-2: Bad character in input: %"
    );
}

fn lex_helper_error(
    input: &str,
    expected_success_length: usize,
) -> Vec<crate::diagnostics::Diagnostic> {
    let mut diagnostic_bag = DiagnosticBag::new(SourceText::new(input, ""));
    let result = lex(input, &mut diagnostic_bag);
    assert!(diagnostic_bag.has_errors(), "input: {}", input);
    assert_eq!(result.len(), expected_success_length);
    diagnostic_bag.diagnostics
}
