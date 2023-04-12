use crate::text::SourceText;

use super::*;

#[test]
fn lexer_successfull() {
    lex_helper_successfull("        \n\n\r\r\t\r   \n", |token| {
        assert_eq!(token.len(), 1);
    });

    lex_helper_successfull("", |token| {
        assert_eq!(token.len(), 1);
    });

    // lex_helper_successfull("true", |token| {
    //     assert_eq!(token.len(), 2);
    //     assert_matches!(
    //         token[0],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::TrueKeyword,
    //             lexeme: "true",
    //             start: 0
    //         }
    //     );
    //     assert_matches!(
    //         token[1],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::Eoi,
    //             lexeme: "",
    //             start: 4
    //         }
    //     );
    // });

    // lex_helper_successfull("!=", |token| {
    //     assert_eq!(token.len(), 2);
    //     assert_matches!(
    //         token[0],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::BangEquals,
    //             lexeme: "!=",
    //             start: 0
    //         }
    //     );
    //     assert_matches!(
    //         token[1],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::Eoi,
    //             lexeme: "",
    //             start: 2
    //         }
    //     );
    // });

    // lex_helper_successfull("= =", |token| {
    //     assert_eq!(token.len(), 3);
    //     assert_matches!(
    //         token[0],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::Equals,
    //             lexeme: "=",
    //             start: 0
    //         }
    //     );
    //     assert_matches!(
    //         token[1],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::Equals,
    //             lexeme: "=",
    //             start: 2
    //         }
    //     );
    //     assert_matches!(
    //         token[2],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::Eoi,
    //             lexeme: "",
    //             start: 3
    //         }
    //     );
    // });

    // lex_helper_successfull("false", |token| {
    //     assert_eq!(token.len(), 2);
    //     assert_matches!(
    //         token[0],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::FalseKeyword,
    //             lexeme: "false",
    //             start: 0
    //         }
    //     );
    //     assert_matches!(
    //         token[1],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::Eoi,
    //             lexeme: "",
    //             start: 5
    //         }
    //     );
    // });

    // lex_helper_successfull("1+1", |token| {
    //     assert_eq!(token.len(), 4);
    //     assert_matches!(
    //         token[0],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::NumberLiteral,
    //             lexeme: "1",
    //             start: 0
    //         }
    //     );
    //     assert_matches!(
    //         token[1],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::Plus,
    //             lexeme: "+",
    //             start: 1
    //         }
    //     );
    //     assert_matches!(
    //         token[2],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::NumberLiteral,
    //             lexeme: "1",
    //             start: 2
    //         }
    //     );
    //     assert_matches!(
    //         token[3],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::Eoi,
    //             lexeme: "",
    //             start: 3
    //         }
    //     );
    // });

    // lex_helper_successfull("1 + 1", |token| {
    //     assert_eq!(token.len(), 4);
    //     assert_matches!(
    //         token[0],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::NumberLiteral,
    //             lexeme: "1",
    //             start: 0
    //         }
    //     );
    //     assert_matches!(
    //         token[1],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::Plus,
    //             lexeme: "+",
    //             start: 2
    //         }
    //     );
    //     assert_matches!(
    //         token[2],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::NumberLiteral,
    //             lexeme: "1",
    //             start: 4
    //         }
    //     );
    //     assert_matches!(
    //         token[3],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::Eoi,
    //             lexeme: "",
    //             start: 5
    //         }
    //     );
    // });

    // lex_helper_successfull("-1", |token| {
    //     assert_eq!(token.len(), 3);
    //     assert_matches!(
    //         token[0],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::Minus,
    //             lexeme: "-",
    //             start: 0
    //         }
    //     );
    //     assert_matches!(
    //         token[1],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::NumberLiteral,
    //             lexeme: "1",
    //             start: 1
    //         }
    //     );
    //     assert_matches!(
    //         token[2],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::Eoi,
    //             lexeme: "",
    //             start: 2
    //         }
    //     );
    // });

    // lex_helper_successfull("//This is a comment\n", |token| {
    //     assert_eq!(token.len(), 1);
    //     assert_matches!(
    //         token[0],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::Eoi,
    //             lexeme: "",
    //             start: 20
    //         }
    //     );
    // });
    // lex_helper_successfull("//This is a comment", |token| {
    //     assert_eq!(token.len(), 1);
    //     assert_matches!(
    //         token[0],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::Eoi,
    //             lexeme: "",
    //             start: 19
    //         }
    //     );
    // });
    // lex_helper_successfull("/*This*/ /*is a /*comment*/*/", |token| {
    //     assert_eq!(token.len(), 1);
    //     assert_matches!(
    //         token[0],
    //         SyntaxToken {
    //             kind: SyntaxTokenKind::Eoi,
    //             lexeme: "",
    //             start: 29
    //         }
    //     );
    // });
}

fn lex_helper_successfull(input: &str, callback: impl FnOnce(VecDeque<SyntaxToken>)) {
    let source_text = SourceText::new(input, "");
    let mut source_text_collection = SourceTextCollection::default();
    let source_text = source_text_collection.add(source_text);
    let mut diagnostic_bag = DiagnosticBag::new();
    let result = lex(
        source_text,
        &source_text_collection,
        &mut diagnostic_bag,
        DebugFlags::default(),
    );
    assert!(!diagnostic_bag.has_errors());
    callback(result)
}

#[test]
fn lexer_error() {
    lex_helper_errors("-99999999999999999999", |token, diagnostics| {
        assert_eq!(token.len(), 3);
        assert_eq!(diagnostics.len(), 1);
    });

    lex_helper_errors(" %", |token, diagnostics| {
        assert_eq!(token.len(), 1);
        assert_eq!(diagnostics.len(), 1);
    });

    lex_helper_errors("/* this is a comment", |token, diagnostics| {
        assert_eq!(token.len(), 1);
        assert_eq!(diagnostics.len(), 1);
    });

    lex_helper_errors("1 /* 2", |token, diagnostics| {
        assert_eq!(token.len(), 2);
        assert_eq!(diagnostics.len(), 1);
    });
}

fn lex_helper_errors(
    input: &str,
    callback: impl FnOnce(VecDeque<SyntaxToken>, Vec<crate::diagnostics::Diagnostic>),
) {
    let source_text = SourceText::new(input, "");
    let mut source_text_collection = SourceTextCollection::default();
    let source_text = source_text_collection.add(source_text);
    let mut diagnostic_bag = DiagnosticBag::new();
    let result = lex(
        source_text,
        &source_text_collection,
        &mut diagnostic_bag,
        DebugFlags::default(),
    );
    assert!(diagnostic_bag.has_errors(), "input: {}", input);
    callback(result, diagnostic_bag.diagnostics)
}
