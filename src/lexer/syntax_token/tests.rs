use crate::text::SourceText;

use super::*;
use assert_matches::assert_matches;

#[test]
fn number_literal() {
    let mut diagnostic_bag = DiagnosticBag::new(SourceText::new("4312", ""));
    let number_literal = SyntaxToken::number_literal(0, "4312", &mut diagnostic_bag);
    assert!(!diagnostic_bag.has_errors());
    if let SyntaxTokenKind::NumberLiteral(NumberLiteralKind { value }) = number_literal.kind {
        assert_eq!(value, 4312);
    } else {
        panic!("number_literal.kind is wrong: {:?}", number_literal.kind);
    }

    let mut diagnostic_bag = DiagnosticBag::new(SourceText::new("431243124312431243124312431243124312", ""));
    let number_literal = SyntaxToken::number_literal(
        0,
        "431243124312431243124312431243124312",
        &mut diagnostic_bag,
    );
    assert!(diagnostic_bag.has_errors());
    if let SyntaxTokenKind::NumberLiteral(NumberLiteralKind { value }) = number_literal.kind {
        assert_eq!(value, 0);
    } else {
        panic!("number_literal.kind is wrong: {:?}", number_literal.kind);
    }
}

#[test]
fn syntax_token_kind() {
    let op = SyntaxTokenKind::from("+");
    assert_matches!(op, SyntaxTokenKind::Plus);
    let op = SyntaxTokenKind::from("-");
    assert_matches!(op, SyntaxTokenKind::Minus);
    let op = SyntaxTokenKind::from("*");
    assert_matches!(op, SyntaxTokenKind::Star);
    let op = SyntaxTokenKind::from("/");
    assert_matches!(op, SyntaxTokenKind::Slash);
    let op = SyntaxTokenKind::from("!");
    assert_matches!(op, SyntaxTokenKind::Bang);
}
