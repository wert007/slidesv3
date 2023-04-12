use super::*;
use assert_matches::assert_matches;

#[test]
fn number_literal() {
    // let number_literal = SyntaxToken::number_literal(0, "4312");
    // assert!(matches!(
    //     number_literal.kind,
    //     SyntaxTokenKind::NumberLiteral
    // ));

    // let number_literal = SyntaxToken::number_literal(0, "431243124312431243124312431243124312");
    // assert!(matches!(
    //     number_literal.kind,
    //     SyntaxTokenKind::NumberLiteral
    // ));
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
