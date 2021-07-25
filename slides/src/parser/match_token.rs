#[macro_export]
macro_rules! match_token {
    ($tokens:expr, $diagnostic_bag:expr, NumberLiteral) => {
        if matches!(peek_token($tokens).kind, SyntaxTokenKind::NumberLiteral(_)) {
            next_token($tokens)
        } else {
            let current = peek_token($tokens);
            $diagnostic_bag.report_unexpected_token_kind(
                current.span(),
                &current.kind,
                &SyntaxTokenKind::default_number_literal(),
            );
            SyntaxToken::number_literal_no_diagnostics(current.span().start(), "0")
        }
    };
    ($tokens:expr, $diagnostic_bag:expr, Eoi) => {
        if matches!(peek_token($tokens).kind, SyntaxTokenKind::Eoi) {
            next_token($tokens)
        } else {
            let current = peek_token($tokens);
            $diagnostic_bag.report_unexpected_token_kind(
                current.span(),
                &current.kind,
                &SyntaxTokenKind::Eoi,
            );
            SyntaxToken::eoi(current.span().start())
        }
    };
    ($tokens:expr, $diagnostic_bag:expr, RParen) => {
        if matches!(peek_token($tokens).kind, SyntaxTokenKind::RParen) {
            next_token($tokens)
        } else {
            let current = peek_token($tokens);
            $diagnostic_bag.report_unexpected_token_kind(
                current.span(),
                &current.kind,
                &SyntaxTokenKind::RParen,
            );
            SyntaxToken::operator(current.span().start(), ")")
        }
    };
    ($tokens:expr, $diagnostic_bag:expr, LBrace) => {
        if matches!(peek_token($tokens).kind, SyntaxTokenKind::LBrace) {
            next_token($tokens)
        } else {
            let current = peek_token($tokens);
            $diagnostic_bag.report_unexpected_token_kind(
                current.span(),
                &current.kind,
                &SyntaxTokenKind::LBrace,
            );
            SyntaxToken::operator(current.span().start(), "{")
        }
    };
    ($tokens:expr, $diagnostic_bag:expr, RBrace) => {
        if matches!(peek_token($tokens).kind, SyntaxTokenKind::RBrace) {
            next_token($tokens)
        } else {
            let current = peek_token($tokens);
            $diagnostic_bag.report_unexpected_token_kind(
                current.span(),
                &current.kind,
                &SyntaxTokenKind::RBrace,
            );
            SyntaxToken::operator(current.span().start(), "}")
        }
    };
    ($tokens:expr, $diagnostic_bag:expr, Equals) => {
        if matches!(peek_token($tokens).kind, SyntaxTokenKind::Equals) {
            next_token($tokens)
        } else {
            let current = peek_token($tokens);
            $diagnostic_bag.report_unexpected_token_kind(
                current.span(),
                &current.kind,
                &SyntaxTokenKind::Equals,
            );
            SyntaxToken::operator(current.span().start(), "=")
        }
    };
    ($tokens:expr, $diagnostic_bag:expr, Semicolon) => {
        if matches!(peek_token($tokens).kind, SyntaxTokenKind::Semicolon) {
            next_token($tokens)
        } else {
            let current = peek_token($tokens);
            $diagnostic_bag.report_unexpected_token_kind(
                current.span(),
                &current.kind,
                &SyntaxTokenKind::Semicolon,
            );
            SyntaxToken::operator(current.span().start(), ";")
        }
    };
    ($tokens:expr, $diagnostic_bag:expr, Comma) => {
        if matches!(peek_token($tokens).kind, SyntaxTokenKind::Comma) {
            next_token($tokens)
        } else {
            let current = peek_token($tokens);
            $diagnostic_bag.report_unexpected_token_kind(
                current.span(),
                &current.kind,
                &SyntaxTokenKind::Comma,
            );
            SyntaxToken::operator(current.span().start(), ",")
        }
    };
    ($tokens:expr, $diagnostic_bag:expr, IfKeyword) => {
        if matches!(peek_token($tokens).kind, SyntaxTokenKind::IfKeyword) {
            next_token($tokens)
        } else {
            let current = peek_token($tokens);
            $diagnostic_bag.report_unexpected_token_kind(
                current.span(),
                &current.kind,
                &SyntaxTokenKind::IfKeyword,
            );
            SyntaxToken::keyword(current.span().start(), "if")
        }
    };
    ($tokens:expr, $diagnostic_bag:expr, LetKeyword) => {
        if matches!(peek_token($tokens).kind, SyntaxTokenKind::LetKeyword) {
            next_token($tokens)
        } else {
            let current = peek_token($tokens);
            $diagnostic_bag.report_unexpected_token_kind(
                current.span(),
                &current.kind,
                &SyntaxTokenKind::LetKeyword,
            );
            SyntaxToken::keyword(current.span().start(), "let")
        }
    };
    ($tokens:expr, $diagnostic_bag:expr, WhileKeyword) => {
        if matches!(peek_token($tokens).kind, SyntaxTokenKind::WhileKeyword) {
            next_token($tokens)
        } else {
            let current = peek_token($tokens);
            $diagnostic_bag.report_unexpected_token_kind(
                current.span(),
                &current.kind,
                &SyntaxTokenKind::WhileKeyword,
            );
            SyntaxToken::keyword(current.span().start(), "while")
        }
    };
    ($tokens:expr, $diagnostic_bag:expr, Identifier) => {
        if matches!(peek_token($tokens).kind, SyntaxTokenKind::Identifier) {
            next_token($tokens)
        } else {
            let current = peek_token($tokens);
            $diagnostic_bag.report_unexpected_token_kind(
                current.span(),
                &current.kind,
                &SyntaxTokenKind::Identifier,
            );
            SyntaxToken::identifier(current.span().start(), "")
        }
    };
}
