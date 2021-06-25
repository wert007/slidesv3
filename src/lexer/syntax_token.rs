#[cfg(test)]
mod tests;

use crate::{diagnostics::DiagnosticBag, text::TextSpan};

#[derive(Debug, Clone)]
pub struct SyntaxToken<'a> {
    pub kind: SyntaxTokenKind,
    pub lexeme: &'a str,
    pub start: usize,
}

#[macro_export]
macro_rules! const_number_literal_syntax_token {
    ($start:expr, $value:literal) => {
        SyntaxToken {
            start: $start,
            lexeme: stringify!($value),
            kind: SyntaxTokenKind::NumberLiteral(crate::lexer::syntax_token::NumberLiteralKind {
                value: $value,
            }),
        }
    };
}

impl<'a> SyntaxToken<'a> {
    pub fn number_literal(
        start: usize,
        lexeme: &'a str,
        diagnostic_bag: &mut DiagnosticBag,
    ) -> Self {
        let value = match lexeme.parse::<u64>() {
            Ok(value) => value,
            Err(_) => {
                // TODO: Someday ParseIntError::kind() might allow to differentiate more
                diagnostic_bag.report_bad_integer(start, lexeme);
                0
            }
        };
        Self {
            start,
            lexeme,
            kind: SyntaxTokenKind::NumberLiteral(NumberLiteralKind { value }),
        }
    }

    pub fn keyword(start: usize, lexeme: &'a str) -> Self {
        let kind = SyntaxTokenKind::keyword(lexeme).expect(lexeme);
        Self {
            kind,
            lexeme,
            start,
        }
    }

    pub fn identifier(start: usize, lexeme: &'a str) -> Self {
        Self {
            kind: SyntaxTokenKind::Identifier,
            lexeme,
            start,
        }
    }

    pub fn operator(start: usize, lexeme: &'a str) -> Self {
        let kind = lexeme.into();
        Self {
            kind,
            lexeme,
            start,
        }
    }

    pub fn eoi(start: usize) -> Self {
        Self {
            kind: SyntaxTokenKind::Eoi,
            lexeme: "",
            start,
        }
    }

    pub fn span(&self) -> TextSpan {
        TextSpan::new(self.start, self.lexeme.chars().count())
    }
}

#[derive(Clone)]
pub enum SyntaxTokenKind {
    Eoi,
    NumberLiteral(NumberLiteralKind),
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,
    Plus,
    Minus,
    Star,
    Slash,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
    Bang,
    Equals,
    BangEquals,
    EqualsEquals,

    Identifier,

    // Keywords
    FalseKeyword,
    IfKeyword,
    LetKeyword,
    TrueKeyword,
    WhileKeyword,
}

impl SyntaxTokenKind {
    pub fn unary_precedence(&self) -> u32 {
        match self {
            Self::Plus | Self::Minus => 5,
            _ => 0,
        }
    }

    pub fn binary_precedence(&self) -> u32 {
        match self {
            Self::Star | Self::Slash => 4,
            Self::Plus | Self::Minus => 3,
            Self::EqualsEquals
            | Self::BangEquals
            | Self::LessThan
            | Self::LessThanEquals
            | Self::GreaterThan
            | Self::GreaterThanEquals => 2,
            Self::Equals => 1,
            _ => 0,
        }
    }

    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            Self::FalseKeyword | Self::TrueKeyword | Self::LetKeyword
        )
    }

    pub fn default_number_literal() -> Self {
        SyntaxTokenKind::NumberLiteral(NumberLiteralKind { value: 0 })
    }

    pub fn keyword(identifier: &str) -> Option<Self> {
        match identifier {
            "false" => Some(Self::FalseKeyword),
            "if" => Some(Self::IfKeyword),
            "let" => Some(Self::LetKeyword),
            "true" => Some(Self::TrueKeyword),
            "while" => Some(Self::WhileKeyword),
            _ => None,
        }
    }
}

impl From<&str> for SyntaxTokenKind {
    fn from(lexeme: &str) -> Self {
        match lexeme {
            "+" => Self::Plus,
            "-" => Self::Minus,
            "*" => Self::Star,
            "/" => Self::Slash,
            "!" => Self::Bang,
            "(" => Self::LParen,
            ")" => Self::RParen,
            "{" => Self::LBrace,
            "}" => Self::RBrace,
            "=" => Self::Equals,
            ";" => Self::Semicolon,
            "==" => Self::EqualsEquals,
            "!=" => Self::BangEquals,
            "<=" => Self::LessThanEquals,
            ">=" => Self::GreaterThanEquals,
            "<" => Self::LessThan,
            ">" => Self::GreaterThan,
            "" => Self::Eoi,
            _ => panic!("Unrecognized Lexeme '{}'", lexeme),
        }
    }
}

impl std::fmt::Debug for SyntaxTokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxTokenKind::NumberLiteral(v) => write!(f, "{:?}", v),
            SyntaxTokenKind::Eoi => write!(f, "End-of-Input-Token"),
            SyntaxTokenKind::Semicolon => write!(f, "SemicolonToken"),
            SyntaxTokenKind::LParen => write!(f, "Open-Parenthesis-Token"),
            SyntaxTokenKind::RParen => write!(f, "Close-Parenthesis-Token"),
            SyntaxTokenKind::LBrace => write!(f, "Open-Brace-Token"),
            SyntaxTokenKind::RBrace => write!(f, "Close-Brace-Token"),
            SyntaxTokenKind::Plus => write!(f, "PlusToken"),
            SyntaxTokenKind::Minus => write!(f, "MinusToken"),
            SyntaxTokenKind::Star => write!(f, "StarToken"),
            SyntaxTokenKind::Slash => write!(f, "SlashToken"),
            SyntaxTokenKind::LessThan => write!(f, "Less-Than-Token"),
            SyntaxTokenKind::GreaterThan => write!(f, "Greater-Than-Token"),
            SyntaxTokenKind::LessThanEquals => write!(f, "Less-Than-Equals-Token"),
            SyntaxTokenKind::GreaterThanEquals => write!(f, "Greater-Than-Equals-Token"),
            SyntaxTokenKind::Bang => write!(f, "BangToken"),
            SyntaxTokenKind::Equals => write!(f, "EqualsToken"),
            SyntaxTokenKind::BangEquals => write!(f, "Bang-Equals-Token"),
            SyntaxTokenKind::EqualsEquals => write!(f, "Equals-Equals-Token"),
            SyntaxTokenKind::Identifier => write!(f, "Identifier"),
            SyntaxTokenKind::FalseKeyword => write!(f, "false"),
            SyntaxTokenKind::IfKeyword => write!(f, "if"),
            SyntaxTokenKind::LetKeyword => write!(f, "let"),
            SyntaxTokenKind::TrueKeyword => write!(f, "true"),
            SyntaxTokenKind::WhileKeyword => write!(f, "while"),
        }
    }
}

#[derive(Clone)]
pub struct NumberLiteralKind {
    pub value: u64,
}

impl std::fmt::Debug for NumberLiteralKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NumberLiteralKind({})", self.value)
    }
}
