#[cfg(test)]
mod tests;

use crate::text::TextSpan;

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
            kind: SyntaxTokenKind::NumberLiteral,
        }
    };
}

impl<'a> SyntaxToken<'a> {
    pub fn error(start: usize, kind: SyntaxTokenKind) -> Self {
        Self {
            start,
            lexeme: "error",
            kind,
        }
    }

    pub fn number_literal(start: usize, lexeme: &'a str) -> Self {
        Self {
            start,
            lexeme,
            kind: SyntaxTokenKind::NumberLiteral,
        }
    }

    pub fn string_literal(start: usize, lexeme: &'a str) -> Self {
        Self {
            start,
            lexeme,
            kind: SyntaxTokenKind::StringLiteral,
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

    pub fn bracket_pair(lbracket: SyntaxToken, _rbracket: SyntaxToken) -> Self {
        Self {
            kind: SyntaxTokenKind::BracketPair,
            lexeme: "[]", // TODO: use actual lexeme
            start: lbracket.start,
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
        TextSpan::new(self.start, self.lexeme.chars().count(), false)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum SyntaxTokenKind {
    Eoi,
    NumberLiteral,
    StringLiteral,
    LParen,
    RParen,
    LBracket,
    RBracket,
    BracketPair,
    LBrace,
    RBrace,
    Semicolon,
    Colon,
    Comma,
    Period,
    PeriodPeriod,
    Ampersand,
    AmpersandAmpersand,
    PipePipe,
    QuestionMark,
    QuestionMarkQuestionMark,
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
    Arrow,
    Identifier,

    // Keywords
    AsKeyword,
    CastKeyword,
    ConstKeyword,
    ElseKeyword,
    EnumKeyword,
    FalseKeyword,
    ForKeyword,
    FuncKeyword,
    GenericKeyword,
    IfKeyword,
    ImportKeyword,
    InKeyword,
    LetKeyword,
    NewKeyword,
    NoneKeyword,
    ReturnKeyword,
    StructKeyword,
    TrueKeyword,
    WhileKeyword,
}

impl SyntaxTokenKind {
    pub fn unary_precedence(&self) -> u32 {
        match self {
            Self::Plus | Self::Minus | Self::Bang => 9,
            _ => 0,
        }
    }

    pub fn binary_precedence(&self) -> u32 {
        match self {
            Self::Star | Self::Slash => 8,
            Self::Plus | Self::Minus => 7,
            Self::QuestionMarkQuestionMark => 6,
            Self::PeriodPeriod => 5,
            Self::EqualsEquals
            | Self::BangEquals
            | Self::LessThan
            | Self::LessThanEquals
            | Self::GreaterThan
            | Self::GreaterThanEquals => 4,
            Self::AmpersandAmpersand => 3,
            Self::PipePipe => 2,
            Self::Equals => 1,
            _ => 0,
        }
    }

    // TODO: Add test, that asserts, that is_keyword covers all of the keyword constructor.
    pub fn is_keyword(&self) -> bool {
        matches!(
            self,
            Self::FalseKeyword | Self::TrueKeyword | Self::LetKeyword
        )
    }

    pub fn keyword(identifier: &str) -> Option<Self> {
        match identifier {
            "as" => Some(Self::AsKeyword),
            "cast" => Some(Self::CastKeyword),
            "const" => Some(Self::ConstKeyword),
            "else" => Some(Self::ElseKeyword),
            "enum" => Some(Self::EnumKeyword),
            "false" => Some(Self::FalseKeyword),
            "for" => Some(Self::ForKeyword),
            "func" => Some(Self::FuncKeyword),
            "generic" => Some(Self::GenericKeyword),
            "if" => Some(Self::IfKeyword),
            "import" => Some(Self::ImportKeyword),
            "in" => Some(Self::InKeyword),
            "let" => Some(Self::LetKeyword),
            "new" => Some(Self::NewKeyword),
            "none" => Some(Self::NoneKeyword),
            "return" => Some(Self::ReturnKeyword),
            "struct" => Some(Self::StructKeyword),
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
            "[" => Self::LBracket,
            "]" => Self::RBracket,
            "{" => Self::LBrace,
            "}" => Self::RBrace,
            "=" => Self::Equals,
            ";" => Self::Semicolon,
            ":" => Self::Colon,
            "," => Self::Comma,
            "." => Self::Period,
            ".." => Self::PeriodPeriod,
            "&" => Self::Ampersand,
            "&&" => Self::AmpersandAmpersand,
            "||" => Self::PipePipe,
            "?" => Self::QuestionMark,
            "??" => Self::QuestionMarkQuestionMark,
            "==" => Self::EqualsEquals,
            "!=" => Self::BangEquals,
            "<=" => Self::LessThanEquals,
            ">=" => Self::GreaterThanEquals,
            "<" => Self::LessThan,
            ">" => Self::GreaterThan,
            "->" => Self::Arrow,
            "" => Self::Eoi,
            _ => panic!("Unrecognized Lexeme '{}'", lexeme),
        }
    }
}

impl std::fmt::Debug for SyntaxTokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxTokenKind::NumberLiteral => write!(f, "Number-Literal-Token"),
            SyntaxTokenKind::StringLiteral => write!(f, "String-Literal-Token"),
            SyntaxTokenKind::Eoi => write!(f, "End-of-Input-Token"),
            SyntaxTokenKind::Semicolon => write!(f, "SemicolonToken"),
            SyntaxTokenKind::Colon => write!(f, "ColonToken"),
            SyntaxTokenKind::Comma => write!(f, "CommaToken"),
            SyntaxTokenKind::Period => write!(f, "PeriodToken"),
            SyntaxTokenKind::PeriodPeriod => write!(f, "Period-Period-Token"),
            SyntaxTokenKind::Ampersand => write!(f, "Ampersand-Token"),
            SyntaxTokenKind::AmpersandAmpersand => write!(f, "Ampersand-Ampersand-Token"),
            SyntaxTokenKind::PipePipe => write!(f, "Pipe-Pipe-Token"),
            SyntaxTokenKind::QuestionMark => write!(f, "Question-Mark-Token"),
            SyntaxTokenKind::QuestionMarkQuestionMark => {
                write!(f, "Question-Mark-Question-Mark-Token")
            }
            SyntaxTokenKind::LParen => write!(f, "Open-Parenthesis-Token"),
            SyntaxTokenKind::RParen => write!(f, "Close-Parenthesis-Token"),
            SyntaxTokenKind::LBracket => write!(f, "Open-Bracket-Token"),
            SyntaxTokenKind::RBracket => write!(f, "Close-Bracket-Token"),
            SyntaxTokenKind::BracketPair => write!(f, "Bracket-Pair-Token"),
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
            SyntaxTokenKind::Arrow => write!(f, "Arrow-Token"),
            SyntaxTokenKind::Identifier => write!(f, "Identifier"),
            SyntaxTokenKind::AsKeyword => write!(f, "as"),
            SyntaxTokenKind::CastKeyword => write!(f, "cast"),
            SyntaxTokenKind::ConstKeyword => write!(f, "const"),
            SyntaxTokenKind::ElseKeyword => write!(f, "else"),
            SyntaxTokenKind::EnumKeyword => write!(f, "enum"),
            SyntaxTokenKind::FalseKeyword => write!(f, "false"),
            SyntaxTokenKind::ForKeyword => write!(f, "for"),
            SyntaxTokenKind::FuncKeyword => write!(f, "func"),
            SyntaxTokenKind::GenericKeyword => write!(f, "generic"),
            SyntaxTokenKind::IfKeyword => write!(f, "if"),
            SyntaxTokenKind::ImportKeyword => write!(f, "import"),
            SyntaxTokenKind::InKeyword => write!(f, "in"),
            SyntaxTokenKind::LetKeyword => write!(f, "let"),
            SyntaxTokenKind::NewKeyword => write!(f, "new"),
            SyntaxTokenKind::NoneKeyword => write!(f, "none"),
            SyntaxTokenKind::ReturnKeyword => write!(f, "return"),
            SyntaxTokenKind::StructKeyword => write!(f, "struct"),
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

#[derive(Clone)]
pub struct StringLiteralKind {
    pub value: String,
}

impl std::fmt::Debug for StringLiteralKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "StringLiteralKind({:#?})", self.value)
    }
}
