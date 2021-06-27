use assert_matches::assert_matches;

use crate::{
    lexer::syntax_token::{SyntaxToken, SyntaxTokenKind},
    text::TextSpan,
    value::Value,
};

#[derive(Debug)]
pub struct SyntaxNode<'a> {
    pub kind: SyntaxNodeKind<'a>,
    pub span: TextSpan,
    pub is_inserted: bool,
}

impl<'a> SyntaxNode<'a> {
    pub fn binary(
        lhs: SyntaxNode<'a>,
        operator_token: SyntaxToken<'a>,
        rhs: SyntaxNode<'a>,
    ) -> Self {
        let span = TextSpan::bounds(lhs.span, rhs.span);
        Self {
            kind: SyntaxNodeKind::Binary(BinaryNodeKind {
                lhs: Box::new(lhs),
                operator_token,
                rhs: Box::new(rhs),
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn error(start: usize) -> Self {
        let mut result = Self::literal(SyntaxToken::number_literal_no_diagnostics(start, "0"));
        result.is_inserted = true;
        result
    }

    pub fn literal(token: SyntaxToken<'a>) -> Self {
        let value =
            assert_matches!(&token.kind, SyntaxTokenKind::NumberLiteral(n) => n).value as i64;
        let value = value.into();
        Self {
            span: token.span(),
            kind: SyntaxNodeKind::Literal(LiteralNodeKind { token, value }),
            is_inserted: false,
        }
    }

    pub fn true_literal(token: SyntaxToken<'a>) -> Self {
        let value = true.into();
        Self {
            span: token.span(),
            kind: SyntaxNodeKind::Literal(LiteralNodeKind { token, value }),
            is_inserted: false,
        }
    }

    pub fn false_literal(token: SyntaxToken<'a>) -> Self {
        let value = false.into();
        Self {
            span: token.span(),
            kind: SyntaxNodeKind::Literal(LiteralNodeKind { token, value }),
            is_inserted: false,
        }
    }

    pub fn variable(token: SyntaxToken<'a>) -> Self {
        Self {
            span: token.span(),
            kind: SyntaxNodeKind::Variable(VariableNodeKind { token }),
            is_inserted: false,
        }
    }

    pub fn unary(operator_token: SyntaxToken<'a>, operand: SyntaxNode<'a>) -> Self {
        let span = TextSpan::bounds(operator_token.span(), operand.span());
        Self {
            kind: SyntaxNodeKind::Unary(UnaryNodeKind {
                operator_token,
                operand: Box::new(operand),
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn parenthesized(
        lparen: SyntaxToken<'a>,
        expression: SyntaxNode<'a>,
        rparen: SyntaxToken<'a>,
    ) -> Self {
        let span = TextSpan::bounds(lparen.span(), rparen.span());
        Self {
            kind: SyntaxNodeKind::Parenthesized(ParenthesizedNodeKind {
                lparen,
                expression: Box::new(expression),
                rparen,
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn function_call(
        base: SyntaxNode<'a>,
        open_parenthesis_token: SyntaxToken<'a>,
        arguments: Vec<SyntaxNode<'a>>,
        comma_tokens: Vec<SyntaxToken<'a>>,
        close_parenthesis_token: SyntaxToken<'a>,
    ) -> Self {
        let span = TextSpan::bounds(base.span(), close_parenthesis_token.span());
        Self {
            kind: SyntaxNodeKind::FunctionCall(FunctionCallNodeKind {
                base: Box::new(base),
                open_parenthesis_token,
                arguments,
                comma_tokens,
                close_parenthesis_token,
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn if_statement(
        if_keyword: SyntaxToken<'a>,
        condition: SyntaxNode<'a>,
        body: SyntaxNode<'a>,
    ) -> Self {
        let span = TextSpan::bounds(if_keyword.span(), body.span());
        Self {
            kind: SyntaxNodeKind::IfStatement(IfStatementNodeKind {
                if_keyword,
                condition: Box::new(condition),
                body: Box::new(body),
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn variable_declaration(
        let_keyword: SyntaxToken<'a>,
        identifier: SyntaxToken<'a>,
        equals_token: SyntaxToken<'a>,
        initializer: SyntaxNode<'a>,
        semicolon_token: SyntaxToken<'a>,
    ) -> Self {
        let span = TextSpan::bounds(let_keyword.span(), semicolon_token.span());
        Self {
            kind: SyntaxNodeKind::VariableDeclaration(VariableDeclarationNodeKind {
                let_keyword,
                identifier,
                equals_token,
                initializer: Box::new(initializer),
                semicolon_token,
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn while_statement(
        while_keyword: SyntaxToken<'a>,
        condition: SyntaxNode<'a>,
        body: SyntaxNode<'a>,
    ) -> Self {
        let span = TextSpan::bounds(while_keyword.span(), body.span());
        Self {
            kind: SyntaxNodeKind::WhileStatement(WhileStatementNodeKind {
                while_keyword,
                condition: Box::new(condition),
                body: Box::new(body),
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn assignment(
        lhs: SyntaxNode<'a>,
        expression: SyntaxNode<'a>,
        semicolon_token: SyntaxToken<'a>,
    ) -> Self {
        let span = TextSpan::bounds(lhs.span(), semicolon_token.span());
        Self {
            kind: SyntaxNodeKind::Assignment(AssignmentNodeKind {
                lhs: Box::new(lhs),
                expression: Box::new(expression),
                semicolon_token,
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn block_statement(
        lbrace: SyntaxToken<'a>,
        statements: Vec<SyntaxNode<'a>>,
        rbrace: SyntaxToken<'a>,
    ) -> Self {
        let span = TextSpan::bounds(lbrace.span(), rbrace.span());
        Self {
            kind: SyntaxNodeKind::BlockStatement(BlockStatementNodeKind {
                lbrace,
                statements,
                rbrace,
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn expression_statement(
        expression: SyntaxNode<'a>,
        semicolon_token: SyntaxToken<'a>,
    ) -> Self {
        let span = TextSpan::bounds(expression.span(), semicolon_token.span());
        Self {
            kind: SyntaxNodeKind::ExpressionStatement(ExpressionStatementNodeKind {
                expression: Box::new(expression),
                semicolon_token,
            }),
            span,
            is_inserted: false,
        }
    }

    pub fn span(&self) -> TextSpan {
        self.span
    }
}

#[derive(Debug)]
pub enum SyntaxNodeKind<'a> {
    //Expressions
    Literal(LiteralNodeKind<'a>),
    Variable(VariableNodeKind<'a>),
    Binary(BinaryNodeKind<'a>),
    Unary(UnaryNodeKind<'a>),
    Parenthesized(ParenthesizedNodeKind<'a>),
    FunctionCall(FunctionCallNodeKind<'a>),

    // Statements
    BlockStatement(BlockStatementNodeKind<'a>),
    IfStatement(IfStatementNodeKind<'a>),
    VariableDeclaration(VariableDeclarationNodeKind<'a>),
    WhileStatement(WhileStatementNodeKind<'a>),
    Assignment(AssignmentNodeKind<'a>),
    ExpressionStatement(ExpressionStatementNodeKind<'a>),
}

impl SyntaxNodeKind<'_> {
    pub fn is_assignable(&self) -> bool {
        matches!(self, Self::Variable(_))
    }
}

#[derive(Debug)]
pub struct LiteralNodeKind<'a> {
    pub token: SyntaxToken<'a>,
    pub value: Value,
}

#[derive(Debug)]
pub struct VariableNodeKind<'a> {
    pub token: SyntaxToken<'a>,
}

#[derive(Debug)]
pub struct BinaryNodeKind<'a> {
    pub lhs: Box<SyntaxNode<'a>>,
    pub operator_token: SyntaxToken<'a>,
    pub rhs: Box<SyntaxNode<'a>>,
}

#[derive(Debug)]
pub struct UnaryNodeKind<'a> {
    pub operator_token: SyntaxToken<'a>,
    pub operand: Box<SyntaxNode<'a>>,
}

#[derive(Debug)]
pub struct ParenthesizedNodeKind<'a> {
    pub lparen: SyntaxToken<'a>,
    pub expression: Box<SyntaxNode<'a>>,
    pub rparen: SyntaxToken<'a>,
}

#[derive(Debug)]
pub struct FunctionCallNodeKind<'a> {
    pub base: Box<SyntaxNode<'a>>,
    pub open_parenthesis_token: SyntaxToken<'a>,
    pub arguments: Vec<SyntaxNode<'a>>,
    pub comma_tokens: Vec<SyntaxToken<'a>>,
    pub close_parenthesis_token: SyntaxToken<'a>,
}

impl FunctionCallNodeKind<'_> {
    pub fn argument_span(&self) -> TextSpan {
        TextSpan::bounds(self.open_parenthesis_token.span(), self.close_parenthesis_token.span())
    }
}

#[derive(Debug)]
pub struct BlockStatementNodeKind<'a> {
    pub lbrace: SyntaxToken<'a>,
    pub statements: Vec<SyntaxNode<'a>>,
    pub rbrace: SyntaxToken<'a>,
}

#[derive(Debug)]
pub struct IfStatementNodeKind<'a> {
    pub if_keyword: SyntaxToken<'a>,
    pub condition: Box<SyntaxNode<'a>>,
    pub body: Box<SyntaxNode<'a>>,
}

#[derive(Debug)]
pub struct VariableDeclarationNodeKind<'a> {
    pub let_keyword: SyntaxToken<'a>,
    pub identifier: SyntaxToken<'a>,
    pub equals_token: SyntaxToken<'a>,
    pub initializer: Box<SyntaxNode<'a>>,
    pub semicolon_token: SyntaxToken<'a>,
}

#[derive(Debug)]
pub struct WhileStatementNodeKind<'a> {
    pub while_keyword: SyntaxToken<'a>,
    pub condition: Box<SyntaxNode<'a>>,
    pub body: Box<SyntaxNode<'a>>,
}

#[derive(Debug)]
pub struct AssignmentNodeKind<'a> {
    pub lhs: Box<SyntaxNode<'a>>,
    pub expression: Box<SyntaxNode<'a>>,
    pub semicolon_token: SyntaxToken<'a>,
}

#[derive(Debug)]
pub struct ExpressionStatementNodeKind<'a> {
    pub expression: Box<SyntaxNode<'a>>,
    pub semicolon_token: SyntaxToken<'a>,
}
