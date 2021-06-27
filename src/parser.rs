mod match_token;
pub mod syntax_nodes;
#[cfg(test)]
mod tests;

use std::collections::VecDeque;

use crate::{
    diagnostics::DiagnosticBag,
    lexer::{
        self,
        syntax_token::{SyntaxToken, SyntaxTokenKind},
    },
    text::SourceText,
};

use self::syntax_nodes::SyntaxNode;
use crate::match_token;

pub fn parse<'a>(
    source_text: &'a SourceText<'a>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let mut tokens = lexer::lex(source_text, diagnostic_bag);
    if diagnostic_bag.has_errors() {
        return SyntaxNode::error(0);
    }
    let result = parse_statement(&mut tokens, diagnostic_bag);
    match_token!(&mut tokens, diagnostic_bag, Eoi);
    result
}

fn parse_statement<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    match &peek_token(tokens).kind {
        SyntaxTokenKind::LBrace => parse_block_statement(tokens, diagnostic_bag),
        SyntaxTokenKind::IfKeyword => parse_if_statement(tokens, diagnostic_bag),
        SyntaxTokenKind::LetKeyword => parse_variable_declaration(tokens, diagnostic_bag),
        SyntaxTokenKind::WhileKeyword => parse_while_statement(tokens, diagnostic_bag),
        _ => parse_assignment_statement(tokens, diagnostic_bag),
    }
}

fn next_token<'a>(tokens: &mut VecDeque<SyntaxToken<'a>>) -> SyntaxToken<'a> {
    if tokens.is_empty() {
        panic!("No tokens could be found!")
    } else if tokens.len() == 1 {
        peek_token(tokens).clone()
    } else {
        tokens.pop_front().unwrap()
    }
}

fn peek_token<'a, 'b>(tokens: &'b mut VecDeque<SyntaxToken<'a>>) -> &'b SyntaxToken<'a> {
    if tokens.is_empty() {
        panic!("No tokens could be found!")
    } else {
        tokens.front().unwrap()
    }
}

fn parse_block_statement<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let open_brace_token = match_token!(tokens, diagnostic_bag, LBrace);
    let mut statements = vec![];
    while !matches!(
        peek_token(tokens).kind,
        SyntaxTokenKind::RBrace | SyntaxTokenKind::Eoi
    ) {
        statements.push(parse_statement(tokens, diagnostic_bag));
    }
    let close_brace_token = match_token!(tokens, diagnostic_bag, RBrace);
    SyntaxNode::block_statement(open_brace_token, statements, close_brace_token)
}

fn parse_if_statement<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let if_keyword = match_token!(tokens, diagnostic_bag, IfKeyword);
    let condition = parse_expression(tokens, diagnostic_bag);
    let body = parse_block_statement(tokens, diagnostic_bag);

    SyntaxNode::if_statement(if_keyword, condition, body)
}

fn parse_variable_declaration<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let let_keyword = match_token!(tokens, diagnostic_bag, LetKeyword);
    let identifier = if peek_token(tokens).kind.is_keyword() {
        let token = next_token(tokens);
        diagnostic_bag.report_cannot_declare_keyword_as_variable(token.span(), token.lexeme);
        SyntaxToken::identifier(token.span().start(), "")
    } else {
        match_token!(tokens, diagnostic_bag, Identifier)
    };
    let equals_token = match_token!(tokens, diagnostic_bag, Equals);
    let initializer = parse_expression(tokens, diagnostic_bag);
    let semicolon_token = match_token!(tokens, diagnostic_bag, Semicolon);
    SyntaxNode::variable_declaration(
        let_keyword,
        identifier,
        equals_token,
        initializer,
        semicolon_token,
    )
}

fn parse_while_statement<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let while_keyword = match_token!(tokens, diagnostic_bag, WhileKeyword);
    let condition = parse_expression(tokens, diagnostic_bag);
    let body = parse_block_statement(tokens, diagnostic_bag);
    SyntaxNode::while_statement(while_keyword, condition, body)
}

fn parse_assignment_statement<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let lhs = parse_binary_with_parent_precedence(tokens, diagnostic_bag, 1);
    if matches!(peek_token(tokens).kind, SyntaxTokenKind::Equals) {
        if !lhs.kind.is_assignable() && !lhs.is_inserted {
            diagnostic_bag.report_cannot_assign_to(lhs.span);
        }
        next_token(tokens);
        let expression = parse_expression(tokens, diagnostic_bag);
        let semicolon_token = match_token!(tokens, diagnostic_bag, Semicolon);
        SyntaxNode::assignment(lhs, expression, semicolon_token)
    } else {
        let semicolon_token = match_token!(tokens, diagnostic_bag, Semicolon);
        SyntaxNode::expression_statement(lhs, semicolon_token)
    }
}

fn parse_expression<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    parse_binary(tokens, diagnostic_bag)
}

fn parse_binary<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    parse_binary_with_parent_precedence(tokens, diagnostic_bag, 0)
}

fn parse_binary_with_parent_precedence<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
    parent_precedence: u32,
) -> SyntaxNode<'a> {
    let mut lhs;
    let unary_precedence = peek_token(tokens).kind.unary_precedence();
    if unary_precedence != 0 && unary_precedence >= parent_precedence {
        let operator_token = next_token(tokens);
        let operand = parse_binary_with_parent_precedence(tokens, diagnostic_bag, unary_precedence);
        lhs = SyntaxNode::unary(operator_token, operand);
    } else {
        lhs = parse_function_call(tokens, diagnostic_bag);
    }

    loop {
        let precedence = peek_token(tokens).kind.binary_precedence();
        if precedence == 0 || precedence <= parent_precedence {
            break;
        }

        let operator_token = next_token(tokens);
        if matches!(operator_token.kind, SyntaxTokenKind::Equals) {
            diagnostic_bag.report_cannot_assign_to(lhs.span);
        }
        let rhs = parse_binary_with_parent_precedence(tokens, diagnostic_bag, precedence);
        lhs = SyntaxNode::binary(lhs, operator_token, rhs)
    }
    lhs
}

fn parse_function_call<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let base = parse_primary(tokens, diagnostic_bag);
    if matches!(peek_token(tokens).kind, SyntaxTokenKind::LParen) {
        let open_parenthesis_token = next_token(tokens);
        let mut arguments = vec![];
        let mut comma_tokens = vec![];
        while !matches!(
            peek_token(tokens).kind,
            // Currently there are no statements allowed as arguments to a
            // function. This may change..
            SyntaxTokenKind::RParen | SyntaxTokenKind::Eoi | SyntaxTokenKind::Semicolon
        ) {
            arguments.push(parse_expression(tokens, diagnostic_bag));
            if !matches!(peek_token(tokens).kind, SyntaxTokenKind::RParen) {
                comma_tokens.push(match_token!(tokens, diagnostic_bag, Comma));
            }
        }
        let close_parenthesis_token = match_token!(tokens, diagnostic_bag, RParen);
        SyntaxNode::function_call(base, open_parenthesis_token, arguments, comma_tokens, close_parenthesis_token)
    } else {
        base
    }
}

fn parse_primary<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let span = peek_token(tokens).span();
    match &peek_token(tokens).kind {
        SyntaxTokenKind::LParen => {
            let lparen = next_token(tokens);
            let expression = parse_expression(tokens, diagnostic_bag);
            let rparen = match_token!(tokens, diagnostic_bag, RParen);
            SyntaxNode::parenthesized(lparen, expression, rparen)
        }
        SyntaxTokenKind::NumberLiteral(_) => parse_number_literal(tokens, diagnostic_bag),
        SyntaxTokenKind::TrueKeyword | SyntaxTokenKind::FalseKeyword => {
            parse_boolean_literal(tokens, diagnostic_bag)
        }
        SyntaxTokenKind::Identifier => parse_identifier(tokens, diagnostic_bag),
        unexpected_token => {
            diagnostic_bag.report_unexpected_token_kind(
                span,
                unexpected_token,
                &SyntaxTokenKind::default_number_literal(),
            );
            // next_token(tokens);
            SyntaxNode::error(span.start())
        }
    }
}

fn parse_number_literal<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    SyntaxNode::literal(match_token!(tokens, diagnostic_bag, NumberLiteral))
}

fn parse_boolean_literal<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    _: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let token = next_token(tokens);
    match token.kind {
        SyntaxTokenKind::TrueKeyword => SyntaxNode::true_literal(token),
        SyntaxTokenKind::FalseKeyword => SyntaxNode::false_literal(token),
        _ => unreachable!(),
    }
}

fn parse_identifier<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    SyntaxNode::variable(match_token!(tokens, diagnostic_bag, Identifier))
}
