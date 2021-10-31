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
    parser::syntax_nodes::{FunctionTypeNode, ReturnTypeNode, TypeDeclaration},
    text::{SourceText, TextSpan},
    DebugFlags,
};

use self::syntax_nodes::{ElseClause, ParameterNode, StructBodyNode, SyntaxNode, TypeNode};
use crate::match_token;

pub fn parse<'a>(
    source_text: &'a SourceText<'a>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
    debug_flags: DebugFlags,
) -> SyntaxNode<'a> {
    let mut tokens = lexer::lex(source_text, diagnostic_bag, debug_flags);
    if diagnostic_bag.has_errors() {
        return SyntaxNode::error(0);
    }
    parse_compilation_unit(&mut tokens, diagnostic_bag)
}

fn parse_compilation_unit<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let mut statements = vec![];
    while !matches!(&peek_token(tokens).kind, SyntaxTokenKind::Eoi) {
        let token_count = tokens.len();
        statements.push(parse_top_level_statement(tokens, diagnostic_bag));
        if token_count == tokens.len() {
            next_token(tokens);
        }
    }
    let eoi = match_token!(tokens, diagnostic_bag, Eoi);
    SyntaxNode::compilation_unit(statements, eoi)
}

fn parse_top_level_statement<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    match &peek_token(tokens).kind {
        SyntaxTokenKind::FuncKeyword => parse_function_statement(tokens, diagnostic_bag),
        SyntaxTokenKind::StructKeyword => parse_struct_statement(tokens, diagnostic_bag),
        _ => {
            let token = next_token(tokens);
            let span = token.span();
            let actual_token_kind = &token.kind;
            diagnostic_bag.report_unexpected_token_kind(
                span,
                actual_token_kind,
                &SyntaxTokenKind::FuncKeyword,
            );
            SyntaxNode::error(span.start())
        }
    }
}

fn parse_function_statement<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let func_keyword = match_token!(tokens, diagnostic_bag, FuncKeyword);
    let identifier = match_token!(tokens, diagnostic_bag, Identifier);
    let function_type = parse_function_type(tokens, diagnostic_bag);
    let body = parse_block_statement(tokens, diagnostic_bag);

    SyntaxNode::function_declaration(func_keyword, identifier, function_type, body)
}

fn parse_struct_statement<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let struct_keyword = match_token!(tokens, diagnostic_bag, StructKeyword);
    let identifier = match_token!(tokens, diagnostic_bag, Identifier);
    let body = parse_struct_body(tokens, diagnostic_bag);

    SyntaxNode::struct_declaration(struct_keyword, identifier, body)
}

fn parse_function_type<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> FunctionTypeNode<'a> {
    let lparen = match_token!(tokens, diagnostic_bag, LParen);
    let mut parameters = vec![];
    let mut comma_tokens = vec![];
    while !matches!(&peek_token(tokens).kind, SyntaxTokenKind::RParen) {
        let token_count = tokens.len();
        let parameter = parse_parameter(tokens, diagnostic_bag);
        parameters.push(parameter);
        if !matches!(&peek_token(tokens).kind, SyntaxTokenKind::RParen) {
            let comma = match_token!(tokens, diagnostic_bag, Comma);
            comma_tokens.push(comma);
        }
        if token_count == tokens.len() {
            next_token(tokens);
        }
    }
    let rparen = match_token!(tokens, diagnostic_bag, RParen);

    let return_type = if matches!(&peek_token(tokens).kind, SyntaxTokenKind::Arrow) {
        let arrow_token = next_token(tokens);
        let return_type = parse_type(tokens, diagnostic_bag);
        Some(ReturnTypeNode {
            arrow_token,
            return_type,
        })
    } else {
        None
    };

    FunctionTypeNode::new(lparen, parameters, comma_tokens, rparen, return_type)
}

fn parse_struct_body<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> StructBodyNode<'a> {
    let lbrace = match_token!(tokens, diagnostic_bag, LBrace);
    let mut statements = vec![];
    while !matches!(&peek_token(tokens).kind, SyntaxTokenKind::RBrace) {
        let token_count = tokens.len();
        let statement = match &peek_token(tokens).kind {
            SyntaxTokenKind::FuncKeyword => parse_function_statement(tokens, diagnostic_bag),
            SyntaxTokenKind::Identifier => {
                let field = parse_parameter(tokens, diagnostic_bag);
                let semicolon_token = match_token!(tokens, diagnostic_bag, Semicolon);
                SyntaxNode::struct_field(field, semicolon_token)
            }
            _ => {
                let current = next_token(tokens);
                diagnostic_bag.report_unexpected_token_kind(
                    current.span(),
                    &current.kind,
                    &SyntaxTokenKind::Identifier,
                );
                SyntaxNode::error(current.start)
            }
        };
        statements.push(statement);
        if token_count == tokens.len() {
            next_token(tokens);
        }
    }
    let rbrace = match_token!(tokens, diagnostic_bag, RBrace);

    StructBodyNode::new(lbrace, statements, rbrace)
}

fn parse_parameter<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> ParameterNode<'a> {
    let identifier = match_token!(tokens, diagnostic_bag, Identifier);
    let colon_token = match_token!(tokens, diagnostic_bag, Colon);
    let type_ = parse_type(tokens, diagnostic_bag);
    ParameterNode::new(identifier, colon_token, type_)
}

fn parse_type<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> TypeNode<'a> {
    let identifier = match_token!(tokens, diagnostic_bag, Identifier);
    let optional_question_mark =
        if matches!(&peek_token(tokens).kind, &SyntaxTokenKind::QuestionMark) {
            Some(next_token(tokens))
        } else {
            None
        };
    let mut brackets = vec![];
    while matches!(&peek_token(tokens).kind, SyntaxTokenKind::LBracket) {
        let token_count = tokens.len();
        let lbracket = match_token!(tokens, diagnostic_bag, LBracket);
        let rbracket = match_token!(tokens, diagnostic_bag, RBracket);
        if token_count == tokens.len() {
            next_token(tokens);
        }
        brackets.push(SyntaxToken::bracket_pair(lbracket, rbracket));
    }

    TypeNode::new(identifier, optional_question_mark, brackets)
}

fn parse_statement<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    match &peek_token(tokens).kind {
        SyntaxTokenKind::LBrace => parse_block_statement(tokens, diagnostic_bag),
        SyntaxTokenKind::ForKeyword => parse_for_statement(tokens, diagnostic_bag),
        SyntaxTokenKind::IfKeyword => parse_if_statement(tokens, diagnostic_bag),
        SyntaxTokenKind::LetKeyword => parse_variable_declaration(tokens, diagnostic_bag),
        SyntaxTokenKind::ReturnKeyword => parse_return_statement(tokens, diagnostic_bag),
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
        let token_count = tokens.len();
        statements.push(parse_statement(tokens, diagnostic_bag));
        if token_count == tokens.len() {
            next_token(tokens);
        }
    }
    let close_brace_token = match_token!(tokens, diagnostic_bag, RBrace);
    SyntaxNode::block_statement(open_brace_token, statements, close_brace_token)
}

fn parse_for_statement<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let for_keyword = match_token!(tokens, diagnostic_bag, ForKeyword);
    let mut variable = match_token!(tokens, diagnostic_bag, Identifier);
    let optional_index_variable = if matches!(peek_token(tokens).kind, SyntaxTokenKind::Comma) {
        next_token(tokens);
        let it = variable;
        variable = match_token!(tokens, diagnostic_bag, Identifier);
        Some(it)
    } else {
        None
    };
    let in_keyword = match_token!(tokens, diagnostic_bag, InKeyword);
    let collection = parse_expression(tokens, diagnostic_bag);
    let body = parse_block_statement(tokens, diagnostic_bag);

    SyntaxNode::for_statement(
        for_keyword,
        optional_index_variable,
        variable,
        in_keyword,
        collection,
        body,
    )
}

fn parse_if_statement<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let if_keyword = match_token!(tokens, diagnostic_bag, IfKeyword);
    let condition = parse_expression(tokens, diagnostic_bag);
    let body = parse_block_statement(tokens, diagnostic_bag);
    let else_clause = if matches!(&peek_token(tokens).kind, SyntaxTokenKind::ElseKeyword) {
        Some(parse_else_clause(tokens, diagnostic_bag))
    } else {
        None
    };
    SyntaxNode::if_statement(if_keyword, condition, body, else_clause)
}

fn parse_else_clause<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> ElseClause<'a> {
    let else_keyword = match_token!(tokens, diagnostic_bag, ElseKeyword);
    let body = if matches!(peek_token(tokens).kind, SyntaxTokenKind::IfKeyword) {
        parse_if_statement(tokens, diagnostic_bag)
    } else {
        parse_block_statement(tokens, diagnostic_bag)
    };
    ElseClause::new(else_keyword, body)
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
    let optional_type_declaration = if matches!(&peek_token(tokens).kind, &SyntaxTokenKind::Colon) {
        let colon_token = next_token(tokens);
        let type_ = parse_type(tokens, diagnostic_bag);
        Some(TypeDeclaration { colon_token, type_ })
    } else {
        None
    };
    let equals_token = match_token!(tokens, diagnostic_bag, Equals);
    let initializer = parse_expression(tokens, diagnostic_bag);
    let semicolon_token = match_token!(tokens, diagnostic_bag, Semicolon);
    SyntaxNode::variable_declaration(
        let_keyword,
        identifier,
        optional_type_declaration,
        equals_token,
        initializer,
        semicolon_token,
    )
}

fn parse_return_statement<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let return_keyword = match_token!(tokens, diagnostic_bag, ReturnKeyword);
    let optional_expression = if matches!(&peek_token(tokens).kind, SyntaxTokenKind::Semicolon) {
        None
    } else {
        Some(parse_expression(tokens, diagnostic_bag))
    };
    let semicolon_token = match_token!(tokens, diagnostic_bag, Semicolon);

    SyntaxNode::return_statement(return_keyword, optional_expression, semicolon_token)
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
        lhs = SyntaxNode::binary(lhs, operator_token, rhs);
    }
    lhs
}

fn parse_function_call<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let mut base = parse_primary(tokens, diagnostic_bag);
    loop {
        match peek_token(tokens).kind {
            SyntaxTokenKind::LParen => {
                let open_parenthesis_token = next_token(tokens);
                let mut arguments = vec![];
                let mut comma_tokens = vec![];
                while !matches!(
                    peek_token(tokens).kind,
                    // Currently there are no statements allowed as arguments to a
                    // function. This may change..
                    SyntaxTokenKind::RParen | SyntaxTokenKind::Eoi | SyntaxTokenKind::Semicolon
                ) {
                    let token_count = tokens.len();
                    arguments.push(parse_expression(tokens, diagnostic_bag));
                    if !matches!(peek_token(tokens).kind, SyntaxTokenKind::RParen) {
                        comma_tokens.push(match_token!(tokens, diagnostic_bag, Comma));
                    }
                    if token_count == tokens.len() {
                        next_token(tokens);
                    }
                }
                let close_parenthesis_token = match_token!(tokens, diagnostic_bag, RParen);
                base = SyntaxNode::function_call(
                    base,
                    open_parenthesis_token,
                    arguments,
                    comma_tokens,
                    close_parenthesis_token,
                );
            }
            SyntaxTokenKind::LBracket => {
                let lbracket = next_token(tokens);
                let index = parse_expression(tokens, diagnostic_bag);
                let rbracket = match_token!(tokens, diagnostic_bag, RBracket);
                base = SyntaxNode::array_index(base, lbracket, index, rbracket);
            }
            SyntaxTokenKind::Period => {
                let period = next_token(tokens);
                let identifier = match_token!(tokens, diagnostic_bag, Identifier);
                base = SyntaxNode::field_access(base, period, identifier);
            }
            _ => break,
        }
    }
    base
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
        SyntaxTokenKind::LBracket => parse_array_literal(tokens, diagnostic_bag),
        SyntaxTokenKind::NumberLiteral => parse_number_literal(tokens, diagnostic_bag),
        SyntaxTokenKind::StringLiteral => parse_string_literal(tokens, diagnostic_bag),
        SyntaxTokenKind::TrueKeyword | SyntaxTokenKind::FalseKeyword => {
            parse_boolean_literal(tokens, diagnostic_bag)
        }
        SyntaxTokenKind::NoneKeyword => parse_none_literal(tokens, diagnostic_bag),
        SyntaxTokenKind::CastKeyword => parse_cast_expression(tokens, diagnostic_bag),
        SyntaxTokenKind::NewKeyword => parse_constructor(tokens, diagnostic_bag),
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

fn parse_array_literal<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let lbracket = match_token!(tokens, diagnostic_bag, LBracket);
    let mut children = vec![];
    let mut comma_tokens = vec![];
    while !matches!(
        peek_token(tokens).kind,
        SyntaxTokenKind::RBracket | SyntaxTokenKind::Eoi
    ) {
        let token_count = tokens.len();
        let expression = parse_expression(tokens, diagnostic_bag);
        children.push(expression);
        if !matches!(peek_token(tokens).kind, SyntaxTokenKind::RBracket) {
            let comma_token = match_token!(tokens, diagnostic_bag, Comma);
            comma_tokens.push(comma_token);
        }
        if token_count == tokens.len() {
            next_token(tokens);
        }
    }
    let rbracket = match_token!(tokens, diagnostic_bag, RBracket);
    if children.is_empty() {
        let span = TextSpan::bounds(lbracket.span(), rbracket.span());
        diagnostic_bag.report_not_supported(
            span,
            "Empty Array literals (`[]`) are not supported currently.",
        );
    }
    SyntaxNode::array_literal(lbracket, children, comma_tokens, rbracket)
}

fn parse_number_literal<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    SyntaxNode::literal(match_token!(tokens, diagnostic_bag, NumberLiteral), diagnostic_bag)
}

fn parse_string_literal<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    SyntaxNode::literal(match_token!(tokens, diagnostic_bag, StringLiteral), diagnostic_bag)
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

fn parse_none_literal<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let token = match_token!(tokens, diagnostic_bag, NoneKeyword);
    SyntaxNode::none_literal(token)
}

fn parse_cast_expression<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let cast_keyword = match_token!(tokens, diagnostic_bag, CastKeyword);
    let expression = parse_expression(tokens, diagnostic_bag);
    let colon_token = match_token!(tokens, diagnostic_bag, Colon);
    let type_ = parse_type(tokens, diagnostic_bag);

    SyntaxNode::cast_expression(cast_keyword, expression, colon_token, type_)
}

fn parse_constructor<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    let new_keyword = match_token!(tokens, diagnostic_bag, NewKeyword);
    let type_name = match_token!(tokens, diagnostic_bag, Identifier);

    // FIXME: Copy-Paste from function call.
    let open_parenthesis_token = next_token(tokens);
    let mut arguments = vec![];
    let mut comma_tokens = vec![];
    while !matches!(
        peek_token(tokens).kind,
        // Currently there are no statements allowed as arguments to a
        // function. This may change..
        SyntaxTokenKind::RParen | SyntaxTokenKind::Eoi | SyntaxTokenKind::Semicolon
    ) {
        let token_count = tokens.len();
        arguments.push(parse_expression(tokens, diagnostic_bag));
        if !matches!(peek_token(tokens).kind, SyntaxTokenKind::RParen) {
            comma_tokens.push(match_token!(tokens, diagnostic_bag, Comma));
        }
        if token_count == tokens.len() {
            next_token(tokens);
        }
    }
    let close_parenthesis_token = match_token!(tokens, diagnostic_bag, RParen);

    SyntaxNode::constructor_call(
        new_keyword,
        type_name,
        open_parenthesis_token,
        arguments,
        comma_tokens,
        close_parenthesis_token,
    )
}

fn parse_identifier<'a>(
    tokens: &mut VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
) -> SyntaxNode<'a> {
    SyntaxNode::variable(match_token!(tokens, diagnostic_bag, Identifier))
}
