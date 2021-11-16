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

struct Parser<'a, 'b> {
    tokens: VecDeque<SyntaxToken<'a>>,
    diagnostic_bag: &'b mut DiagnosticBag<'a>,
}

impl<'a> Parser<'a, '_> {
    fn token_count(&self) -> usize {
        self.tokens.len()
    }

    fn next_token(&mut self) -> SyntaxToken<'a> {
        if self.tokens.is_empty() {
            panic!("No tokens could be found!")
        } else if self.tokens.len() == 1 {
            self.peek_token().clone()
        } else {
            self.tokens.pop_front().unwrap()
        }
    }

    fn peek_token(&self) -> &SyntaxToken<'a> {
        if self.tokens.is_empty() {
            panic!("No tokens could be found!")
        } else {
            self.tokens.front().unwrap()
        }
    }

    fn match_token(&mut self, kind: SyntaxTokenKind) -> SyntaxToken<'a> {
        let current_token_kind = self.peek_token().kind;
        let current_token_span = self.peek_token().span();
        let current_token_start = self.peek_token().start;
        if current_token_kind == kind {
            self.next_token()
        } else {
            self.diagnostic_bag.report_unexpected_token_kind(
                current_token_span,
                current_token_kind,
                kind,
            );
            SyntaxToken::error(current_token_start, kind)
        }
    }
}

pub fn parse<'a>(
    source_text: &'a SourceText<'a>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
    debug_flags: DebugFlags,
) -> SyntaxNode<'a> {
    let tokens = lexer::lex(source_text, diagnostic_bag, debug_flags);
    if diagnostic_bag.has_errors() {
        return SyntaxNode::error(0);
    }
    let mut parser = Parser {
        tokens,
        diagnostic_bag,
    };
    parse_compilation_unit(&mut parser)
}

fn parse_compilation_unit<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    let mut statements = vec![];
    while parser.peek_token().kind != SyntaxTokenKind::Eoi {
        let token_count = parser.token_count();
        statements.push(parse_top_level_statement(parser));
        if token_count == parser.token_count() {
            parser.next_token();
        }
    }
    let eoi = parser.match_token(SyntaxTokenKind::Eoi);
    SyntaxNode::compilation_unit(statements, eoi)
}

fn parse_top_level_statement<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    match parser.peek_token().kind {
        SyntaxTokenKind::ImportKeyword => parse_import_statement(parser),
        SyntaxTokenKind::FuncKeyword => parse_function_statement(parser),
        SyntaxTokenKind::StructKeyword => parse_struct_statement(parser),
        _ => {
            let token = parser.next_token();
            let span = token.span();
            parser.diagnostic_bag.report_unexpected_token_kind(
                span,
                token.kind,
                SyntaxTokenKind::FuncKeyword,
            );
            SyntaxNode::error(span.start())
        }
    }
}

fn parse_import_statement<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    let import_keyword = parser.match_token(SyntaxTokenKind::ImportKeyword);
    // This currently, and probably always, can only be a function call.
    let expression = parse_function_call(parser);
    let as_keyword = parser.match_token(SyntaxTokenKind::AsKeyword);
    let identifier = parser.match_token(SyntaxTokenKind::Identifier);
    let semicolon_token = parser.match_token(SyntaxTokenKind::Semicolon);

    SyntaxNode::import_statement(import_keyword, expression, as_keyword, identifier, semicolon_token)
}

fn parse_function_statement<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    let func_keyword = parser.match_token(SyntaxTokenKind::FuncKeyword);
    let identifier = parser.match_token(SyntaxTokenKind::Identifier);
    let function_type = parse_function_type(parser);
    let body = parse_block_statement(parser);

    SyntaxNode::function_declaration(func_keyword, identifier, function_type, body)
}

fn parse_struct_statement<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    let struct_keyword = parser.match_token(SyntaxTokenKind::StructKeyword);
    let identifier = parser.match_token(SyntaxTokenKind::Identifier);
    let body = parse_struct_body(parser);

    SyntaxNode::struct_declaration(struct_keyword, identifier, body)
}

fn parse_function_type<'a>(parser: &mut Parser<'a, '_>) -> FunctionTypeNode<'a> {
    let lparen = parser.match_token(SyntaxTokenKind::LParen);
    let mut parameters = vec![];
    let mut comma_tokens = vec![];
    while parser.peek_token().kind != SyntaxTokenKind::RParen {
        let token_count = parser.token_count();
        let parameter = parse_parameter(parser);
        parameters.push(parameter);
        if parser.peek_token().kind != SyntaxTokenKind::RParen {
            let comma = parser.match_token(SyntaxTokenKind::Comma);
            comma_tokens.push(comma);
        }
        if token_count == parser.token_count() {
            parser.next_token();
        }
    }
    let rparen = parser.match_token(SyntaxTokenKind::RParen);

    let return_type = if parser.peek_token().kind == SyntaxTokenKind::Arrow {
        let arrow_token = parser.next_token();
        let return_type = parse_type(parser);
        Some(ReturnTypeNode {
            arrow_token,
            return_type,
        })
    } else {
        None
    };

    FunctionTypeNode::new(lparen, parameters, comma_tokens, rparen, return_type)
}

fn parse_struct_body<'a>(parser: &mut Parser<'a, '_>) -> StructBodyNode<'a> {
    let lbrace = parser.match_token(SyntaxTokenKind::LBrace);
    let mut statements = vec![];
    while parser.peek_token().kind != SyntaxTokenKind::RBrace {
        let token_count = parser.token_count();
        let statement = match parser.peek_token().kind {
            SyntaxTokenKind::FuncKeyword => parse_function_statement(parser),
            SyntaxTokenKind::Identifier => {
                let field = parse_parameter(parser);
                let semicolon_token = parser.match_token(SyntaxTokenKind::Semicolon);
                SyntaxNode::struct_field(field, semicolon_token)
            }
            _ => {
                let current = parser.next_token();
                parser.diagnostic_bag.report_unexpected_token_kind(
                    current.span(),
                    current.kind,
                    SyntaxTokenKind::Identifier,
                );
                SyntaxNode::error(current.start)
            }
        };
        statements.push(statement);
        if token_count == parser.token_count() {
            parser.next_token();
        }
    }
    let rbrace = parser.match_token(SyntaxTokenKind::RBrace);

    StructBodyNode::new(lbrace, statements, rbrace)
}

fn parse_parameter<'a>(parser: &mut Parser<'a, '_>) -> ParameterNode<'a> {
    let identifier = parser.match_token(SyntaxTokenKind::Identifier);
    let colon_token = parser.match_token(SyntaxTokenKind::Colon);
    let type_ = parse_type(parser);
    ParameterNode::new(identifier, colon_token, type_)
}

fn parse_type<'a>(parser: &mut Parser<'a, '_>) -> TypeNode<'a> {
    let identifier = parser.match_token(SyntaxTokenKind::Identifier);
    let optional_question_mark = if parser.peek_token().kind == SyntaxTokenKind::QuestionMark {
        Some(parser.next_token())
    } else {
        None
    };
    let mut brackets = vec![];
    while parser.peek_token().kind == SyntaxTokenKind::LBracket {
        let token_count = parser.token_count();
        let lbracket = parser.match_token(SyntaxTokenKind::LBracket);
        let rbracket = parser.match_token(SyntaxTokenKind::RBracket);
        if token_count == parser.token_count() {
            parser.next_token();
        }
        brackets.push(SyntaxToken::bracket_pair(lbracket, rbracket));
    }

    TypeNode::new(identifier, optional_question_mark, brackets)
}

fn parse_statement<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    match parser.peek_token().kind {
        SyntaxTokenKind::LBrace => parse_block_statement(parser),
        SyntaxTokenKind::ForKeyword => parse_for_statement(parser),
        SyntaxTokenKind::IfKeyword => parse_if_statement(parser),
        SyntaxTokenKind::LetKeyword => parse_variable_declaration(parser),
        SyntaxTokenKind::ReturnKeyword => parse_return_statement(parser),
        SyntaxTokenKind::WhileKeyword => parse_while_statement(parser),
        _ => parse_assignment_statement(parser),
    }
}

fn parse_block_statement<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    let open_brace_token = parser.match_token(SyntaxTokenKind::LBrace);
    let mut statements = vec![];
    while !matches!(
        parser.peek_token().kind,
        SyntaxTokenKind::RBrace | SyntaxTokenKind::Eoi
    ) {
        let token_count = parser.token_count();
        statements.push(parse_statement(parser));
        if token_count == parser.token_count() {
            parser.next_token();
        }
    }
    let close_brace_token = parser.match_token(SyntaxTokenKind::RBrace);
    SyntaxNode::block_statement(open_brace_token, statements, close_brace_token)
}

fn parse_for_statement<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    let for_keyword = parser.match_token(SyntaxTokenKind::ForKeyword);
    let mut variable = parser.match_token(SyntaxTokenKind::Identifier);
    let optional_index_variable = if parser.peek_token().kind == SyntaxTokenKind::Comma {
        parser.next_token();
        let it = variable;
        variable = parser.match_token(SyntaxTokenKind::Identifier);
        Some(it)
    } else {
        None
    };
    let in_keyword = parser.match_token(SyntaxTokenKind::InKeyword);
    let collection = parse_expression(parser);
    let body = parse_block_statement(parser);

    SyntaxNode::for_statement(
        for_keyword,
        optional_index_variable,
        variable,
        in_keyword,
        collection,
        body,
    )
}

fn parse_if_statement<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    let if_keyword = parser.match_token(SyntaxTokenKind::IfKeyword);
    let condition = parse_expression(parser);
    let body = parse_block_statement(parser);
    let else_clause = if parser.peek_token().kind == SyntaxTokenKind::ElseKeyword {
        Some(parse_else_clause(parser))
    } else {
        None
    };
    SyntaxNode::if_statement(if_keyword, condition, body, else_clause)
}

fn parse_else_clause<'a>(parser: &mut Parser<'a, '_>) -> ElseClause<'a> {
    let else_keyword = parser.match_token(SyntaxTokenKind::ElseKeyword);
    let body = if parser.peek_token().kind == SyntaxTokenKind::IfKeyword {
        parse_if_statement(parser)
    } else {
        parse_block_statement(parser)
    };
    ElseClause::new(else_keyword, body)
}

fn parse_variable_declaration<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    let let_keyword = parser.match_token(SyntaxTokenKind::LetKeyword);
    let identifier = if parser.peek_token().kind.is_keyword() {
        let token = parser.next_token();
        parser
            .diagnostic_bag
            .report_cannot_declare_keyword_as_variable(token.span(), token.lexeme);
        SyntaxToken::identifier(token.span().start(), "")
    } else {
        parser.match_token(SyntaxTokenKind::Identifier)
    };
    let optional_type_declaration = if parser.peek_token().kind == SyntaxTokenKind::Colon {
        let colon_token = parser.next_token();
        let type_ = parse_type(parser);
        Some(TypeDeclaration { colon_token, type_ })
    } else {
        None
    };
    let equals_token = parser.match_token(SyntaxTokenKind::Equals);
    let initializer = parse_expression(parser);
    let semicolon_token = parser.match_token(SyntaxTokenKind::Semicolon);
    SyntaxNode::variable_declaration(
        let_keyword,
        identifier,
        optional_type_declaration,
        equals_token,
        initializer,
        semicolon_token,
    )
}

fn parse_return_statement<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    let return_keyword = parser.match_token(SyntaxTokenKind::ReturnKeyword);
    let optional_expression = if parser.peek_token().kind == SyntaxTokenKind::Semicolon {
        None
    } else {
        Some(parse_expression(parser))
    };
    let semicolon_token = parser.match_token(SyntaxTokenKind::Semicolon);

    SyntaxNode::return_statement(return_keyword, optional_expression, semicolon_token)
}

fn parse_while_statement<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    let while_keyword = parser.match_token(SyntaxTokenKind::WhileKeyword);
    let condition = parse_expression(parser);
    let body = parse_block_statement(parser);
    SyntaxNode::while_statement(while_keyword, condition, body)
}

fn parse_assignment_statement<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    let lhs = parse_binary_with_parent_precedence(parser, 1);
    if parser.peek_token().kind == SyntaxTokenKind::Equals {
        if !lhs.kind.is_assignable() && !lhs.is_inserted {
            parser.diagnostic_bag.report_cannot_assign_to(lhs.span);
        }
        parser.next_token();
        let expression = parse_expression(parser);
        let semicolon_token = parser.match_token(SyntaxTokenKind::Semicolon);
        SyntaxNode::assignment(lhs, expression, semicolon_token)
    } else {
        let semicolon_token = parser.match_token(SyntaxTokenKind::Semicolon);
        SyntaxNode::expression_statement(lhs, semicolon_token)
    }
}

fn parse_expression<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    parse_binary(parser)
}

fn parse_binary<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    parse_binary_with_parent_precedence(parser, 0)
}

fn parse_binary_with_parent_precedence<'a>(
    parser: &mut Parser<'a, '_>,
    parent_precedence: u32,
) -> SyntaxNode<'a> {
    let mut lhs;
    let unary_precedence = parser.peek_token().kind.unary_precedence();
    if unary_precedence != 0 && unary_precedence >= parent_precedence {
        let operator_token = parser.next_token();
        let operand = parse_binary_with_parent_precedence(parser, unary_precedence);
        lhs = SyntaxNode::unary(operator_token, operand);
    } else {
        lhs = parse_function_call(parser);
    }

    loop {
        let precedence = parser.peek_token().kind.binary_precedence();
        if precedence == 0 || precedence <= parent_precedence {
            break;
        }

        let operator_token = parser.next_token();
        if operator_token.kind == SyntaxTokenKind::Equals {
            parser.diagnostic_bag.report_cannot_assign_to(lhs.span);
        }
        let rhs = parse_binary_with_parent_precedence(parser, precedence);
        lhs = SyntaxNode::binary(lhs, operator_token, rhs);
    }
    lhs
}

fn parse_function_call<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    let mut base = parse_primary(parser);
    loop {
        match parser.peek_token().kind {
            SyntaxTokenKind::LParen => {
                let open_parenthesis_token = parser.next_token();
                let mut arguments = vec![];
                let mut comma_tokens = vec![];
                while !matches!(
                    parser.peek_token().kind,
                    // Currently there are no statements allowed as arguments to a
                    // function. This may change..
                    SyntaxTokenKind::RParen | SyntaxTokenKind::Eoi | SyntaxTokenKind::Semicolon
                ) {
                    let token_count = parser.token_count();
                    arguments.push(parse_expression(parser));
                    if parser.peek_token().kind != SyntaxTokenKind::RParen {
                        comma_tokens.push(parser.match_token(SyntaxTokenKind::Comma));
                    }
                    if token_count == parser.token_count() {
                        parser.next_token();
                    }
                }
                let close_parenthesis_token = parser.match_token(SyntaxTokenKind::RParen);
                base = SyntaxNode::function_call(
                    base,
                    open_parenthesis_token,
                    arguments,
                    comma_tokens,
                    close_parenthesis_token,
                );
            }
            SyntaxTokenKind::LBracket => {
                let lbracket = parser.next_token();
                let index = parse_expression(parser);
                let rbracket = parser.match_token(SyntaxTokenKind::RBracket);
                base = SyntaxNode::array_index(base, lbracket, index, rbracket);
            }
            SyntaxTokenKind::Period => {
                let period = parser.next_token();
                let identifier = parser.match_token(SyntaxTokenKind::Identifier);
                base = SyntaxNode::field_access(base, period, identifier);
            }
            _ => break,
        }
    }
    base
}

fn parse_primary<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    let span = parser.peek_token().span();
    match parser.peek_token().kind {
        SyntaxTokenKind::LParen => {
            let lparen = parser.next_token();
            let expression = parse_expression(parser);
            let rparen = parser.match_token(SyntaxTokenKind::RParen);
            SyntaxNode::parenthesized(lparen, expression, rparen)
        }
        SyntaxTokenKind::LBracket => parse_array_literal(parser),
        SyntaxTokenKind::NumberLiteral => parse_number_literal(parser),
        SyntaxTokenKind::StringLiteral => parse_string_literal(parser),
        SyntaxTokenKind::TrueKeyword | SyntaxTokenKind::FalseKeyword => {
            parse_boolean_literal(parser)
        }
        SyntaxTokenKind::NoneKeyword => parse_none_literal(parser),
        SyntaxTokenKind::CastKeyword => parse_cast_expression(parser),
        SyntaxTokenKind::NewKeyword => parse_constructor(parser),
        SyntaxTokenKind::Identifier => parse_identifier(parser),
        unexpected_token => {
            parser.diagnostic_bag.report_unexpected_token_kind(
                span,
                unexpected_token,
                SyntaxTokenKind::NumberLiteral,
            );
            // parser.next_token();
            SyntaxNode::error(span.start())
        }
    }
}

fn parse_array_literal<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    let lbracket = parser.match_token(SyntaxTokenKind::LBracket);
    let mut children = vec![];
    let mut comma_tokens = vec![];
    while !matches!(
        parser.peek_token().kind,
        SyntaxTokenKind::RBracket | SyntaxTokenKind::Eoi
    ) {
        let token_count = parser.token_count();
        let mut expression = parse_expression(parser);
        if parser.peek_token().kind == SyntaxTokenKind::Semicolon {
            let semicolon_token = parser.match_token(SyntaxTokenKind::Semicolon);
            let repetition = parse_expression(parser);
            expression = SyntaxNode::repetition_node(expression, semicolon_token, repetition);
        }
        children.push(expression);
        if parser.peek_token().kind != SyntaxTokenKind::RBracket {
            let comma_token = parser.match_token(SyntaxTokenKind::Comma);
            comma_tokens.push(comma_token);
        }
        if token_count == parser.token_count() {
            parser.next_token();
        }
    }
    let rbracket = parser.match_token(SyntaxTokenKind::RBracket);
    if children.is_empty() {
        let span = TextSpan::bounds(lbracket.span(), rbracket.span());
        parser.diagnostic_bag.report_not_supported(
            span,
            "Empty Array literals (`[]`) are not supported currently.",
        );
    }
    SyntaxNode::array_literal(lbracket, children, comma_tokens, rbracket)
}

fn parse_number_literal<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    SyntaxNode::literal(
        parser.match_token(SyntaxTokenKind::NumberLiteral),
        parser.diagnostic_bag,
    )
}

fn parse_string_literal<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    SyntaxNode::literal(
        parser.match_token(SyntaxTokenKind::StringLiteral),
        parser.diagnostic_bag,
    )
}

fn parse_boolean_literal<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    let token = parser.next_token();
    match token.kind {
        SyntaxTokenKind::TrueKeyword => SyntaxNode::true_literal(token),
        SyntaxTokenKind::FalseKeyword => SyntaxNode::false_literal(token),
        _ => unreachable!(),
    }
}

fn parse_none_literal<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    let token = parser.match_token(SyntaxTokenKind::NoneKeyword);
    SyntaxNode::none_literal(token)
}

fn parse_cast_expression<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    let cast_keyword = parser.match_token(SyntaxTokenKind::CastKeyword);
    let expression = parse_expression(parser);
    let colon_token = parser.match_token(SyntaxTokenKind::Colon);
    let type_ = parse_type(parser);

    SyntaxNode::cast_expression(cast_keyword, expression, colon_token, type_)
}

fn parse_constructor<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    let new_keyword = parser.match_token(SyntaxTokenKind::NewKeyword);
    let type_name = parser.match_token(SyntaxTokenKind::Identifier);
    let (library_name, type_name) = if parser.peek_token().kind == SyntaxTokenKind::Period {
        // FIXME: It would be nice to keep the period token around.
        parser.next_token();
        (Some(type_name), parser.match_token(SyntaxTokenKind::Identifier))
    } else {
        (None, type_name)
    };
    // FIXME: Copy-Paste from function call.
    let open_parenthesis_token = parser.next_token();
    let mut arguments = vec![];
    let mut comma_tokens = vec![];
    while !matches!(
        parser.peek_token().kind,
        // Currently there are no statements allowed as arguments to a
        // function. This may change..
        SyntaxTokenKind::RParen | SyntaxTokenKind::Eoi | SyntaxTokenKind::Semicolon
    ) {
        let token_count = parser.token_count();
        arguments.push(parse_expression(parser));
        if parser.peek_token().kind != SyntaxTokenKind::RParen {
            comma_tokens.push(parser.match_token(SyntaxTokenKind::Comma));
        }
        if token_count == parser.token_count() {
            parser.next_token();
        }
    }
    let close_parenthesis_token = parser.match_token(SyntaxTokenKind::RParen);

    SyntaxNode::constructor_call(
        new_keyword,
        library_name,
        type_name,
        open_parenthesis_token,
        arguments,
        comma_tokens,
        close_parenthesis_token,
    )
}

fn parse_identifier<'a>(parser: &mut Parser<'a, '_>) -> SyntaxNode<'a> {
    SyntaxNode::variable(parser.match_token(SyntaxTokenKind::Identifier))
}
