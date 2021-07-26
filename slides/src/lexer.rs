pub mod syntax_token;
#[cfg(test)]
mod tests;

use std::collections::VecDeque;

use crate::{
    diagnostics::DiagnosticBag, lexer::syntax_token::SyntaxToken, text::SourceText, DebugFlags,
};

use self::syntax_token::SyntaxTokenKind;

#[derive(Clone, Copy)]
enum State {
    Default,
    DecimalNumber(TextIndex),
    Identifier(TextIndex),
    MultiCharOperator(TextIndex),
}

#[derive(Clone, Copy)]
struct TextIndex {
    byte_index: usize,
    char_index: usize,
}

pub fn lex<'a>(
    content: &'a SourceText<'a>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
    debug_flags: DebugFlags,
) -> VecDeque<SyntaxToken<'a>> {
    let mut result = VecDeque::new();
    let mut state = State::Default;
    let text = content.text;
    for (char_index, (byte_index, character)) in text.char_indices().enumerate() {
        let index = TextIndex {
            byte_index,
            char_index,
        };
        'start: loop {
            match (state, character) {
                (State::Default, d) if d.is_digit(10) => {
                    state = State::DecimalNumber(index);
                }
                (State::Default, c) if c.is_alphabetic() || c == '_' => {
                    state = State::Identifier(index);
                }
                (State::Default, ws) if ws.is_whitespace() => {}
                (State::Default, o) if is_operator(o) => {
                    result.push_back(SyntaxToken::operator(byte_index, &text[byte_index..][..1]));
                }
                (State::Default, o) if is_multi_char_operator(o) => {
                    state = State::MultiCharOperator(index);
                }
                (State::DecimalNumber(start), d) => {
                    if !d.is_digit(10) {
                        state = State::Default;
                        result.push_back(SyntaxToken::number_literal(
                            start.char_index,
                            &text[start.byte_index..byte_index],
                            diagnostic_bag,
                        ));
                        continue 'start;
                    }
                }
                (State::Identifier(start), c) => {
                    if !c.is_alphanumeric() && c != '_' {
                        state = State::Default;
                        let lexeme = &text[start.byte_index..byte_index];
                        result.push_back(if SyntaxTokenKind::keyword(lexeme).is_some() {
                            SyntaxToken::keyword(start.char_index, lexeme)
                        } else {
                            SyntaxToken::identifier(start.char_index, lexeme)
                        });
                        continue 'start;
                    }
                }
                (State::MultiCharOperator(start), o) => {
                    if !is_multi_char_operator(o) || char_index - start.char_index > 2 {
                        state = State::Default;
                        let lexeme = &text[start.byte_index..byte_index];
                        if is_valid_operator(lexeme) {
                            result.push_back(SyntaxToken::operator(start.char_index, lexeme));
                        } else {
                            diagnostic_bag.report_bad_input(
                                start.char_index,
                                text.as_bytes()[start.byte_index] as _,
                            )
                        }
                        continue 'start;
                    }
                }
                (_, bad_character) => {
                    diagnostic_bag.report_bad_input(char_index, bad_character);
                }
            }
            break;
        }
    }
    match state {
        State::Default => {}
        State::DecimalNumber(start) => {
            result.push_back(SyntaxToken::number_literal(
                start.char_index,
                &text[start.byte_index..],
                diagnostic_bag,
            ));
        }
        State::Identifier(start) => {
            let lexeme = &text[start.byte_index..];
            result.push_back(if SyntaxTokenKind::keyword(lexeme).is_some() {
                SyntaxToken::keyword(start.char_index, lexeme)
            } else {
                SyntaxToken::identifier(start.char_index, lexeme)
            });
        }
        State::MultiCharOperator(start) => {
            let lexeme = &text[start.byte_index..];
            if !is_valid_operator(lexeme) {
                diagnostic_bag
                    .report_bad_input(start.char_index, text.as_bytes()[start.byte_index] as _)
            } else {
                result.push_back(SyntaxToken::operator(start.char_index, lexeme));
            }
        }
    }
    result.push_back(SyntaxToken::eoi(text.len()));
    if debug_flags.print_tokens() {
        for token in &result {
            println!("{:?}", token);
        }
    }
    result
}

fn is_operator(character: char) -> bool {
    matches!(
        character,
        '+' | '-' | '*' | '/' | '(' | ')' | ';' | '{' | '}' | ','
    )
}

fn is_multi_char_operator(character: char) -> bool {
    matches!(character, '=' | '!' | '<' | '>')
}

fn is_valid_operator(operator: &str) -> bool {
    matches!(operator, "!=" | "==" | "=" | "<=" | ">=" | "<" | ">")
}
