pub mod syntax_token;
#[cfg(test)]
mod tests;

use std::collections::VecDeque;

use crate::{
    diagnostics::DiagnosticBag,
    lexer::syntax_token::SyntaxToken,
    text::{SourceText, TextSpan},
    DebugFlags,
};

use self::syntax_token::SyntaxTokenKind;

#[derive(Clone, Copy)]
enum State {
    /// Default state, this means that for the current token, there are no
    /// characters consumed and any token may follow.
    Default,
    /// This is a simple decimal integer number right now. It checks, that the
    /// value fits into an i64 and only consumes chars 0..9
    DecimalNumber(TextIndex),
    /// This is a simple string starting with ' and ending with the same
    /// character. StrintState stores the character which started the string and
    /// may in the future hold flags like formatted strings, math strings and
    /// similiar things.
    String(TextIndex, StringState),
    /// An Identifier is a single word, it might contain ascii letters, numbers
    /// and underscores, it cannot start with a number. If it starts with an
    /// dollar sign, this means its representing something which is normally not
    /// accessible on user level.
    Identifier(TextIndex),
    /// These are operators wich exist as a single char operator and as a multi
    /// char operator, but it might make sense to repeat them. Like unary
    /// operators e.g. So "!!" gets parsed as two tokens and "!=" as one token.
    /// char is the expected next character of the mutli char operator.
    MaybeTwoCharOperator(TextIndex, char),
    /// This is a operator which may be multi char but actually does not have to
    /// be. It simply consumes as long as there are operator characters in the
    /// input. It currently also has a max length of 2 characters, since there
    /// are no operators, which are longer.
    MultiCharOperator(TextIndex),
    /// This is a single dash, which might be an operator, a single line comment
    /// or a multiline comment.
    MaybeComment(TextIndex),
    /// A single line comment starts with // and ends in a new line. Anything in
    /// between is ignored.
    SingleLineComment(TextIndex),
    /// A multi line comment starts with /* and ends with */. Anything in
    /// between is ignored, it allows for nested multi line comments.
    MultiLineComment(TextIndex),
    /// This is a single * inside a multi line comment. If the next char is a /
    /// the current multi line comment will be closed. Other wise the state will
    /// be set to multi line comment again.
    MaybeCloseMultiLineComment(TextIndex),
}

#[derive(Clone, Copy)]
struct TextIndex {
    byte_index: usize,
    char_index: usize,
}

#[derive(Clone, Copy)]
struct StringState {
    enclosing_char: char,
}

impl StringState {
    pub fn single_quote() -> Self {
        Self {
            enclosing_char: '\'',
        }
    }
}

pub fn lex<'a>(
    content: &'a SourceText<'a>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
    debug_flags: DebugFlags,
) -> VecDeque<SyntaxToken<'a>> {
    let mut result = VecDeque::new();
    let mut state = State::Default;
    let mut nested_comments_depth = 0;
    let mut char_len = 0;
    let text = content.text;
    for (char_index, (byte_index, character)) in text.char_indices().enumerate() {
        let index = TextIndex {
            byte_index,
            char_index,
        };
        char_len = char_index;
        'start: loop {
            match (state, character) {
                (State::Default, '/') => {
                    state = State::MaybeComment(index);
                }
                (State::Default, d) if d.is_digit(10) => {
                    state = State::DecimalNumber(index);
                }
                (State::Default, '\'') => {
                    state = State::String(index, StringState::single_quote());
                }
                (State::Default, c) if c.is_alphabetic() || c == '_' || c == '$' => {
                    state = State::Identifier(index);
                }
                (State::Default, ws) if ws.is_whitespace() => {}
                (State::Default, o) if is_operator(o) => {
                    result.push_back(SyntaxToken::operator(char_index, &text[byte_index..][..1]));
                }
                (State::Default, '!') => {
                    state = State::MaybeTwoCharOperator(index, '=');
                }
                (State::Default, o) if is_multi_char_operator(o) => {
                    state = State::MultiCharOperator(index);
                }
                (State::MaybeComment(start), '/') if nested_comments_depth == 0 => {
                    state = State::SingleLineComment(start);
                }
                (State::MaybeComment(start), '*') => {
                    state = State::MultiLineComment(start);
                    nested_comments_depth += 1;
                }
                (State::MaybeComment(start), _) if nested_comments_depth > 0 => {
                    state = State::MultiLineComment(start);
                }
                (State::MaybeComment(start), _) => {
                    result.push_back(SyntaxToken::operator(
                        start.char_index,
                        &text[start.byte_index..][..1],
                    ));
                    state = State::Default;
                    continue 'start;
                }
                (State::SingleLineComment(_start), '\n') => {
                    // Here you would emit a token.
                    state = State::Default;
                }
                (State::MultiLineComment(start), '*') => {
                    state = State::MaybeCloseMultiLineComment(start);
                }
                (State::MultiLineComment(start), '/') => {
                    state = State::MaybeComment(start);
                }
                (State::MaybeCloseMultiLineComment(start), '/') => {
                    // Here you would emit a token.
                    nested_comments_depth -= 1;
                    if nested_comments_depth == 0 {
                        state = State::Default;
                    } else {
                        state = State::MultiLineComment(start);
                    }
                }
                (State::MaybeCloseMultiLineComment(start), _) => {
                    state = State::MultiLineComment(start);
                }
                (State::DecimalNumber(start), d) => {
                    if !d.is_digit(10) {
                        state = State::Default;
                        result.push_back(SyntaxToken::number_literal(
                            start.char_index,
                            &text[start.byte_index..byte_index],
                        ));
                        continue 'start;
                    }
                }
                (State::String(start, string_state), c) => {
                    if c == '\n' || c == string_state.enclosing_char {
                        state = State::Default;
                        if c == '\n' {
                            let span = TextSpan::new(
                                start.char_index,
                                char_index - start.char_index,
                                false,
                            );
                            diagnostic_bag
                                .report_unterminated_string(span, string_state.enclosing_char);
                        }
                        result.push_back(SyntaxToken::string_literal(
                            start.char_index,
                            &text[start.byte_index..byte_index + 1],
                        ));
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
                (State::MaybeTwoCharOperator(start, expected), actual) => {
                    state = State::Default;
                    if expected == actual {
                        state = State::MultiCharOperator(start);
                    } else {
                        let lexeme = &text[start.byte_index..byte_index];
                        result.push_back(SyntaxToken::operator(start.char_index, lexeme));
                        continue 'start;
                    }
                }
                (State::MultiCharOperator(start), o) => {
                    if !is_multi_char_operator(o) || char_index - start.char_index >= 2 {
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
                (State::SingleLineComment(_) | State::MultiLineComment(_), _) => {}
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
            ));
        }
        State::String(start, string_state) => {
            let span = TextSpan::new(start.char_index, char_len - start.char_index, false);
            diagnostic_bag.report_unterminated_string(span, string_state.enclosing_char);
            result.push_back(SyntaxToken::string_literal(
                start.char_index,
                &text[start.byte_index..],
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
        State::MaybeTwoCharOperator(start, _) => {
            let lexeme = &text[start.byte_index..];
            result.push_back(SyntaxToken::operator(start.char_index, lexeme));
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
        State::MaybeComment(start) => result.push_back(SyntaxToken::operator(
            start.char_index,
            &text[start.byte_index..],
        )),
        State::SingleLineComment(_start) => {
            // Here you would emit a token.
        }
        State::MaybeCloseMultiLineComment(start) | State::MultiLineComment(start) => {
            diagnostic_bag.report_unterminated_comment(start.char_index, &text[start.byte_index..])
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
        '+' | '*' | '(' | ')' | ';' | ':' | '{' | '}' | ',' | '[' | ']'
    )
}

fn is_multi_char_operator(character: char) -> bool {
    matches!(character, '=' | '<' | '>' | '-' | '?' | '.' | '&' | '|')
}

fn is_valid_operator(operator: &str) -> bool {
    matches!(
        operator,
        "!=" | "=="
            | "="
            | "<="
            | ">="
            | "<"
            | ">"
            | "->"
            | "-"
            | "?"
            | "??"
            | "."
            | ".."
            | "&"
            | "&&"
            | "||"
    )
}
