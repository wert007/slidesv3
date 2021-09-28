use crate::{
    binder::typing::Type,
    lexer::syntax_token::SyntaxTokenKind,
    parser::syntax_nodes::SyntaxNodeKind,
    text::{SourceText, TextLocation, TextSpan},
};

#[derive(Debug, Clone)]
pub struct Diagnostic<'a> {
    message: String,
    location: TextLocation<'a>,
}

impl<'a> Diagnostic<'a> {
    pub fn runtime(
        message: String,
        span: Option<TextSpan>,
        source_text: &'a SourceText<'a>,
    ) -> Self {
        Self {
            message,
            location: TextLocation {
                span: span.unwrap_or(TextSpan::zero()),
                source_text,
            },
        }
    }
}

impl std::fmt::Display for Diagnostic<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error {}: {}", self.location, self.message)
    }
}

#[derive(Clone)]
pub struct DiagnosticBag<'a> {
    pub diagnostics: Vec<Diagnostic<'a>>,
    pub source_text: &'a SourceText<'a>,
}

impl<'a> DiagnosticBag<'a> {
    pub fn new(source_text: &'a SourceText<'a>) -> Self {
        Self {
            diagnostics: vec![],
            source_text,
        }
    }

    pub fn has_errors(&self) -> bool {
        !self.diagnostics.is_empty()
    }

    pub fn flush_to_console(self) {
        for diagnostic in self.diagnostics {
            println!("{}", diagnostic);
        }
    }

    fn report(&mut self, message: String, span: TextSpan) {
        let diagnostic = Diagnostic {
            message,
            location: TextLocation {
                span,
                source_text: self.source_text,
            },
        };
        self.diagnostics.push(diagnostic);
    }

    fn report_runtime(&mut self, message: String, span: Option<TextSpan>) {
        let diagnostic = Diagnostic::runtime(message, span, self.source_text);
        self.diagnostics.push(diagnostic);
    }

    pub fn report_bad_input(&mut self, start: usize, character: char) {
        let message = format!("Bad character in input: {}", character);
        let span = TextSpan::new(start, 1);
        self.report(message, span)
    }
    pub fn report_bad_integer(&mut self, start: usize, text: &str) {
        let message = format!("'{}' is no valid u64 integer.", text);
        let span = TextSpan::new(start, text.len());
        self.report(message, span)
    }

    pub fn report_unexpected_token_kind(
        &mut self,
        span: TextSpan,
        actual_token_kind: &SyntaxTokenKind,
        expected_token_kind: &SyntaxTokenKind,
    ) {
        let message = format!(
            "Expected token kind {:?} but actually found {:?}.",
            expected_token_kind, actual_token_kind
        );
        self.report(message, span)
    }

    pub fn report_cannot_convert(&mut self, span: TextSpan, from_type: &Type, to_type: &Type) {
        let message = format!("Cannot convert type {} to type {}.", from_type, to_type);
        self.report(message, span)
    }

    pub fn report_invalid_void_expression(&mut self, span: TextSpan) {
        let message = "Expected value type, but expression is of type void.".into();
        self.report(message, span)
    }

    pub fn report_no_unary_operator(&mut self, span: TextSpan, operator: &str, type_: &Type) {
        let message = format!("No unary operator {} for type {}.", operator, type_);
        self.report(message, span);
    }

    pub fn report_no_binary_operator(
        &mut self,
        span: TextSpan,
        lhs_type: &Type,
        operator: &str,
        rhs_type: &Type,
    ) {
        let message = format!(
            "No binary operator {} for types {} and {}.",
            operator, lhs_type, rhs_type
        );
        self.report(message, span);
    }

    #[allow(dead_code)]
    pub fn report_not_supported(&mut self, span: TextSpan, unsupported: &str) {
        let message = format!("Currently {} are not supported!", unsupported);
        self.report(message, span);
    }

    pub fn report_cannot_declare_variable(&mut self, span: TextSpan, variable_name: &str) {
        let message = format!(
            "A variable named '{}' cannot be declared here.",
            variable_name
        );
        self.report(message, span);
    }

    pub fn report_cannot_declare_keyword_as_variable(
        &mut self,
        span: TextSpan,
        variable_name: &str,
    ) {
        let message = format!(
            "'{}' is a keyword and cannot be overwritten.",
            variable_name
        );
        self.report(message, span);
    }

    pub fn report_variable_not_found(&mut self, span: TextSpan, variable_name: &str) {
        let message = format!("No variable named '{}' could be found.", variable_name);
        self.report(message, span);
    }

    pub fn report_cannot_assign_to(&mut self, span: TextSpan) {
        let message = "Only variables can be assigned to. Did you mean to use ==?".into();
        self.report(message, span);
    }

    pub fn report_variable_is_read_only(&mut self, span: TextSpan) {
        let message = "Variable cannot be assigned to, it is a read only variable.".into();
        self.report(message, span);
    }

    pub fn report_unexpected_argument_count(
        &mut self,
        span: TextSpan,
        actual_argument_count: usize,
        expected_argument_count: usize,
    ) {
        let message = format!(
            "Expected {} arguments, but actually found {}.",
            expected_argument_count, actual_argument_count
        );
        self.report(message, span)
    }

    pub fn report_unterminated_comment(&mut self, start: usize, text: &str) {
        let span = TextSpan::new(start, text.len());
        let message = "Unterminated comment found here.".into();
        self.report(message, span);
    }

    pub fn report_unterminated_string(&mut self, span: TextSpan, expected_char: char) {
        let message = format!(
            "Unterminated string found here. Expected `{}`.",
            expected_char
        );
        self.report(message, span);
    }

    pub fn report_no_fields_on_type(&mut self, span: TextSpan, type_: &Type) {
        let message = format!("There are no fields on type {}.", type_);
        self.report(message, span);
    }

    pub fn report_no_field_named_on_type(
        &mut self,
        span: TextSpan,
        field_name: &str,
        type_: &Type,
    ) {
        let message = format!(
            "There are no fields named '{}' on type {}.",
            field_name, type_
        );
        self.report(message, span);
    }

    pub fn report_invalid_top_level_statement(&mut self, span: TextSpan, kind: SyntaxNodeKind) {
        let message = format!(
            "{:?} is no valid top level statement. Use functions instead.",
            kind
        );
        self.report(message, span);
    }

    pub fn report_unknown_type(&mut self, span: TextSpan, type_name: &str) {
        let message = format!("Unknown type {} found.", type_name);
        self.report(message, span);
    }

    pub fn report_missing_return_value(&mut self, span: TextSpan, expected_return_type: &Type) {
        let message = format!(
            "Function returns type {} and needs a value of type {}.",
            expected_return_type, expected_return_type
        );
        self.report(message, span);
    }

    pub fn report_missing_return_statement(&mut self, span: TextSpan, expected_return_type: &Type) {
        let message = format!(
            "Not all paths in function return. Every path needs a return value of type {}",
            expected_return_type
        );
        self.report(message, span);
    }

    // Runtime Errors
    pub fn index_out_of_bounds(&mut self, span: Option<TextSpan>, index: i64, length: u64) {
        let message = format!(
            "Index out of Bounds. Index was {} and length was {}.",
            index, length
        );
        self.report_runtime(message, span);
    }

    pub fn no_heap_memory_left(&mut self, span: Option<TextSpan>, needed_memory_in_bytes: u64) {
        let message = format!("Out of memory. No heap memory for {} bytes left.", needed_memory_in_bytes);
        self.report_runtime(message, span);
    }
}
