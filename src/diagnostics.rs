use crate::{binder::typing::Type, lexer::syntax_token::SyntaxTokenKind, text::TextSpan};

#[derive(Debug)]
pub struct Diagnostic {
    message: String,
    span: TextSpan,
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Error at {}-{}: {}",
            self.span.start(),
            self.span.end(),
            self.message
        )
    }
}

pub struct DiagnosticBag {
    pub diagnostics: Vec<Diagnostic>,
}

impl DiagnosticBag {
    pub fn new() -> Self {
        Self {
            diagnostics: vec![],
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

    fn report(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn report_bad_input(&mut self, start: usize, character: char) {
        let message = format!("Bad character in input: {}", character);
        let span = TextSpan::new(start, 1);
        self.report(Diagnostic { message, span })
    }
    pub fn report_bad_integer(&mut self, start: usize, text: &str) {
        let message = format!("'{}' is no valid u64 integer.", text);
        let span = TextSpan::new(start, text.len());
        self.report(Diagnostic { message, span })
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
        self.report(Diagnostic { message, span })
    }

    #[allow(dead_code)]
    pub fn report_cannot_convert(&mut self, span: TextSpan, from_type: &Type, to_type: &Type) {
        let message = format!("Cannot convert type {} to type {}.", from_type, to_type);
        self.report(Diagnostic { message, span })
    }

    pub fn report_no_unary_operator(&mut self, span: TextSpan, operator: &str, type_: &Type) {
        let message = format!("No unary operator {} for type {}.", operator, type_);
        self.report(Diagnostic { message, span });
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
        self.report(Diagnostic { message, span });
    }

    #[allow(dead_code)]
    pub fn report_not_supported(&mut self, span: TextSpan, unsupported: &str) {
        let message = format!("Currently {} are not supported!", unsupported);
        self.report(Diagnostic { message, span });
    }

    pub fn report_cannot_declare_variable(&mut self, span: TextSpan, variable_name: &str) {
        let message = format!(
            "A variable named '{}' cannot be declared here.",
            variable_name
        );
        self.report(Diagnostic { message, span });
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
        self.report(Diagnostic { message, span });
    }

    pub fn report_variable_not_found(&mut self, span: TextSpan, variable_name: &str) {
        let message = format!("No variable named '{}' could be found.", variable_name);
        self.report(Diagnostic { message, span });
    }

    pub fn report_cannot_assign_to(&mut self, span: TextSpan) {
        let message = "Only variables can be assigned to. Did you mean to use ==?".into();
        self.report(Diagnostic { message, span });
    }
}
