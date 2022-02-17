use std::borrow::Cow;

use crate::{
    binder::{typing::Type, BoundStructSymbol},
    lexer::syntax_token::SyntaxTokenKind,
    parser::syntax_nodes::SyntaxNodeKind,
    text::{SourceText, TextLocation, TextSpan},
};

#[derive(Debug, Clone)]
enum Message<'a> {
    String(Cow<'a, str>),
    Type(Type),
    Composition(Vec<Message<'a>>),
}

impl Message<'_> {
    pub fn to_string_with_struct_table(self, struct_table: &[String]) -> String {
        match self {
            Message::String(it) => it.into(),
            Message::Type(type_) => type_to_name(struct_table, type_),
            Message::Composition(composition) => composition.into_iter().map(|m|m.to_string_with_struct_table(struct_table)).collect(),
        }
    }
}

fn type_to_name(struct_table: &[String], type_: Type) -> String {
    match type_ {
        Type::Struct(struct_type) => struct_table[struct_type.id as usize].clone(),
        Type::StructReference(id) => struct_table[id as usize].clone(),
        Type::Noneable(base_type) => format!("{}?", type_to_name(struct_table, *base_type)),
        _ => type_.to_string(),
    }
}

impl From<String> for Message<'_> {
    fn from(it: String) -> Self {
        Self::String(it.into())
    }
}

impl<'a> From<&'a str> for Message<'a> {
    fn from(it: &'a str) -> Self {
        Self::String(it.into())
    }
}

impl From<&Type> for Message<'_> {
    fn from(it: &Type) -> Self {
        Self::Type(it.clone())
    }
}

impl<'a> From<&[Message<'a>]> for Message<'a> {
    fn from(it: &[Message<'a>]) -> Self {
        Self::Composition(it.into())
    }
}

macro_rules! message_format {
    ($($entry:expr), * $(,)?) => {
        {
            let message : &[Message] = &[
                $($entry.into(), )*
            ];
            message.into()
        }
    };
}

#[derive(Debug, Clone)]
pub struct Diagnostic<'a> {
    message: Message<'a>,
    location: TextLocation<'a>,
}

impl<'a> Diagnostic<'a> {
    pub(self) fn runtime(
        message: Message<'a>,
        span: Option<TextSpan>,
        source_text: &'a SourceText<'a>,
    ) -> Self {
        Self {
            message,
            location: TextLocation {
                span: span.unwrap_or_else(TextSpan::zero),
                source_text,
            },
        }
    }

    pub fn to_string(self, struct_table: &[String]) -> String {
        format!("Error {}: {}", self.location, self.message.to_string_with_struct_table(struct_table))
    }
}

#[derive(Debug, Clone)]
pub struct DiagnosticBag<'a> {
    pub diagnostics: Vec<Diagnostic<'a>>,
    pub source_text: &'a SourceText<'a>,
    pub registered_types: Vec<String>,
}

impl<'a> DiagnosticBag<'a> {
    pub fn new(source_text: &'a SourceText<'a>) -> Self {
        Self {
            diagnostics: vec![],
            source_text,
            registered_types: vec![],
        }
    }

    pub fn has_errors(&self) -> bool {
        !self.diagnostics.is_empty()
    }

    pub fn flush_to_console(self) {
        for diagnostic in self.diagnostics {
            println!("{}", diagnostic.to_string(&self.registered_types));
        }
    }

    fn report(&mut self, message: Message<'a>, span: TextSpan) {
        let diagnostic = Diagnostic {
            message,
            location: TextLocation {
                span,
                source_text: self.source_text,
            },
        };
        self.diagnostics.push(diagnostic);
    }

    fn report_runtime(&mut self, message: Message<'a>, span: Option<TextSpan>) {
        let diagnostic = Diagnostic::runtime(message, span, self.source_text);
        self.diagnostics.push(diagnostic);
    }

    pub fn report_bad_input(&mut self, start: usize, character: char) {
        let message = format!("Bad character in input: {}", character);
        let span = TextSpan::new(start, 1, true);
        self.report(message.into(), span)
    }
    pub fn report_bad_integer(&mut self, start: usize, text: &str) {
        let message = format!("'{}' is no valid u64 integer.", text);
        let span = TextSpan::new(start, text.len(), true);
        self.report(message.into(), span)
    }

    pub fn report_unexpected_token_kind(
        &mut self,
        span: TextSpan,
        actual_token_kind: SyntaxTokenKind,
        expected_token_kind: SyntaxTokenKind,
    ) {
        let message = format!(
            "Expected token kind {:?} but actually found {:?}.",
            expected_token_kind, actual_token_kind
        );
        self.report(message.into(), span)
    }

    pub fn report_cannot_convert(&mut self, span: TextSpan, from_type: &Type, to_type: &Type) {
        // let message = Message::Composition(vec![
        //     "Cannot convert type ".into(),
        //     from_type.into(),
        //     " to type ".into(),
        //     to_type.into(),
        //     ".".into()
        // ]);
        let message = message_format!("Cannot convert type ", from_type, " to type ", to_type, ".");
        self.report(message, span)
    }

    pub fn report_invalid_void_expression(&mut self, span: TextSpan) {
        let message = "Expected value type, but expression is of type void.".into();
        self.report(message, span)
    }

    pub fn report_no_unary_operator(&mut self, span: TextSpan, operator: &str, type_: &Type) {
        let message = format!("No unary operator {} for type ", operator);
        let message : &[Message] = &[
            message.into(),
            type_.into(),
            ".".into(),
        ];
        self.report(message.into(), span);
    }

    pub fn report_no_binary_operator(
        &mut self,
        span: TextSpan,
        lhs_type: &Type,
        operator: &str,
        rhs_type: &Type,
    ) {
        let message = format!(
            "No binary operator {} for types ",
            operator,
        );
        let message : &[Message] = &[
            message.into(),
            lhs_type.into(),
            " and ".into(),
            rhs_type.into(),
            ".".into(),
        ];
        self.report(message.into(), span);
    }

    #[allow(dead_code)]
    pub fn report_not_supported(&mut self, span: TextSpan, unsupported: &str) {
        let message = format!("Currently {} are not supported!", unsupported);
        self.report(message.into(), span);
    }

    pub fn report_cannot_declare_variable(&mut self, span: TextSpan, variable_name: &str) {
        let message = format!(
            "A variable named '{}' cannot be declared here.",
            variable_name
        );
        self.report(message.into(), span);
    }

    pub(crate) fn report_cannot_declare_struct(&mut self, span: TextSpan, struct_name: &str) {
        let message = format!("A struct named {} was already declared.", struct_name);
        // FIXME: Reference the first declaration of a struct with that name.
        self.report(message.into(), span);
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
        self.report(message.into(), span);
    }

    pub fn report_field_already_declared(&mut self, span: TextSpan, name: &str) {
        let message = format!("Field named {} already declared in struct.", name);
        // FIXME: Reference the first declaration of a field with that name.
        self.report(message.into(), span);
    }

    pub fn report_variable_not_found(&mut self, span: TextSpan, variable_name: &str) {
        let message = format!("No variable named '{}' could be found.", variable_name);
        self.report(message.into(), span);
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
        self.report(message.into(), span)
    }

    pub fn report_unexpected_parameter_count(
        &mut self,
        span: TextSpan,
        actual_parameter_count: usize,
        expected_parameter_count: usize,
    ) {
        let message = format!(
            "Expected {} parameters, but actually found {}.",
            expected_parameter_count, actual_parameter_count
        );
        self.report(message.into(), span)
    }

    pub fn report_unterminated_comment(&mut self, start: usize, text: &str) {
        let span = TextSpan::new(start, text.len(), true);
        let message = "Unterminated comment found here.".into();
        self.report(message, span);
    }

    pub fn report_unterminated_string(&mut self, span: TextSpan, expected_char: char) {
        let message = format!(
            "Unterminated string found here. Expected `{}`.",
            expected_char
        );
        self.report(message.into(), span);
    }

    pub fn report_no_fields_on_type(&mut self, span: TextSpan, type_: &Type) {
        let message : &[Message] = &[
            "There are no fields on type ".into(),
            type_.into(),
            ".".into(),
        ];
        self.report(message.into(), span);
    }

    pub fn report_no_field_named_on_type(
        &mut self,
        span: TextSpan,
        field_name: &str,
        type_: &Type,
    ) {
        let message = format!(
            "There are no fields named '{}' on type ",
            field_name,
        );
        let message : &[Message] = &[
            message.into(),
            type_.into(),
            ".".into(),
        ];
        self.report(message.into(), span);
    }

    pub fn report_no_field_named_on_struct(
        &mut self,
        span: TextSpan,
        field_name: &str,
        bound_struct_type: BoundStructSymbol<'_>,
    ) {
        let mut message = format!(
            "There is no field named {} on struct {}.",
            field_name, bound_struct_type.name
        );
        // TODO: Add optional information field and have a span for the Struct
        // source code.
        if !bound_struct_type.fields.is_empty() {
            message.push_str("\n  Available fields are:\n");
            for field in &bound_struct_type.fields {
                message.push_str("    ");
                message.push_str(&field.name);
                message.push_str(": ");
                message.push_str(&field.type_.to_string());
                message.push_str(";\n");
            }
        }
        self.report(message.into(), span);
    }

    pub fn report_not_all_fields_have_been_assigned(
        &mut self,
        span: TextSpan,
        struct_name: &str,
        unassigned_fields: &[&str],
    ) {
        let mut message = format!("Not all fields have been assigned in $constructor of struct {}. The following fields are missing:\n", struct_name);
        for field in unassigned_fields {
            message.push_str("    ");
            message.push_str(field);
            message.push('\n');
        }
        self.report(message.into(), span);
    }

    pub fn report_invalid_top_level_statement(&mut self, span: TextSpan, kind: SyntaxNodeKind) {
        let message = format!(
            "{:?} is no valid top level statement. Use functions instead.",
            kind
        );
        self.report(message.into(), span);
    }

    pub fn report_unknown_type(&mut self, span: TextSpan, type_name: &str) {
        let message = format!("Unknown type {} found.", type_name);
        self.report(message.into(), span);
    }

    pub fn report_missing_return_value(&mut self, span: TextSpan, expected_return_type: &Type) {
        let message : &[Message] = &[
            "Function returns type ".into(),
            expected_return_type.into(),
            " and needs a value of this type.".into(),
        ];
        self.report(message.into(), span);
    }

    pub fn report_missing_return_statement(&mut self, span: TextSpan, expected_return_type: &Type) {
        let message : &[Message] = &[
            "Not all paths in function return. Every path needs a return value of type".into(),
            expected_return_type.into(),
            ".".into(),
        ];
        self.report(message.into(), span);
    }

    pub fn report_invalid_variable_type_none(&mut self, span: TextSpan) {
        let message = "Variables cannot be assigned none, without type information. Try adding a : TypeDeclaration.".into();
        self.report(message, span);
    }

    pub fn report_unnecessary_cast(&mut self, span: TextSpan, from_type: &Type, to_type: &Type) {
        let cast_return_type = &Type::noneable(to_type.clone());
        let message= message_format!("No cast necessary between types ", from_type, " and ", to_type, ". Remove the unnecessary cast. Note, that cast will return ", cast_return_type, ".",);
        self.report(message, span);
    }

    pub fn report_impossible_cast(&mut self, span: TextSpan, from_type: &Type, to_type: &Type) {
        let cast_return_type = &Type::noneable(to_type.clone());
        let message = message_format!("No cast possible between types ", from_type, " and ", to_type, ". Note, that cast will return ", cast_return_type, ".");
        self.report(message, span);
    }

    pub fn report_expected_constant(&mut self, span: TextSpan) {
        let message = "Only constant expressions are allowed here.".into();
        self.report(message, span);
    }

    pub fn report_only_function_call_in_import_statement(&mut self, span: TextSpan) {
        let message =
            "Only function calls like lib('path/to/lib.sld') are valid in import statements."
                .into();
        self.report(message, span);
    }

    pub fn report_unknown_import_function(&mut self, span: TextSpan, function_name: &str) {
        let message = format!(
            "No function named {} found for imports. Use lib('path/to/lib.sld') instead.",
            function_name
        );
        self.report(message.into(), span);
    }

    pub(crate) fn report_unknown_library(&mut self, span: TextSpan, library_name: &str) {
        let message = format!(
            "No library named {} found. Did you misspell it? Or are you missing an import?",
            library_name
        );
        self.report(message.into(), span);
    }

    pub fn report_errors_in_referenced_library(&mut self, span: TextSpan, library_name: &str) {
        let message = format!("Errors in {} found. Fix them and recompile.", library_name);
        self.report(message.into(), span);
    }

    pub fn report_no_main_function_found(&mut self) {
        let message =
            "No main() function found in file. Did you forget it? Or maybe this is a library."
                .into();
        self.report(message, TextSpan::zero());
    }

    pub fn report_invalid_field_name(&mut self, span: TextSpan, field_name: &str) {
        let message = format!("Cannot use $ fields in code. Field was {}.", field_name);
        self.report(message.into(), span);
    }

    pub fn report_unrecognized_operator_function(&mut self, span: TextSpan, identifier: &str) {
        let message = format!("Function name {} was not recognized as a valid operator functions. Remove the $ if you want to call this function. Or use a valid name like $constructor.", identifier);
        self.report(message.into(), span);
    }

    pub fn report_cannot_index_get(&mut self, span: TextSpan, type_: &Type) {
        let message = message_format!("Type ", type_, " cannot be accessed with [], maybe it is missing a $get function?");
        self.report(message, span);
    }

    pub fn report_cannot_index_set(&mut self, span: TextSpan, type_: &Type) {
        let message = message_format!("Type ", type_, " cannot be accessed with [], maybe it is missing a $set function?");
        self.report(message, span);
    }

    pub fn report_cannot_iterate(&mut self, span: TextSpan, type_: &Type) {
        let message = message_format!("Type ", type_, " cannot be for a `for`-loop, maybe it is missing a $get and $elementCount function?");
        self.report(message, span);
    }

    // Runtime Errors
    pub fn no_heap_memory_left(&mut self, span: Option<TextSpan>, needed_memory_in_bytes: u64) {
        let message = format!(
            "Out of memory. No heap memory for {} bytes left.",
            needed_memory_in_bytes
        );
        self.report_runtime(message.into(), span);
    }

    pub fn division_by_zero(&mut self, span: Option<TextSpan>) {
        let message = "Divison by Zero.".into();
        self.report_runtime(message, span);
    }
}
