use crate::{
    binder::typing::{TypeCollection, TypeId},
    lexer::syntax_token::SyntaxTokenKind,
    parser::syntax_nodes::SyntaxNodeKind,
    text::{SourceTextCollection, SourceTextId, TextLocation, TextSpan},
};

#[derive(Debug, Clone)]
pub enum Message {
    String(String),
    SourceTextSnippet(TextLocation),
    TypeId(TypeId),
    Composition(Vec<Message>),
}

impl Message {
    pub fn into_string_with_struct_table(
        self,
        types: &TypeCollection,
        source_text_collection: &SourceTextCollection,
    ) -> String {
        match self {
            Message::String(it) => it.into(),
            Message::TypeId(type_) => types.name(type_).into_owned(),
            Message::SourceTextSnippet(it) => source_text_collection[it].into(),
            Message::Composition(composition) => composition
                .into_iter()
                .map(|m| m.into_string_with_struct_table(types, source_text_collection))
                .collect(),
        }
    }
}

impl From<String> for Message {
    fn from(it: String) -> Self {
        Self::String(it.into())
    }
}

impl From<TextLocation> for Message {
    fn from(it: TextLocation) -> Self {
        Self::SourceTextSnippet(it)
    }
}

impl From<&str> for Message {
    fn from(it: &str) -> Self {
        Self::String(it.into())
    }
}

impl From<TypeId> for Message {
    fn from(it: TypeId) -> Self {
        Self::TypeId(it.clone())
    }
}

impl From<&[Message]> for Message {
    fn from(it: &[Message]) -> Self {
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
pub struct Diagnostic {
    message: Message,
    location: TextLocation,
}

impl Diagnostic {
    pub(self) fn runtime(message: Message, location: Option<TextLocation>) -> Self {
        Self {
            message,
            location: location
                .expect("Instruction without location found. This should be removed!"),
        }
    }

    pub fn into_string(
        self,
        types: &TypeCollection,
        source_text_collection: &SourceTextCollection,
    ) -> String {
        format!(
            "Error {}: {}",
            self.location.display(source_text_collection),
            self.message
                .into_string_with_struct_table(types, source_text_collection)
        )
    }
}

#[derive(Debug, Clone)]
pub struct DiagnosticBag {
    pub diagnostics: Vec<Diagnostic>,
    pub registered_types: TypeCollection,
}

impl DiagnosticBag {
    pub fn new() -> Self {
        Self {
            diagnostics: vec![],
            registered_types: TypeCollection::new(),
        }
    }

    pub fn has_errors(&self) -> bool {
        !self.diagnostics.is_empty()
    }

    pub fn flush_to_console(self, source_text_collection: &SourceTextCollection) {
        for diagnostic in self.diagnostics {
            println!(
                "{}",
                diagnostic.into_string(&self.registered_types, source_text_collection)
            );
        }
    }

    fn report(&mut self, message: Message, location: TextLocation) {
        let diagnostic = Diagnostic { message, location };
        self.diagnostics.push(diagnostic);
    }

    fn report_runtime(&mut self, message: Message, location: Option<TextLocation>) {
        let diagnostic = Diagnostic::runtime(message, location);
        self.diagnostics.push(diagnostic);
    }

    pub fn report_bad_input(&mut self, start: usize, source_text: SourceTextId, character: char) {
        let message = format!("Bad character in input: {}", character);
        let span = TextSpan::new(start, 1, true);
        let location = TextLocation { span, source_text };
        self.report(message.into(), location)
    }
    pub fn report_bad_integer(&mut self, start: usize, source_text: SourceTextId, text: &str) {
        let message = format!("'{}' is no valid u64 integer.", text);
        let span = TextSpan::new(start, text.len(), true);
        let location = TextLocation { span, source_text };
        self.report(message.into(), location)
    }

    pub fn report_unexpected_token_kind(
        &mut self,
        location: TextLocation,
        actual_token_kind: SyntaxTokenKind,
        expected_token_kind: SyntaxTokenKind,
    ) {
        let message = format!(
            "Expected token kind {:?} but actually found {:?}.",
            expected_token_kind, actual_token_kind
        );
        self.report(message.into(), location)
    }

    pub fn report_cannot_convert(
        &mut self,
        location: TextLocation,
        from_type: TypeId,
        to_type: TypeId,
    ) {
        let message = message_format!("Cannot convert type ", from_type, " to type ", to_type, ".");
        self.report(message, location)
    }

    pub fn report_invalid_void_expression(&mut self, location: TextLocation) {
        let message = "Expected value type, but expression is of type void.".into();
        self.report(message, location)
    }

    pub fn report_no_unary_operator(
        &mut self,
        location: TextLocation,
        operator: TextLocation,
        type_: TypeId,
    ) {
        let message = message_format!("No unary operator ", operator, " for type ", type_, ".");
        self.report(message, location);
    }

    pub fn report_no_binary_operator(
        &mut self,
        location: TextLocation,
        lhs_type: TypeId,
        operator: TextLocation,
        rhs_type: TypeId,
    ) {
        let message = message_format!(
            "No binary operator ",
            operator,
            " for types ",
            lhs_type,
            " and ",
            rhs_type,
            "."
        );
        self.report(message, location);
    }

    #[allow(dead_code)]
    pub fn report_not_supported(&mut self, location: TextLocation, unsupported: &'static str) {
        let message = format!("Currently {} are not supported!", unsupported);
        self.report(message.into(), location);
    }

    pub fn report_cannot_declare_variable(
        &mut self,
        location: TextLocation,
        variable_name: impl Into<Message>,
    ) {
        let message = message_format!(
            "A variable named '",
            variable_name,
            "' cannot be declared here."
        );
        self.report(message, location);
    }

    pub(crate) fn report_cannot_declare_struct(
        &mut self,
        location: TextLocation,
        struct_name: TextLocation,
    ) {
        let message = message_format!("A struct named ", struct_name, " was already declared.");
        // FIXME: Reference the first declaration of a struct with that name.
        self.report(message, location);
    }

    pub fn report_cannot_declare_keyword_as_variable(
        &mut self,
        location: TextLocation,
        variable_name: TextLocation,
    ) {
        let message = message_format!(
            "'",
            variable_name,
            "' is a keyword and cannot be overwritten.",
        );
        self.report(message, location);
    }

    pub fn report_field_already_declared(&mut self, location: TextLocation, name: &str) {
        let message = message_format!("Field named ", name, " already declared in struct.");
        // FIXME: Reference the first declaration of a field with that name.
        self.report(message, location);
    }

    pub fn report_variable_not_found(&mut self, location: TextLocation, variable_name: TextLocation) {
        let message = message_format!("No variable named '", variable_name, "' could be found.");
        self.report(message, location);
    }

    pub fn report_cannot_assign_to(&mut self, location: TextLocation) {
        let message = "Only variables can be assigned to. Did you mean to use ==?".into();
        self.report(message, location);
    }

    pub fn report_variable_is_read_only(&mut self, location: TextLocation) {
        let message = "Variable cannot be assigned to, it is a read only variable.".into();
        self.report(message, location);
    }

    pub fn report_unexpected_argument_count(
        &mut self,
        location: TextLocation,
        actual_argument_count: usize,
        expected_argument_count: usize,
    ) {
        let message = format!(
            "Expected {} arguments, but actually found {}.",
            expected_argument_count, actual_argument_count
        );
        self.report(message.into(), location)
    }

    pub fn report_unexpected_parameter_count(
        &mut self,
        location: TextLocation,
        actual_parameter_count: usize,
        expected_parameter_count: usize,
    ) {
        let message = format!(
            "Expected {} parameters, but actually found {}.",
            expected_parameter_count, actual_parameter_count
        );
        self.report(message.into(), location)
    }

    pub fn report_unterminated_comment(
        &mut self,
        start: usize,
        source_text: SourceTextId,
        text: &str,
    ) {
        let span = TextSpan::new(start, text.len(), true);
        let location = TextLocation { span, source_text };
        let message = "Unterminated comment found here.".into();
        self.report(message, location);
    }

    pub fn report_unterminated_string(&mut self, location: TextLocation, expected_char: char) {
        let message = format!(
            "Unterminated string found here. Expected `{}`.",
            expected_char
        );
        self.report(message.into(), location);
    }

    pub fn report_no_fields_on_type(&mut self, location: TextLocation, type_: TypeId) {
        let message: &[Message] = &[
            "There are no fields on type ".into(),
            type_.into(),
            ".".into(),
        ];
        self.report(message.into(), location);
    }

    pub fn report_no_field_named_on_type(
        &mut self,
        location: TextLocation,
        field_name: TextLocation,
        type_: TypeId,
    ) {
        let message = message_format!(
            "There are no fields named '",
            field_name,
            "' on type ",
            type_,
            "."
        );
        self.report(message, location);
    }

    pub fn report_no_field_named_on_struct(
        &mut self,
        location: TextLocation,
        field_name: TextLocation,
        bound_struct_type: TypeId,
    ) {
        let message = message_format!(
            "There is no field named ",
            field_name,
            " on struct ",
            bound_struct_type
        );
        // TODO: Add optional information field and have a span for the Struct
        // source code.
        // if !bound_struct_type.fields.is_empty() {
        //     message.push_str("\n  Available fields are:\n");
        //     for field in &bound_struct_type.fields {
        //         message.push_str("    ");
        //         message.push_str(&field.name);
        //         message.push_str(": ");
        //         message.push_str(&field.type_.to_string());
        //         message.push_str(";\n");
        //     }
        // }
        self.report(message, location);
    }

    pub fn report_not_all_fields_have_been_assigned(
        &mut self,
        span: TextLocation,
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

    pub fn report_invalid_top_level_statement(&mut self, span: TextLocation, kind: SyntaxNodeKind) {
        let message = format!(
            "{:?} is no valid top level statement. Use functions instead.",
            kind
        );
        self.report(message.into(), span);
    }

    pub fn report_unknown_type(&mut self, location: TextLocation, type_name: impl Into<Message>) {
        let message = message_format!("Unknown type ", type_name, " found.");
        self.report(message, location);
    }

    pub fn report_missing_return_value(
        &mut self,
        span: TextLocation,
        expected_return_type: TypeId,
    ) {
        let message: &[Message] = &[
            "Function returns type ".into(),
            expected_return_type.into(),
            " and needs a value of this type.".into(),
        ];
        self.report(message.into(), span);
    }

    pub fn report_missing_return_statement(
        &mut self,
        span: TextLocation,
        expected_return_type: TypeId,
    ) {
        let message: &[Message] = &[
            "Not all paths in function return. Every path needs a return value of type".into(),
            expected_return_type.into(),
            ".".into(),
        ];
        self.report(message.into(), span);
    }

    pub fn report_invalid_variable_type_none(&mut self, span: TextLocation) {
        let message = "Variables cannot be assigned none, without type information. Try adding a : TypeDeclaration.".into();
        self.report(message, span);
    }

    pub fn report_unnecessary_cast(
        &mut self,
        span: TextLocation,
        from_type: TypeId,
        to_type: TypeId,
    ) {
        let message = message_format!(
            "No cast necessary between types ",
            from_type,
            " and ",
            to_type,
            ". Remove the unnecessary cast. Note, that cast will return ",
            to_type,
            "?.",
        );
        self.report(message, span);
    }

    pub fn report_impossible_cast(
        &mut self,
        span: TextLocation,
        from_type: TypeId,
        to_type: TypeId,
    ) {
        let message = message_format!(
            "No cast possible between types ",
            from_type,
            " and ",
            to_type,
            ". Note, that cast will return ",
            to_type,
            "?."
        );
        self.report(message, span);
    }

    pub fn report_expected_constant(&mut self, span: TextLocation) {
        let message = "Only constant expressions are allowed here.".into();
        self.report(message, span);
    }

    pub fn report_only_function_call_in_import_statement(&mut self, span: TextLocation) {
        let message =
            "Only function calls like lib('path/to/lib.sld') are valid in import statements."
                .into();
        self.report(message, span);
    }

    pub fn report_unknown_import_function(
        &mut self,
        location: TextLocation,
        function_name: TextLocation,
    ) {
        let message = message_format!(
            "No function named ",
            function_name,
            " found for imports. Use lib('path/to/lib.sld') instead."
        );
        self.report(message, location);
    }

    pub(crate) fn report_unknown_library(&mut self, span: TextLocation, library_name: TextLocation) {
        let message = message_format!(
            "No library named ", library_name, " found. Did you misspell it? Or are you missing an import?",
        );
        self.report(message, span);
    }

    pub fn report_errors_in_referenced_library(&mut self, span: TextLocation, library_name: &str) {
        let message = format!("Errors in {} found. Fix them and recompile.", library_name);
        self.report(message.into(), span);
    }

    pub fn report_no_main_function_found(&mut self, source_text: SourceTextId) {
        let message =
            "No main() function found in file. Did you forget it? Or maybe this is a library."
                .into();
        self.report(message, TextLocation::zero_in_file(source_text));
    }

    pub fn report_invalid_field_name(&mut self, span: TextLocation, field_name: &str) {
        let message = format!("Cannot use $ fields in code. Field was {}.", field_name);
        self.report(message.into(), span);
    }

    pub fn report_unrecognized_operator_function(&mut self, span: TextLocation, identifier: TextLocation) {
        let message = message_format!("Function name ", identifier, " was not recognized as a valid operator functions. Remove the $ if you want to call this function. Or use a valid name like $constructor.");
        self.report(message, span);
    }

    pub fn report_cannot_index_get(&mut self, span: TextLocation, type_: TypeId) {
        let message = message_format!(
            "Type ",
            type_,
            " cannot be accessed with [], maybe it is missing a $get function?"
        );
        self.report(message, span);
    }

    pub fn report_cannot_index_set(&mut self, span: TextLocation, type_: TypeId) {
        let message = message_format!(
            "Type ",
            type_,
            " cannot be accessed with [], maybe it is missing a $set function?"
        );
        self.report(message, span);
    }

    pub fn report_cannot_iterate(&mut self, span: TextLocation, type_: TypeId) {
        let message = message_format!(
            "Type ",
            type_,
            " cannot be for a `for`-loop, maybe it is missing a $get and $elementCount function?"
        );
        self.report(message, span);
    }

    pub fn report_generic_type_in_ungeneric_struct(
        &mut self,
        span: TextLocation,
        struct_name: TextLocation,
    ) {
        let message = message_format!(
            "Used generic type in struct ",
            struct_name,
            " without declaring it as 'generic'."
        );
        // TODO: Add hint to struct declaration.
        self.report(message, span);
    }

    // Runtime Errors
    pub fn no_heap_memory_left(&mut self, span: Option<TextLocation>, needed_memory_in_bytes: u64) {
        let message = format!(
            "Out of memory. No heap memory for {} bytes left.",
            needed_memory_in_bytes
        );
        self.report_runtime(message.into(), span);
    }

    pub fn division_by_zero(&mut self, span: Option<TextLocation>) {
        let message = "Divison by Zero.".into();
        self.report_runtime(message, span);
    }
}
