use super::DebugPrinter;
use crate::{parser::syntax_nodes::*, text::SourceTextCollection};
use std::fmt::Write;

pub fn print_syntax_node_as_code(node: &SyntaxNode, source_text_collection: &SourceTextCollection) {
    let mut buffer = String::new();
    print_syntax_node_as_code_with_indent(
        node,
        source_text_collection,
        DebugPrinter::default(),
        &mut buffer,
    )
    .unwrap();
    print!("{}", buffer);
}

fn print_syntax_node_as_code_with_indent(
    node: &SyntaxNode,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    match &node.kind {
        SyntaxNodeKind::CompilationUnit(compilation_unit) => {
            print_syntax_node_compilation_unit_as_code_with_indent(
                compilation_unit,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::_ConstDeclaration(_const_declaration) => {
            unimplemented!();
            // print_syntax_node_const_declaration_as_code_with_indent(const_declaration, source_text_collection, printer, buffer)
        }
        SyntaxNodeKind::ImportStatement(import_statement) => {
            print_syntax_node_import_statement_as_code_with_indent(
                import_statement,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::FunctionDeclaration(function_declaration) => {
            print_syntax_node_function_declaration_as_code_with_indent(
                function_declaration,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::StructDeclaration(struct_declaration) => {
            print_syntax_node_struct_declaration_as_code_with_indent(
                struct_declaration,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::EnumDeclaration(enum_declaration) => {
            print_syntax_node_enum_declaration_as_code_with_indent(
                enum_declaration,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::Literal(literal) => print_syntax_node_literal_as_code_with_indent(
            literal,
            source_text_collection,
            printer,
            buffer,
        ),
        SyntaxNodeKind::ArrayLiteral(array_literal) => {
            print_syntax_node_array_literal_as_code_with_indent(
                array_literal,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::RepetitionNode(repetition_node) => {
            print_syntax_node_repetition_node_as_code_with_indent(
                repetition_node,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::CastExpression(cast_expression) => {
            print_syntax_node_cast_expression_as_code_with_indent(
                cast_expression,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::ConstructorCall(constructor_call) => {
            print_syntax_node_constructor_call_as_code_with_indent(
                constructor_call,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::Variable(variable) => print_syntax_node_variable_as_code_with_indent(
            variable,
            source_text_collection,
            printer,
            buffer,
        ),
        SyntaxNodeKind::Binary(binary) => print_syntax_node_binary_as_code_with_indent(
            binary,
            source_text_collection,
            printer,
            buffer,
        ),
        SyntaxNodeKind::Unary(unary) => print_syntax_node_unary_as_code_with_indent(
            unary,
            source_text_collection,
            printer,
            buffer,
        ),
        SyntaxNodeKind::Parenthesized(parenthesized) => {
            print_syntax_node_parenthesized_as_code_with_indent(
                parenthesized,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::FunctionCall(function_call) => {
            print_syntax_node_function_call_as_code_with_indent(
                function_call,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::ArrayIndex(array_index) => {
            print_syntax_node_array_index_as_code_with_indent(
                array_index,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::FieldAccess(field_access) => {
            print_syntax_node_field_access_as_code_with_indent(
                field_access,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::BlockStatement(block_statement) => {
            print_syntax_node_block_statement_as_code_with_indent(
                block_statement,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::ForStatement(for_statement) => {
            print_syntax_node_for_statement_as_code_with_indent(
                for_statement,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::IfStatement(if_statement) => {
            print_syntax_node_if_statement_as_code_with_indent(
                if_statement,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::VariableDeclaration(variable_declaration) => {
            print_syntax_node_variable_declaration_as_code_with_indent(
                variable_declaration,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::ReturnStatement(return_statement) => {
            print_syntax_node_return_statement_as_code_with_indent(
                return_statement,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::WhileStatement(while_statement) => {
            print_syntax_node_while_statement_as_code_with_indent(
                while_statement,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::Assignment(assignment) => print_syntax_node_assignment_as_code_with_indent(
            assignment,
            source_text_collection,
            printer,
            buffer,
        ),
        SyntaxNodeKind::ExpressionStatement(expression_statement) => {
            print_syntax_node_expression_statement_as_code_with_indent(
                expression_statement,
                source_text_collection,
                printer,
                buffer,
            )
        }
        SyntaxNodeKind::StructField(struct_field) => {
            print_syntax_node_struct_field_as_code_with_indent(
                struct_field,
                source_text_collection,
                printer,
                buffer,
            )
        }
    }
}

fn print_syntax_node_compilation_unit_as_code_with_indent(
    compilation_unit: &CompilationUnitNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    for statement in &compilation_unit.statements {
        print_syntax_node_as_code_with_indent(statement, source_text_collection, printer, buffer)?;
    }
    Ok(())
}

fn print_syntax_node_import_statement_as_code_with_indent(
    import_statement: &ImportStatementNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    printer.output_indentation(buffer);
    write!(buffer, "import ")?;
    print_syntax_node_as_code_with_indent(
        &*import_statement.function,
        source_text_collection,
        printer,
        buffer,
    )?;
    writeln!(
        buffer,
        " as {};",
        &source_text_collection[import_statement.identifier.location]
    )?;
    Ok(())
}

fn print_syntax_node_function_declaration_as_code_with_indent(
    function_declaration: &FunctionDeclarationNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    printer.output_indentation(buffer);
    if function_declaration.optional_generic_keyword.is_some() {
        write!(buffer, "generic ")?;
    }
    write!(
        buffer,
        "func {}",
        &source_text_collection[function_declaration.identifier.location]
    )?;
    print_function_type_node_as_code_with_indent(
        &function_declaration.function_type,
        source_text_collection,
        printer,
        buffer,
    )?;
    print_syntax_node_as_code_with_indent(
        &function_declaration.body,
        source_text_collection,
        printer,
        buffer,
    )?;
    writeln!(buffer)?;
    Ok(())
}

fn print_function_type_node_as_code_with_indent(
    function_type: &FunctionTypeNode,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    write!(buffer, "(")?;
    for parameter in &function_type.parameters {
        print_parameter_node_as_code_with_indent(
            parameter,
            source_text_collection,
            printer,
            buffer,
        )?;
        write!(buffer, ", ")?;
    }
    write!(buffer, ")")?;
    if let Some(return_type) = &function_type.return_type {
        write!(buffer, " -> ")?;
        print_type_node_as_code_with_indent(
            &return_type.return_type,
            source_text_collection,
            printer,
            buffer,
        )?;
    }
    writeln!(buffer)?;
    Ok(())
}

fn print_parameter_node_as_code_with_indent(
    parameter: &ParameterNode,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    write!(
        buffer,
        "{}: ",
        &source_text_collection[parameter.identifier.location]
    )?;
    print_type_node_as_code_with_indent(
        &parameter.type_declaration.type_,
        source_text_collection,
        printer,
        buffer,
    )?;
    Ok(())
}

fn print_type_node_as_code_with_indent(
    type_node: &TypeNode,
    source_text_collection: &SourceTextCollection,
    _printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    if type_node.optional_ampersand_token.is_some() {
        write!(buffer, "&")?;
    }
    if let Some(library_name) = &type_node.library_name {
        write!(
            buffer,
            "{}.",
            &source_text_collection[library_name.location]
        )?;
    }
    write!(
        buffer,
        "{}",
        &source_text_collection[type_node.type_name.location]
    )?;
    if type_node.optional_question_mark.is_some() {
        write!(buffer, "?")?;
    }
    for _ in &type_node.brackets {
        write!(buffer, "[]")?;
    }
    Ok(())
}

fn print_syntax_node_struct_declaration_as_code_with_indent(
    struct_declaration: &StructDeclarationNodeKind,
    source_text_collection: &SourceTextCollection,
    mut printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    printer.output_indentation(buffer);
    if struct_declaration.optional_generic_keyword.is_some() {
        write!(buffer, "generic ")?;
    }
    write!(
        buffer,
        "struct {}",
        &source_text_collection[struct_declaration.identifier.location]
    )?;
    if let Some(parent) = &struct_declaration.optional_parent {
        write!(buffer, " < {}", &source_text_collection[parent.location])?;
    }
    writeln!(buffer, " {{")?;
    printer.indent += 4;
    for statement in &struct_declaration.body.statements {
        print_syntax_node_as_code_with_indent(statement, source_text_collection, printer, buffer)?;
    }
    printer.indent -= 4;
    writeln!(buffer, "}}")?;
    writeln!(buffer)?;
    Ok(())
}

fn print_syntax_node_enum_declaration_as_code_with_indent(
    enum_declaration: &EnumDeclarationNodeKind,
    source_text_collection: &SourceTextCollection,
    mut printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    printer.output_indentation(buffer);
    writeln!(
        buffer,
        "enum {} {{",
        &source_text_collection[enum_declaration.identifier.location]
    )?;
    printer.indent += 4;
    for value in &enum_declaration.body.values {
        print_enum_value_node_as_code_with_indent(value, source_text_collection, printer, buffer)?;
        writeln!(buffer, ",")?;
    }
    printer.indent -= 4;
    write!(buffer, "}}")?;
    Ok(())
}

fn print_enum_value_node_as_code_with_indent(
    value: &EnumValueNode,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    printer.output_indentation(buffer);
    write!(
        buffer,
        "{}",
        &source_text_collection[value.identifier.location]
    )?;
    Ok(())
}

fn print_syntax_node_literal_as_code_with_indent(
    literal: &LiteralNodeKind,
    _source_text_collection: &SourceTextCollection,
    _printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    match &literal.value {
        crate::value::Value::None => write!(buffer, "none"),
        crate::value::Value::Integer(it) => write!(buffer, "{it}"),
        crate::value::Value::Boolean(it) => write!(buffer, "{it}"),
        crate::value::Value::SystemCall(it) => write!(buffer, "{it}"),
        crate::value::Value::String(it) => write!(buffer, "{it:?}"),
        crate::value::Value::LabelPointer(it, _) => write!(buffer, "L{it:X}"),
        crate::value::Value::EnumType(..) => write!(buffer, "ENUM TYPE"),
    }
}

fn print_syntax_node_array_literal_as_code_with_indent(
    array_literal: &ArrayLiteralNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    write!(buffer, "[ ")?;
    for child in &array_literal.children {
        print_syntax_node_as_code_with_indent(child, source_text_collection, printer, buffer)?;
        write!(buffer, ", ")?;
    }
    write!(buffer, "]")?;
    Ok(())
}

fn print_syntax_node_repetition_node_as_code_with_indent(
    repetition_node: &RepetitionNodeNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    print_syntax_node_as_code_with_indent(
        &repetition_node.base_expression,
        source_text_collection,
        printer,
        buffer,
    )?;
    write!(buffer, "; ")?;
    print_syntax_node_as_code_with_indent(
        &repetition_node.repetition,
        source_text_collection,
        printer,
        buffer,
    )?;
    Ok(())
}

fn print_syntax_node_cast_expression_as_code_with_indent(
    cast_expression: &CastExpressionNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    write!(buffer, "cast ")?;
    print_syntax_node_as_code_with_indent(
        &cast_expression.expression,
        source_text_collection,
        printer,
        buffer,
    )?;
    write!(buffer, " : ")?;
    print_type_node_as_code_with_indent(
        &cast_expression.type_,
        source_text_collection,
        printer,
        buffer,
    )?;
    Ok(())
}

fn print_syntax_node_constructor_call_as_code_with_indent(
    constructor_call: &ConstructorCallNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    write!(buffer, "new ")?;
    if let Some(library) = constructor_call.library_name {
        write!(buffer, "{}.", &source_text_collection[library.location])?;
    }
    write!(
        buffer,
        "{}(",
        &source_text_collection[constructor_call.type_name.location]
    )?;
    for argument in &constructor_call.arguments {
        print_syntax_node_as_code_with_indent(argument, source_text_collection, printer, buffer)?;
        write!(buffer, ", ")?;
    }
    write!(buffer, ")")?;
    Ok(())
}

fn print_syntax_node_variable_as_code_with_indent(
    variable: &VariableNodeKind,
    source_text_collection: &SourceTextCollection,
    _printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    write!(
        buffer,
        "{}",
        &source_text_collection[variable.token.location]
    )?;
    Ok(())
}

fn print_syntax_node_binary_as_code_with_indent(
    binary: &BinaryNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    write!(buffer, "(")?;
    print_syntax_node_as_code_with_indent(&binary.lhs, source_text_collection, printer, buffer)?;
    write!(
        buffer,
        " {} ",
        &source_text_collection[binary.operator_token.location]
    )?;
    print_syntax_node_as_code_with_indent(&binary.rhs, source_text_collection, printer, buffer)?;
    write!(buffer, ")")?;
    Ok(())
}

fn print_syntax_node_unary_as_code_with_indent(
    unary: &UnaryNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    write!(
        buffer,
        "{}",
        &source_text_collection[unary.operator_token.location]
    )?;
    print_syntax_node_as_code_with_indent(&unary.operand, source_text_collection, printer, buffer)?;
    Ok(())
}

fn print_syntax_node_parenthesized_as_code_with_indent(
    parenthesized: &ParenthesizedNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    write!(buffer, "(")?;
    print_syntax_node_as_code_with_indent(
        &parenthesized.expression,
        source_text_collection,
        printer,
        buffer,
    )?;
    write!(buffer, ")")?;
    Ok(())
}

fn print_syntax_node_function_call_as_code_with_indent(
    function_call: &FunctionCallNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    print_syntax_node_as_code_with_indent(
        &function_call.base,
        source_text_collection,
        printer,
        buffer,
    )?;
    write!(buffer, "(")?;
    for argument in &function_call.arguments {
        print_syntax_node_as_code_with_indent(argument, source_text_collection, printer, buffer)?;
        write!(buffer, ", ")?;
    }
    write!(buffer, ")")?;
    Ok(())
}

fn print_syntax_node_array_index_as_code_with_indent(
    array_index: &ArrayIndexNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    print_syntax_node_as_code_with_indent(
        &array_index.base,
        source_text_collection,
        printer,
        buffer,
    )?;
    write!(buffer, "[")?;
    print_syntax_node_as_code_with_indent(
        &array_index.index,
        source_text_collection,
        printer,
        buffer,
    )?;
    write!(buffer, "]")?;
    Ok(())
}

fn print_syntax_node_field_access_as_code_with_indent(
    field_access: &FieldAccessNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    print_syntax_node_as_code_with_indent(
        &field_access.base,
        source_text_collection,
        printer,
        buffer,
    )?;
    write!(
        buffer,
        ".{}",
        &source_text_collection[field_access.field.location]
    )?;
    Ok(())
}

fn print_syntax_node_block_statement_as_code_with_indent(
    block_statement: &BlockStatementNodeKind,
    source_text_collection: &SourceTextCollection,
    mut printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    printer.output_indentation(buffer);
    writeln!(buffer, "{{")?;
    printer.indent += 4;
    for statement in &block_statement.statements {
        print_syntax_node_as_code_with_indent(statement, source_text_collection, printer, buffer)?;
    }
    printer.indent -= 4;
    printer.output_indentation(buffer);
    writeln!(buffer, "}}")?;
    Ok(())
}

fn print_syntax_node_for_statement_as_code_with_indent(
    for_statement: &ForStatementNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    printer.output_indentation(buffer);
    write!(buffer, "for ")?;
    if let Some(index_variable) = for_statement.optional_index_variable {
        write!(
            buffer,
            "{}, ",
            &source_text_collection[index_variable.location]
        )?;
    }
    write!(
        buffer,
        "{} in ",
        &source_text_collection[for_statement.variable.location]
    )?;
    print_syntax_node_as_code_with_indent(
        &for_statement.collection,
        source_text_collection,
        printer,
        buffer,
    )?;
    writeln!(buffer)?;

    print_syntax_node_as_code_with_indent(
        &for_statement.body,
        source_text_collection,
        printer,
        buffer,
    )?;
    Ok(())
}

fn print_syntax_node_if_statement_as_code_with_indent(
    if_statement: &IfStatementNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    printer.output_indentation(buffer);
    write!(buffer, "if ")?;
    print_syntax_node_as_code_with_indent(
        &if_statement.condition,
        source_text_collection,
        printer,
        buffer,
    )?;
    writeln!(buffer)?;

    print_syntax_node_as_code_with_indent(
        &if_statement.body,
        source_text_collection,
        printer,
        buffer,
    )?;
    if let Some(else_clause) = &if_statement.else_clause {
        printer.output_indentation(buffer);
        write!(buffer, "else ")?;
        writeln!(buffer)?;
        print_syntax_node_as_code_with_indent(
            &else_clause.body,
            source_text_collection,
            printer,
            buffer,
        )?;
    }
    Ok(())
}

fn print_syntax_node_variable_declaration_as_code_with_indent(
    variable_declaration: &VariableDeclarationNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    printer.output_indentation(buffer);
    write!(
        buffer,
        "let {}",
        &source_text_collection[variable_declaration.identifier.location]
    )?;
    if let Some(type_specifier) = &variable_declaration.optional_type_declaration {
        write!(buffer, ": ")?;
        print_type_node_as_code_with_indent(
            &type_specifier.type_,
            source_text_collection,
            printer,
            buffer,
        )?;
    }
    write!(buffer, " = ")?;
    print_syntax_node_as_code_with_indent(
        &variable_declaration.initializer,
        source_text_collection,
        printer,
        buffer,
    )?;
    writeln!(buffer, ";")?;
    Ok(())
}

fn print_syntax_node_return_statement_as_code_with_indent(
    return_statement: &ReturnStatementNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    printer.output_indentation(buffer);
    write!(buffer, "return")?;
    if let Some(expression) = &return_statement.optional_expression {
        write!(buffer, " ")?;
        print_syntax_node_as_code_with_indent(
            &expression,
            source_text_collection,
            printer,
            buffer,
        )?;
    }
    writeln!(buffer, ";")?;
    Ok(())
}

fn print_syntax_node_while_statement_as_code_with_indent(
    while_statement: &WhileStatementNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    printer.output_indentation(buffer);
    write!(buffer, "while ")?;
    print_syntax_node_as_code_with_indent(
        &while_statement.condition,
        source_text_collection,
        printer,
        buffer,
    )?;
    writeln!(buffer)?;
    print_syntax_node_as_code_with_indent(
        &while_statement.body,
        source_text_collection,
        printer,
        buffer,
    )?;
    Ok(())
}

fn print_syntax_node_assignment_as_code_with_indent(
    assignment: &AssignmentNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    printer.output_indentation(buffer);
    print_syntax_node_as_code_with_indent(
        &assignment.lhs,
        source_text_collection,
        printer,
        buffer,
    )?;
    write!(buffer, " = ")?;
    print_syntax_node_as_code_with_indent(
        &assignment.expression,
        source_text_collection,
        printer,
        buffer,
    )?;
    writeln!(buffer, ";")?;
    Ok(())
}

fn print_syntax_node_expression_statement_as_code_with_indent(
    expression_statement: &ExpressionStatementNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    printer.output_indentation(buffer);
    print_syntax_node_as_code_with_indent(
        &expression_statement.expression,
        source_text_collection,
        printer,
        buffer,
    )?;
    writeln!(buffer, ";")?;
    Ok(())
}

fn print_syntax_node_struct_field_as_code_with_indent(
    struct_field: &StructFieldNodeKind,
    source_text_collection: &SourceTextCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    printer.output_indentation(buffer);
    print_parameter_node_as_code_with_indent(
        &struct_field.field,
        source_text_collection,
        printer,
        buffer,
    )?;
    writeln!(buffer, ";")?;
    Ok(())
}
