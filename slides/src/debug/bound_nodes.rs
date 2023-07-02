use super::DebugPrinter;
use crate::binder::{
    bound_nodes::*,
    typing::{TypeCollection, TypeId},
};
use std::fmt::Write;

pub fn print_bound_node_as_code(node: &BoundNode, types: &TypeCollection) {
    let mut buffer = String::new();
    print_bound_node_as_code_with_indent(node, types, DebugPrinter::default(), &mut buffer)
        .unwrap();
    print!("{}", buffer);
}

pub fn bound_node_as_code_to_string(node: &BoundNode, types: &TypeCollection, buffer: &mut String) {
    print_bound_node_as_code_with_indent(node, types, DebugPrinter::default(), buffer).unwrap();
}

fn print_bound_node_as_code_with_indent(
    node: &BoundNode,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    match &node.kind {
        BoundNodeKind::ErrorExpression => {
            print_bound_node_error_expression_as_code(types, printer, buffer)
        }
        BoundNodeKind::Label(label) => {
            print_bound_node_label_as_code(label, types, printer, buffer)
        }
        BoundNodeKind::LabelReference(label_reference) => {
            print_bound_node_label_reference_as_code(label_reference, types, printer, buffer)
        }
        BoundNodeKind::FunctionDeclaration(function_declaration) => {
            print_bound_node_function_declaration_as_code(
                function_declaration,
                types,
                printer,
                buffer,
            )
        }
        BoundNodeKind::LiteralExpression(literal_expression) => {
            print_bound_node_literal_expression_as_code(literal_expression, types, printer, buffer)
        }
        BoundNodeKind::ArrayLiteralExpression(array_literal_expression) => {
            print_bound_node_array_literal_expression_as_code(
                array_literal_expression,
                types,
                printer,
                buffer,
            )
        }
        BoundNodeKind::ConstructorCall(constructor_call) => {
            print_bound_node_constructor_call_as_code(constructor_call, types, printer, buffer)
        }
        BoundNodeKind::VariableExpression(variable_expression) => {
            print_bound_node_variable_expression_as_code(
                variable_expression,
                node.type_,
                types,
                printer,
                buffer,
            )
        }
        BoundNodeKind::UnaryExpression(unary_expression) => {
            print_bound_node_unary_expression_as_code(unary_expression, types, printer, buffer)
        }
        BoundNodeKind::BinaryExpression(binary_expression) => {
            print_bound_node_binary_expression_as_code(binary_expression, types, printer, buffer)
        }
        BoundNodeKind::FunctionCall(function_call) => {
            print_bound_node_function_call_as_code(function_call, types, printer, buffer)
        }
        BoundNodeKind::SystemCall(system_call) => {
            print_bound_node_system_call_as_code(system_call, types, printer, buffer)
        }
        BoundNodeKind::ArrayIndex(array_index) => {
            print_bound_node_array_index_as_code(array_index, types, printer, buffer)
        }
        BoundNodeKind::FieldAccess(field_access) => {
            print_bound_node_field_access_as_code(field_access, types, printer, buffer)
        }
        BoundNodeKind::Closure(closure) => {
            print_bound_node_closure_as_code(closure, types, printer, buffer)
        }
        BoundNodeKind::Conversion(conversion) => {
            print_bound_node_conversion_as_code(conversion, types, printer, buffer)
        }
        BoundNodeKind::RepetitionNode(repetition_node) => {
            print_bound_node_repetition_node_as_code(repetition_node, types, printer, buffer)
        }
        BoundNodeKind::BlockStatement(block_statement) => print_bound_node_block_statement_as_code(
            node.type_,
            block_statement,
            types,
            printer,
            buffer,
        ),
        BoundNodeKind::IfStatement(if_statement) => {
            print_bound_node_if_statement_as_code(if_statement, types, printer, buffer)
        }
        BoundNodeKind::VariableDeclaration(variable_declaration) => {
            print_bound_node_variable_declaration_as_code(
                variable_declaration,
                types,
                printer,
                buffer,
            )
        }
        BoundNodeKind::WhileStatement(while_statement) => {
            print_bound_node_while_statement_as_code(while_statement, types, printer, buffer)
        }
        BoundNodeKind::MatchStatement(match_statement) => {
            print_bound_node_match_statement_as_code(match_statement, types, printer, buffer)
        }
        BoundNodeKind::Assignment(assignment) => {
            print_bound_node_assignment_as_code(assignment, types, printer, buffer)
        }
        BoundNodeKind::ExpressionStatement(expression_statement) => {
            print_bound_node_expression_statement_as_code(
                expression_statement,
                types,
                printer,
                buffer,
            )
        }
        BoundNodeKind::ReturnStatement(return_statement) => {
            print_bound_node_return_statement_as_code(return_statement, types, printer, buffer)
        }
        BoundNodeKind::Jump(jump) => print_bound_node_jump_as_code(jump, types, printer, buffer),
    }
}

fn print_bound_node_function_declaration_as_code(
    function_declaration: &BoundFunctionDeclarationNodeKind,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    if function_declaration.is_main {
        buffer.push_str("func main() ");
    } else {
        write!(
            buffer,
            "{} ",
            types.name_of_type_id(function_declaration.function_type)
        )?;
    }
    print_bound_node_as_code_with_indent(&function_declaration.body, types, printer, buffer)?;
    writeln!(buffer)?;
    Ok(())
}

fn print_bound_node_error_expression_as_code(
    _: &TypeCollection,
    _: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    buffer.push_str("#error");
    Ok(())
}

fn print_bound_node_label_as_code(
    label_address: &usize,
    _: &TypeCollection,
    _: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    buffer.push_str(&format!("L{:X}:\n", label_address));
    Ok(())
}

fn print_bound_node_label_reference_as_code(
    label_reference: &usize,
    _: &TypeCollection,
    _: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    buffer.push_str(&format!("L{:X}", label_reference));
    Ok(())
}

fn print_bound_node_literal_expression_as_code(
    literal_expression: &BoundLiteralNodeKind,
    types: &TypeCollection,
    _: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    buffer.push_str(&literal_expression.value.display(types));
    Ok(())
}

fn print_bound_node_array_literal_expression_as_code(
    array_literal_expression: &BoundArrayLiteralNodeKind,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    // FIXME: How do we print list literals?
    buffer.push_str("[ ");
    for element in &array_literal_expression.children {
        print_bound_node_as_code_with_indent(element, types, printer, buffer)?;
        buffer.push_str(", ");
    }
    buffer.push(']');
    Ok(())
}

fn print_bound_node_constructor_call_as_code(
    constructor_call: &BoundConstructorCallNodeKind,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    buffer.push_str("new ");
    buffer.push_str(&types.name_of_type_id(constructor_call.base_type));
    buffer.push('(');
    for (i, argument) in constructor_call.arguments.iter().enumerate() {
        if i != 0 {
            buffer.push_str(", ");
        }
        print_bound_node_as_code_with_indent(argument, types, printer, buffer)?;
    }
    buffer.push(')');
    Ok(())
}

fn print_bound_node_variable_expression_as_code(
    variable_expression: &BoundVariableNodeKind,
    type_: TypeId,
    types: &TypeCollection,
    _: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    buffer.push_str(&format!(
        "r#{}:{}",
        variable_expression.variable_index,
        types.name_of_type_id(type_)
    ));
    Ok(())
}

fn print_bound_node_unary_expression_as_code(
    unary_expression: &BoundUnaryNodeKind,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    buffer.push_str(&format!("{}", unary_expression.operator_token));
    print_bound_node_as_code_with_indent(&unary_expression.operand, types, printer, buffer)?;
    Ok(())
}

fn print_bound_node_binary_expression_as_code(
    binary_expression: &BoundBinaryNodeKind,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    print_bound_node_as_code_with_indent(&binary_expression.lhs, types, printer, buffer)?;
    buffer.push_str(&format!(" {} ", binary_expression.operator_token));
    print_bound_node_as_code_with_indent(&binary_expression.rhs, types, printer, buffer)?;
    Ok(())
}

fn print_bound_node_function_call_as_code(
    function_call: &BoundFunctionCallNodeKind,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    print_bound_node_as_code_with_indent(&function_call.base, types, printer, buffer)?;
    buffer.push('(');
    for argument in &function_call.arguments {
        print_bound_node_as_code_with_indent(argument, types, printer, buffer)?;
        buffer.push_str(", ");
    }
    buffer.push(')');
    Ok(())
}

fn print_bound_node_system_call_as_code(
    system_call: &BoundSystemCallNodeKind,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    buffer.push_str(&format!("{}(", system_call.base));
    for argument in &system_call.arguments {
        print_bound_node_as_code_with_indent(argument, types, printer, buffer)?;
        buffer.push_str(", ");
    }
    buffer.push(')');
    Ok(())
}

fn print_bound_node_array_index_as_code(
    array_index: &BoundArrayIndexNodeKind,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    print_bound_node_as_code_with_indent(&array_index.base, types, printer, buffer)?;
    buffer.push('[');
    print_bound_node_as_code_with_indent(&array_index.index, types, printer, buffer)?;
    buffer.push(']');
    Ok(())
}

fn print_bound_node_field_access_as_code(
    field_access: &BoundFieldAccessNodeKind,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    print_bound_node_as_code_with_indent(&field_access.base, types, printer, buffer)?;
    buffer.push_str(".field#");
    buffer.push_str(&field_access.offset.to_string());
    buffer.push(':');
    buffer.push_str(&types.name_of_type_id(field_access.type_));
    Ok(())
}

fn print_bound_node_closure_as_code(
    closure: &BoundClosureNodeKind,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    buffer.push('(');
    for (index, argument) in closure.arguments.iter().enumerate() {
        if index != 0 {
            buffer.push_str(", ");
        }
        print_bound_node_as_code_with_indent(argument, types, printer, buffer)?;
    }
    buffer.push_str(").");
    buffer.push_str(&closure.function.to_string());
    Ok(())
}

fn print_bound_node_conversion_as_code(
    conversion: &BoundConversionNodeKind,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    buffer.push('(');
    print_bound_node_as_code_with_indent(&conversion.base, types, printer, buffer)?;
    buffer.push_str(") as ");
    buffer.push_str(&types.name_of_type_id(conversion.type_));
    match conversion.kind(types) {
        Some(ConversionKind::None) => {}
        Some(kind) => write!(buffer, "[{kind:?}]")?,
        None => write!(buffer, "[ERROR]")?,
    }
    Ok(())
}

fn print_bound_node_repetition_node_as_code(
    repetition_node: &BoundRepetitionNodeKind,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    print_bound_node_as_code_with_indent(&repetition_node.expression, types, printer, buffer)?;
    write!(buffer, "; ")?;
    print_bound_node_as_code_with_indent(&repetition_node.repetition, types, printer, buffer)?;
    Ok(())
}

fn print_bound_node_block_statement_as_code(
    type_: TypeId,
    block_statement: &BoundBlockStatementNodeKind,
    types: &TypeCollection,
    mut printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    writeln!(buffer, "{{")?;
    printer.indent += 4;
    for statement in &block_statement.statements {
        printer.output_indentation(buffer);
        print_bound_node_as_code_with_indent(statement, types, printer, buffer)?;
    }
    if type_ != typeid!(Type::Void) {
        writeln!(buffer)?;
    }
    printer.indent -= 4;
    printer.output_indentation(buffer);
    write!(buffer, "}}")?;
    if type_ == typeid!(Type::Void) {
        writeln!(buffer)?;
    }
    Ok(())
}

fn print_bound_node_if_statement_as_code(
    if_statement: &BoundIfStatementNodeKind,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    buffer.push_str("if ");
    print_bound_node_as_code_with_indent(&if_statement.condition, types, printer, buffer)?;
    buffer.push(' ');
    print_bound_node_as_code_with_indent(&if_statement.body, types, printer, buffer)?;
    if let Some(else_body) = &if_statement.else_body {
        printer.output_indentation(buffer);
        buffer.push_str("else ");
        print_bound_node_as_code_with_indent(else_body, types, printer, buffer)?;
    }
    Ok(())
}

fn print_bound_node_variable_declaration_as_code(
    variable_declaration: &BoundVariableDeclarationNodeKind,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    buffer.push_str(&format!("let r#{} = ", variable_declaration.variable_index));
    print_bound_node_as_code_with_indent(
        &variable_declaration.initializer,
        types,
        printer,
        buffer,
    )?;
    buffer.push_str(";\n");
    Ok(())
}

fn print_bound_node_while_statement_as_code(
    while_statement: &BoundWhileStatementNodeKind,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    buffer.push_str("while ");
    print_bound_node_as_code_with_indent(&while_statement.condition, types, printer, buffer)?;
    buffer.push(' ');
    print_bound_node_as_code_with_indent(&while_statement.body, types, printer, buffer)?;
    Ok(())
}

fn print_bound_node_match_statement_as_code(
    match_statement: &BoundMatchStatementNodeKind,
    types: &TypeCollection,
    mut printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    printer.output_indentation(buffer);
    write!(buffer, "match ")?;
    print_bound_node_as_code_with_indent(&match_statement.expression, types, printer, buffer)?;
    writeln!(buffer, "{{")?;
    printer.indent += 4;
    for case in &match_statement.cases {
        printer.output_indentation(buffer);
        print_bound_node_match_case_expression_with_indent(
            &case.expression,
            types,
            printer,
            buffer,
        )?;
        write!(buffer, " => ")?;
        printer.indent += 4;
        print_bound_node_as_code_with_indent(&case.body, types, printer, buffer)?;
        printer.indent -= 4;
        writeln!(buffer)?;
    }
    if let Some(default_case) = &match_statement.default_case {
        printer.output_indentation(buffer);
        write!(buffer, "else => ")?;
        printer.indent += 4;
        print_bound_node_as_code_with_indent(&default_case, types, printer, buffer)?;
        printer.indent -= 4;
        writeln!(buffer)?;
    }
    printer.indent -= 4;
    printer.output_indentation(buffer);
    writeln!(buffer, "}}")?;
    Ok(())
}

fn print_bound_node_match_case_expression_with_indent(
    expression: &BoundMatchCaseExpression,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    match expression {
        BoundMatchCaseExpression::Expression(e) => {
            print_bound_node_as_code_with_indent(e, types, printer, buffer)
        }
        BoundMatchCaseExpression::Type(v, t, _) => {
            write!(buffer, "r#{v}: {}", types.name_of_type_id(*t))
        }
    }
}

fn print_bound_node_assignment_as_code(
    assignment: &BoundAssignmentNodeKind,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    print_bound_node_as_code_with_indent(&assignment.variable, types, printer, buffer)?;
    buffer.push_str(" = ");
    print_bound_node_as_code_with_indent(&assignment.expression, types, printer, buffer)?;
    buffer.push_str(";\n");
    Ok(())
}

fn print_bound_node_expression_statement_as_code(
    expression_statement: &BoundExpressionStatementNodeKind,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    print_bound_node_as_code_with_indent(&expression_statement.expression, types, printer, buffer)?;
    buffer.push_str(";\n");
    Ok(())
}

fn print_bound_node_return_statement_as_code(
    return_statement: &BoundReturnStatementNodeKind,
    types: &TypeCollection,
    printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    buffer.push_str("return");
    if let Some(expression) = &return_statement.expression {
        buffer.push(' ');
        print_bound_node_as_code_with_indent(expression, types, printer, buffer)?;
    }
    buffer.push_str(";\n");
    Ok(())
}

fn print_bound_node_jump_as_code(
    jump: &BoundJumpNodeKind,
    types: &TypeCollection,
    mut printer: DebugPrinter,
    buffer: &mut String,
) -> std::fmt::Result {
    if let Some(condition) = &jump.condition {
        write!(buffer, "jump if ")?;
        printer.indent += 4;
        print_bound_node_as_code_with_indent(condition, types, printer, buffer)?;
        printer.indent -= 4;
        printer.output_indentation(buffer);
        write!(buffer, "is {} to ", jump.jump_if_true)?;
    } else {
        write!(buffer, "jump to ")?;
    }
    if matches!(jump.target.kind, BoundNodeKind::LabelReference(_)) {
        print_bound_node_as_code_with_indent(&jump.target, types, printer, buffer)?;
    } else {
        writeln!(buffer)?;
        printer.output_indentation(buffer);
        writeln!(buffer, "{{")?;
        printer.indent += 4;
        printer.output_indentation(buffer);
        print_bound_node_as_code_with_indent(&jump.target, types, printer, buffer)?;
        printer.indent -= 4;
        writeln!(buffer)?;
        printer.output_indentation(buffer);
        write!(buffer, "}}")?;
    }
    writeln!(buffer)?;
    Ok(())
}
