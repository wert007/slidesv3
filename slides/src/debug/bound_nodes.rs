use crate::binder::bound_nodes::*;

use super::DebugPrinter;

pub fn print_bound_node_as_code(node: &BoundNode) {
    let mut buffer = String::new();
    print_bound_node_as_code_with_indent(node, DebugPrinter::default(), &mut buffer);
    print!("{}", buffer);
}

pub fn bound_node_as_code_to_string(node: &BoundNode, buffer: &mut String) {
    print_bound_node_as_code_with_indent(node, DebugPrinter::default(), buffer);
}

fn print_bound_node_as_code_with_indent(
    node: &BoundNode,
    printer: DebugPrinter,
    buffer: &mut String,
) {
    match &node.kind {
        BoundNodeKind::ErrorExpression => {
            print_bound_node_error_expression_as_code(printer, buffer)
        }
        BoundNodeKind::Label(label) => print_bound_node_label_as_code(label, printer, buffer),
        BoundNodeKind::LabelReference(label_reference) => {
            print_bound_node_label_reference_as_code(label_reference, printer, buffer)
        }
        BoundNodeKind::FunctionDeclaration(function_declaration) => {
            print_bound_node_function_declaration_as_code(function_declaration, printer, buffer)
        }
        BoundNodeKind::LiteralExpression(literal_expression) => {
            print_bound_node_literal_expression_as_code(literal_expression, printer, buffer)
        }
        BoundNodeKind::ArrayLiteralExpression(array_literal_expression) => {
            print_bound_node_array_literal_expression_as_code(
                array_literal_expression,
                printer,
                buffer,
            )
        }
        BoundNodeKind::ConstructorCall(constructor_call) => {
            print_bound_node_constructor_call_as_code(constructor_call, printer, buffer)
        }
        BoundNodeKind::VariableExpression(variable_expression) => {
            print_bound_node_variable_expression_as_code(variable_expression, printer, buffer)
        }
        BoundNodeKind::UnaryExpression(unary_expression) => {
            print_bound_node_unary_expression_as_code(unary_expression, printer, buffer)
        }
        BoundNodeKind::BinaryExpression(binary_expression) => {
            print_bound_node_binary_expression_as_code(binary_expression, printer, buffer)
        }
        BoundNodeKind::FunctionCall(function_call) => {
            print_bound_node_function_call_as_code(function_call, printer, buffer)
        }
        BoundNodeKind::SystemCall(system_call) => {
            print_bound_node_system_call_as_code(system_call, printer, buffer)
        }
        BoundNodeKind::ArrayIndex(array_index) => {
            print_bound_node_array_index_as_code(array_index, printer, buffer)
        }
        BoundNodeKind::FieldAccess(field_access) => {
            print_bound_node_field_access_as_code(field_access, printer, buffer)
        }
        BoundNodeKind::Closure(closure) => {
            print_bound_node_closure_as_code(closure, printer, buffer)
        }
        BoundNodeKind::Conversion(conversion) => {
            print_bound_node_conversion_as_code(conversion, printer, buffer)
        }
        BoundNodeKind::BlockStatement(block_statement) => {
            print_bound_node_block_statement_as_code(block_statement, printer, buffer)
        }
        BoundNodeKind::IfStatement(if_statement) => {
            print_bound_node_if_statement_as_code(if_statement, printer, buffer)
        }
        BoundNodeKind::VariableDeclaration(variable_declaration) => {
            print_bound_node_variable_declaration_as_code(variable_declaration, printer, buffer)
        }
        BoundNodeKind::WhileStatement(while_statement) => {
            print_bound_node_while_statement_as_code(while_statement, printer, buffer)
        }
        BoundNodeKind::Assignment(assignment) => {
            print_bound_node_assignment_as_code(assignment, printer, buffer)
        }
        BoundNodeKind::ExpressionStatement(expression_statement) => {
            print_bound_node_expression_statement_as_code(expression_statement, printer, buffer)
        }
        BoundNodeKind::ReturnStatement(return_statement) => {
            print_bound_node_return_statement_as_code(return_statement, printer, buffer)
        }
        BoundNodeKind::Jump(jump) => print_bound_node_jump_as_code(jump, printer, buffer),
    }
}

fn print_bound_node_function_declaration_as_code(
    function_declaration: &BoundFunctionDeclarationNodeKind,
    printer: DebugPrinter,
    buffer: &mut String,
) {
    if function_declaration.is_main {
        buffer.push_str("fn main() ");
    } else {
        buffer.push_str(&format!("fn f{}(...) -> ... ", function_declaration.label));
    }
    print_bound_node_as_code_with_indent(&function_declaration.body, printer, buffer);
}

fn print_bound_node_error_expression_as_code(_: DebugPrinter, buffer: &mut String) {
    buffer.push_str("#error");
}

fn print_bound_node_label_as_code(label_address: &usize, _: DebugPrinter, buffer: &mut String) {
    buffer.push_str(&format!("l#{}:\n", label_address));
}

fn print_bound_node_label_reference_as_code(
    label_reference: &usize,
    _: DebugPrinter,
    buffer: &mut String,
) {
    buffer.push_str(&format!("l#{}", label_reference));
}

fn print_bound_node_literal_expression_as_code(
    literal_expression: &BoundLiteralNodeKind,
    _: DebugPrinter,
    buffer: &mut String,
) {
    buffer.push_str(&format!("{}", literal_expression.value));
}

fn print_bound_node_array_literal_expression_as_code(
    array_literal_expression: &BoundArrayLiteralNodeKind,
    printer: DebugPrinter,
    buffer: &mut String,
) {
    buffer.push_str("[ ");
    for element in &array_literal_expression.children {
        print_bound_node_as_code_with_indent(element, printer, buffer);
        buffer.push_str(", ");
    }
    buffer.push(']');
}

fn print_bound_node_constructor_call_as_code(
    constructor_call: &BoundConstructorCallNodeKind,
    printer: DebugPrinter,
    buffer: &mut String,
) {
    buffer.push_str("new ");
    buffer.push_str(&constructor_call.base_type.to_string());
    buffer.push('(');
    for (i, argument) in constructor_call.arguments.iter().enumerate() {
        if i != 0 {
            buffer.push_str(", ");
        }
        print_bound_node_as_code_with_indent(argument, printer, buffer);
    }
    buffer.push(')');
}

fn print_bound_node_variable_expression_as_code(
    variable_expression: &BoundVariableNodeKind,
    _: DebugPrinter,
    buffer: &mut String,
) {
    buffer.push_str(&format!("r#{}", variable_expression.variable_index));
}

fn print_bound_node_unary_expression_as_code(
    unary_expression: &BoundUnaryNodeKind,
    printer: DebugPrinter,
    buffer: &mut String,
) {
    buffer.push_str(&format!("{}", unary_expression.operator_token));
    print_bound_node_as_code_with_indent(&unary_expression.operand, printer, buffer);
}

fn print_bound_node_binary_expression_as_code(
    binary_expression: &BoundBinaryNodeKind,
    printer: DebugPrinter,
    buffer: &mut String,
) {
    print_bound_node_as_code_with_indent(&binary_expression.lhs, printer, buffer);
    buffer.push_str(&format!(" {} ", binary_expression.operator_token));
    print_bound_node_as_code_with_indent(&binary_expression.rhs, printer, buffer);
}

fn print_bound_node_function_call_as_code(
    function_call: &BoundFunctionCallNodeKind,
    printer: DebugPrinter,
    buffer: &mut String,
) {
    print_bound_node_as_code_with_indent(&function_call.base, printer, buffer);
    buffer.push('(');
    for argument in &function_call.arguments {
        print_bound_node_as_code_with_indent(argument, printer, buffer);
        buffer.push_str(", ");
    }
    buffer.push(')');
}

fn print_bound_node_system_call_as_code(
    system_call: &BoundSystemCallNodeKind,
    printer: DebugPrinter,
    buffer: &mut String,
) {
    buffer.push_str(&format!("{}(", system_call.base));
    for argument in &system_call.arguments {
        print_bound_node_as_code_with_indent(argument, printer, buffer);
        buffer.push_str(", ");
    }
    buffer.push(')');
}

fn print_bound_node_array_index_as_code(
    array_index: &BoundArrayIndexNodeKind,
    printer: DebugPrinter,
    buffer: &mut String,
) {
    print_bound_node_as_code_with_indent(&array_index.base, printer, buffer);
    buffer.push('[');
    print_bound_node_as_code_with_indent(&array_index.index, printer, buffer);
    buffer.push(']');
}

fn print_bound_node_field_access_as_code(
    field_access: &BoundFieldAccessNodeKind,
    printer: DebugPrinter,
    buffer: &mut String,
) {
    print_bound_node_as_code_with_indent(&field_access.base, printer, buffer);
    buffer.push_str(".field#");
    buffer.push_str(&field_access.offset.to_string());
    buffer.push(':');
    buffer.push_str(&field_access.type_.to_string());
}

fn print_bound_node_closure_as_code(
    closure: &BoundClosureNodeKind,
    printer: DebugPrinter,
    buffer: &mut String,
) {
    buffer.push('(');
    for (index, argument) in closure.arguments.iter().enumerate() {
        if index != 0 {
            buffer.push_str(", ");
        }
        print_bound_node_as_code_with_indent(argument, printer, buffer);

    }
    buffer.push_str(").");
    buffer.push_str(&closure.function.to_string());
}

fn print_bound_node_conversion_as_code(
    closure: &BoundConversionNodeKind,
    printer: DebugPrinter,
    buffer: &mut String,
) {
    buffer.push('(');
    print_bound_node_as_code_with_indent(&closure.base, printer, buffer);
    buffer.push_str(") as ");
    buffer.push_str(&closure.type_.to_string());
}

fn print_bound_node_block_statement_as_code(
    block_statement: &BoundBlockStatementNodeKind,
    mut printer: DebugPrinter,
    buffer: &mut String,
) {
    buffer.push_str("{\n");
    printer.indent += 4;
    for statement in &block_statement.statements {
        printer.print_indentation();
        print_bound_node_as_code_with_indent(statement, printer, buffer);
    }
    printer.indent -= 4;
    printer.print_indentation();
    buffer.push_str("}\n");
}

fn print_bound_node_if_statement_as_code(
    if_statement: &BoundIfStatementNodeKind,
    printer: DebugPrinter,
    buffer: &mut String,
) {
    buffer.push_str("if ");
    print_bound_node_as_code_with_indent(&if_statement.condition, printer, buffer);
    buffer.push(' ');
    print_bound_node_as_code_with_indent(&if_statement.body, printer, buffer);
    if let Some(else_body) = &if_statement.else_body {
        printer.print_indentation();
        buffer.push_str("else ");
        print_bound_node_as_code_with_indent(else_body, printer, buffer);
    }
}

fn print_bound_node_variable_declaration_as_code(
    variable_declaration: &BoundVariableDeclarationNodeKind,
    printer: DebugPrinter,
    buffer: &mut String,
) {
    buffer.push_str(&format!("let r#{} = ", variable_declaration.variable_index));
    print_bound_node_as_code_with_indent(&variable_declaration.initializer, printer, buffer);
    buffer.push_str(";\n");
}

fn print_bound_node_while_statement_as_code(
    while_statement: &BoundWhileStatementNodeKind,
    printer: DebugPrinter,
    buffer: &mut String,
) {
    buffer.push_str("while ");
    print_bound_node_as_code_with_indent(&while_statement.condition, printer, buffer);
    buffer.push(' ');
    print_bound_node_as_code_with_indent(&while_statement.body, printer, buffer);
}

fn print_bound_node_assignment_as_code(
    assignment: &BoundAssignmentNodeKind,
    printer: DebugPrinter,
    buffer: &mut String,
) {
    print_bound_node_as_code_with_indent(&assignment.variable, printer, buffer);
    buffer.push_str(" = ");
    print_bound_node_as_code_with_indent(&assignment.expression, printer, buffer);
    buffer.push_str(";\n");
}

fn print_bound_node_expression_statement_as_code(
    expression_statement: &BoundExpressionStatementNodeKind,
    printer: DebugPrinter,
    buffer: &mut String,
) {
    print_bound_node_as_code_with_indent(&expression_statement.expression, printer, buffer);
    buffer.push_str(";\n");
}

fn print_bound_node_return_statement_as_code(
    return_statement: &BoundReturnStatementNodeKind,
    printer: DebugPrinter,
    buffer: &mut String,
) {
    buffer.push_str("return");
    if let Some(expression) = &return_statement.expression {
        buffer.push(' ');
        print_bound_node_as_code_with_indent(expression, printer, buffer);
    }
    buffer.push_str(";\n");
}

fn print_bound_node_jump_as_code(
    jump: &BoundJumpNodeKind,
    printer: DebugPrinter,
    buffer: &mut String,
) {
    if let Some(condition) = &jump.condition {
        buffer.push_str("jump if ");
        print_bound_node_as_code_with_indent(condition, printer, buffer);
        buffer.push_str(&format!(" is {} to  ", jump.jump_if_true));
    } else {
        buffer.push_str("jump to ");
    }
    print_bound_node_as_code_with_indent(&jump.target, printer, buffer);
    buffer.push('\n');
}
