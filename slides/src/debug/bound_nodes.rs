use super::DebugPrinter;
use crate::binder::{
    bound_nodes::*,
    typing::{TypeCollection, TypeId},
};
use std::fmt::Write;

struct Printer<'a, 'b> {
    printer: DebugPrinter,
    types: &'a TypeCollection,
    buffer: &'b mut String,
    print_types: bool,
}
impl Printer<'_, '_> {
    fn output_indentation(&mut self) {
        self.printer.output_indentation(self.buffer);
    }

    fn indent(&mut self) {
        self.printer.indent += 4;
    }

    fn dedent(&mut self) {
        self.printer.indent -= 4;
    }

    fn write_type(&mut self, type_: TypeId) -> std::fmt::Result {
        if !self.print_types {
            return Ok(());
        }
        write!(self.buffer, ":{}", self.types.name_of_type_id(type_))
    }
}

pub fn print_bound_node_as_code(node: &BoundNode, types: &TypeCollection) {
    let mut buffer = String::new();
    let mut printer = Printer {
        printer: DebugPrinter::default(),
        types,
        buffer: &mut buffer,
        print_types: true,
    };
    print_bound_node_as_code_with_indent(node, &mut printer).unwrap();
    print!("{}", printer.buffer);
}

pub fn bound_node_as_code_to_string(node: &BoundNode, types: &TypeCollection, buffer: &mut String) {
    let mut printer = Printer {
        printer: DebugPrinter::default(),
        types,
        buffer,
        print_types: true,
    };
    print_bound_node_as_code_with_indent(node, &mut printer).unwrap();
}

fn print_bound_node_as_code_with_indent(
    node: &BoundNode,
    printer: &mut Printer,
) -> std::fmt::Result {
    match &node.kind {
        BoundNodeKind::ErrorExpression => print_bound_node_error_expression_as_code(printer),
        BoundNodeKind::Label(label) => print_bound_node_label_as_code(label, printer),
        BoundNodeKind::LabelReference(label_reference) => {
            print_bound_node_label_reference_as_code(node.type_, label_reference, printer)
        }
        BoundNodeKind::FunctionDeclaration(function_declaration) => {
            print_bound_node_function_declaration_as_code(function_declaration, printer)
        }
        BoundNodeKind::LiteralExpression(literal_expression) => {
            print_bound_node_literal_expression_as_code(literal_expression, printer)
        }
        BoundNodeKind::ArrayLiteralExpression(array_literal_expression) => {
            print_bound_node_array_literal_expression_as_code(array_literal_expression, printer)
        }
        BoundNodeKind::ConstructorCall(constructor_call) => {
            print_bound_node_constructor_call_as_code(constructor_call, printer)
        }
        BoundNodeKind::VariableExpression(variable_expression) => {
            print_bound_node_variable_expression_as_code(variable_expression, node.type_, printer)
        }
        BoundNodeKind::UnaryExpression(unary_expression) => {
            print_bound_node_unary_expression_as_code(unary_expression, printer)
        }
        BoundNodeKind::BinaryExpression(binary_expression) => {
            print_bound_node_binary_expression_as_code(binary_expression, printer)
        }
        BoundNodeKind::FunctionCall(function_call) => {
            print_bound_node_function_call_as_code(function_call, printer)
        }
        BoundNodeKind::SystemCall(system_call) => {
            print_bound_node_system_call_as_code(system_call, printer)
        }
        BoundNodeKind::ArrayIndex(array_index) => {
            print_bound_node_array_index_as_code(node.type_, array_index, printer)
        }
        BoundNodeKind::FieldAccess(field_access) => {
            print_bound_node_field_access_as_code(node.type_, field_access, printer)
        }
        BoundNodeKind::Closure(closure) => print_bound_node_closure_as_code(closure, printer),
        BoundNodeKind::Conversion(conversion) => {
            print_bound_node_conversion_as_code(conversion, printer)
        }
        BoundNodeKind::RepetitionNode(repetition_node) => {
            print_bound_node_repetition_node_as_code(repetition_node, printer)
        }
        BoundNodeKind::BlockStatement(block_statement) => {
            print_bound_node_block_statement_as_code(node.type_, block_statement, printer)
        }
        BoundNodeKind::IfStatement(if_statement) => {
            print_bound_node_if_statement_as_code(if_statement, printer)
        }
        BoundNodeKind::VariableDeclaration(variable_declaration) => {
            print_bound_node_variable_declaration_as_code(variable_declaration, printer)
        }
        BoundNodeKind::WhileStatement(while_statement) => {
            print_bound_node_while_statement_as_code(while_statement, printer)
        }
        BoundNodeKind::MatchStatement(match_statement) => {
            print_bound_node_match_statement_as_code(match_statement, printer)
        }
        BoundNodeKind::Assignment(assignment) => {
            print_bound_node_assignment_as_code(assignment, printer)
        }
        BoundNodeKind::ExpressionStatement(expression_statement) => {
            print_bound_node_expression_statement_as_code(expression_statement, printer)
        }
        BoundNodeKind::ReturnStatement(return_statement) => {
            print_bound_node_return_statement_as_code(return_statement, printer)
        }
        BoundNodeKind::Jump(jump) => print_bound_node_jump_as_code(jump, printer),
    }
}

fn print_bound_node_function_declaration_as_code(
    function_declaration: &BoundFunctionDeclarationNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    if function_declaration.is_main {
        printer.buffer.push_str("func main() ");
    } else {
        write!(
            printer.buffer,
            "{} ",
            printer
                .types
                .name_of_type_id(function_declaration.function_type)
        )?;
    }
    print_bound_node_as_code_with_indent(&function_declaration.body, printer)?;
    writeln!(printer.buffer)?;
    Ok(())
}

fn print_bound_node_error_expression_as_code(printer: &mut Printer) -> std::fmt::Result {
    printer.buffer.push_str("#error");
    Ok(())
}

fn print_bound_node_label_as_code(
    label_address: &usize,
    printer: &mut Printer,
) -> std::fmt::Result {
    printer.buffer.push_str(&format!("L{:X}:\n", label_address));
    Ok(())
}

fn print_bound_node_label_reference_as_code(
    type_: TypeId,
    label_reference: &usize,
    printer: &mut Printer,
) -> std::fmt::Result {
    write!(printer.buffer, "L{label_reference:X}")?;
    printer.write_type(type_)?;
    Ok(())
}

fn print_bound_node_literal_expression_as_code(
    literal_expression: &BoundLiteralNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    printer
        .buffer
        .push_str(&literal_expression.value.display(printer.types));
    Ok(())
}

fn print_bound_node_array_literal_expression_as_code(
    array_literal_expression: &BoundArrayLiteralNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    // FIXME: How do we print list literals?
    printer.buffer.push_str("[ ");
    for element in &array_literal_expression.children {
        print_bound_node_as_code_with_indent(element, printer)?;
        printer.buffer.push_str(", ");
    }
    printer.buffer.push(']');
    Ok(())
}

fn print_bound_node_constructor_call_as_code(
    constructor_call: &BoundConstructorCallNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    printer.buffer.push_str("new ");
    printer
        .buffer
        .push_str(&printer.types.name_of_type_id(constructor_call.base_type));
    printer.buffer.push('(');
    for (i, argument) in constructor_call.arguments.iter().enumerate() {
        if i != 0 {
            printer.buffer.push_str(", ");
        }
        print_bound_node_as_code_with_indent(argument, printer)?;
    }
    printer.buffer.push(')');
    Ok(())
}

fn print_bound_node_variable_expression_as_code(
    variable_expression: &BoundVariableNodeKind,
    type_: TypeId,
    printer: &mut Printer,
) -> std::fmt::Result {
    write!(printer.buffer, "r#{}", variable_expression.variable_index)?;
    printer.write_type(type_)?;
    Ok(())
}

fn print_bound_node_unary_expression_as_code(
    unary_expression: &BoundUnaryNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    printer
        .buffer
        .push_str(&format!("{}", unary_expression.operator_token));
    print_bound_node_as_code_with_indent(&unary_expression.operand, printer)?;
    Ok(())
}

fn print_bound_node_binary_expression_as_code(
    binary_expression: &BoundBinaryNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    print_bound_node_as_code_with_indent(&binary_expression.lhs, printer)?;
    printer
        .buffer
        .push_str(&format!(" {} ", binary_expression.operator_token));
    print_bound_node_as_code_with_indent(&binary_expression.rhs, printer)?;
    Ok(())
}

fn print_bound_node_function_call_as_code(
    function_call: &BoundFunctionCallNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    print_bound_node_as_code_with_indent(&function_call.base, printer)?;
    printer.buffer.push('(');
    for argument in &function_call.arguments {
        print_bound_node_as_code_with_indent(argument, printer)?;
        printer.buffer.push_str(", ");
    }
    printer.buffer.push(')');
    Ok(())
}

fn print_bound_node_system_call_as_code(
    system_call: &BoundSystemCallNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    printer.buffer.push_str(&format!("{}(", system_call.base));
    for argument in &system_call.arguments {
        print_bound_node_as_code_with_indent(argument, printer)?;
        printer.buffer.push_str(", ");
    }
    printer.buffer.push(')');
    Ok(())
}

fn print_bound_node_array_index_as_code(
    type_: TypeId,
    array_index: &BoundArrayIndexNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    print_bound_node_as_code_with_indent(&array_index.base, printer)?;
    printer.buffer.push('[');
    print_bound_node_as_code_with_indent(&array_index.index, printer)?;
    printer.buffer.push(']');
    printer.write_type(type_)?;
    Ok(())
}

fn print_bound_node_field_access_as_code(
    type_: TypeId,
    field_access: &BoundFieldAccessNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    print_bound_node_as_code_with_indent(&field_access.base, printer)?;
    printer.buffer.push_str(".field#");
    printer.buffer.push_str(&field_access.offset.to_string());
    printer.write_type(type_)?;
    Ok(())
}

fn print_bound_node_closure_as_code(
    closure: &BoundClosureNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    printer.buffer.push('(');
    for (index, argument) in closure.arguments.iter().enumerate() {
        if index != 0 {
            printer.buffer.push_str(", ");
        }
        print_bound_node_as_code_with_indent(argument, printer)?;
    }
    printer.buffer.push_str(").");
    printer.buffer.push_str(&closure.function.to_string());
    Ok(())
}

fn print_bound_node_conversion_as_code(
    conversion: &BoundConversionNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    printer.buffer.push('(');
    print_bound_node_as_code_with_indent(&conversion.base, printer)?;
    printer.buffer.push_str(") as ");
    printer
        .buffer
        .push_str(&printer.types.name_of_type_id(conversion.type_));
    match conversion.kind(printer.types) {
        Some(ConversionKind::None) => {}
        Some(kind) => write!(printer.buffer, "[{kind:?}]")?,
        None => write!(printer.buffer, "[ERROR]")?,
    }
    Ok(())
}

fn print_bound_node_repetition_node_as_code(
    repetition_node: &BoundRepetitionNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    print_bound_node_as_code_with_indent(&repetition_node.expression, printer)?;
    write!(printer.buffer, "; ")?;
    print_bound_node_as_code_with_indent(&repetition_node.repetition, printer)?;
    Ok(())
}

fn print_bound_node_block_statement_as_code(
    type_: TypeId,
    block_statement: &BoundBlockStatementNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    writeln!(printer.buffer, "{{")?;
    printer.indent();
    for statement in &block_statement.statements {
        printer.output_indentation();
        print_bound_node_as_code_with_indent(statement, printer)?;
    }
    if type_ != typeid!(Type::Void) {
        writeln!(printer.buffer)?;
    }
    printer.dedent();
    printer.output_indentation();
    write!(printer.buffer, "}}")?;
    if type_ == typeid!(Type::Void) {
        writeln!(printer.buffer)?;
    }
    Ok(())
}

fn print_bound_node_if_statement_as_code(
    if_statement: &BoundIfStatementNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    printer.buffer.push_str("if ");
    print_bound_node_as_code_with_indent(&if_statement.condition, printer)?;
    printer.buffer.push(' ');
    print_bound_node_as_code_with_indent(&if_statement.body, printer)?;
    if let Some(else_body) = &if_statement.else_body {
        printer.output_indentation();
        printer.buffer.push_str("else ");
        print_bound_node_as_code_with_indent(else_body, printer)?;
    }
    Ok(())
}

fn print_bound_node_variable_declaration_as_code(
    variable_declaration: &BoundVariableDeclarationNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    printer
        .buffer
        .push_str(&format!("let r#{} = ", variable_declaration.variable_index));
    print_bound_node_as_code_with_indent(&variable_declaration.initializer, printer)?;
    printer.buffer.push_str(";\n");
    Ok(())
}

fn print_bound_node_while_statement_as_code(
    while_statement: &BoundWhileStatementNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    printer.buffer.push_str("while ");
    print_bound_node_as_code_with_indent(&while_statement.condition, printer)?;
    printer.buffer.push(' ');
    print_bound_node_as_code_with_indent(&while_statement.body, printer)?;
    Ok(())
}

fn print_bound_node_match_statement_as_code(
    match_statement: &BoundMatchStatementNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    printer.output_indentation();
    write!(printer.buffer, "match ")?;
    print_bound_node_as_code_with_indent(&match_statement.expression, printer)?;
    writeln!(printer.buffer, "{{")?;
    printer.indent();
    for case in &match_statement.cases {
        printer.output_indentation();
        print_bound_node_match_case_expression_with_indent(&case.expression, printer)?;
        write!(printer.buffer, " => ")?;
        printer.indent();
        print_bound_node_as_code_with_indent(&case.body, printer)?;
        printer.dedent();
        writeln!(printer.buffer)?;
    }
    if let Some(default_case) = &match_statement.default_case {
        printer.output_indentation();
        write!(printer.buffer, "else => ")?;
        printer.indent();
        print_bound_node_as_code_with_indent(&default_case, printer)?;
        printer.dedent();
        writeln!(printer.buffer)?;
    }
    printer.dedent();
    printer.output_indentation();
    writeln!(printer.buffer, "}}")?;
    Ok(())
}

fn print_bound_node_match_case_expression_with_indent(
    expression: &BoundMatchCaseExpression,
    printer: &mut Printer,
) -> std::fmt::Result {
    match expression {
        BoundMatchCaseExpression::Expression(e) => print_bound_node_as_code_with_indent(e, printer),
        BoundMatchCaseExpression::Type(v, t, _) => {
            write!(
                printer.buffer,
                "r#{v}: {}",
                printer.types.name_of_type_id(*t)
            )
        }
    }
}

fn print_bound_node_assignment_as_code(
    assignment: &BoundAssignmentNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    print_bound_node_as_code_with_indent(&assignment.variable, printer)?;
    printer.buffer.push_str(" = ");
    print_bound_node_as_code_with_indent(&assignment.expression, printer)?;
    printer.buffer.push_str(";\n");
    Ok(())
}

fn print_bound_node_expression_statement_as_code(
    expression_statement: &BoundExpressionStatementNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    print_bound_node_as_code_with_indent(&expression_statement.expression, printer)?;
    printer.buffer.push_str(";\n");
    Ok(())
}

fn print_bound_node_return_statement_as_code(
    return_statement: &BoundReturnStatementNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    printer.buffer.push_str("return");
    if let Some(expression) = &return_statement.expression {
        printer.buffer.push(' ');
        print_bound_node_as_code_with_indent(expression, printer)?;
    }
    printer.buffer.push_str(";\n");
    Ok(())
}

fn print_bound_node_jump_as_code(
    jump: &BoundJumpNodeKind,
    printer: &mut Printer,
) -> std::fmt::Result {
    if let Some(condition) = &jump.condition {
        write!(printer.buffer, "jump if ")?;
        printer.indent();
        print_bound_node_as_code_with_indent(condition, printer)?;
        printer.dedent();
        printer.output_indentation();
        write!(printer.buffer, "is {} to ", jump.jump_if_true)?;
    } else {
        write!(printer.buffer, "jump to ")?;
    }
    if matches!(jump.target.kind, BoundNodeKind::LabelReference(_)) {
        print_bound_node_as_code_with_indent(&jump.target, printer)?;
    } else {
        writeln!(printer.buffer)?;
        printer.output_indentation();
        writeln!(printer.buffer, "{{")?;
        printer.indent();
        printer.output_indentation();
        print_bound_node_as_code_with_indent(&jump.target, printer)?;
        printer.dedent();
        writeln!(printer.buffer)?;
        printer.output_indentation();
        write!(printer.buffer, "}}")?;
    }
    writeln!(printer.buffer)?;
    Ok(())
}
