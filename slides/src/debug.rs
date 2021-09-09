use crate::{binder::bound_nodes::*, parser::syntax_nodes::LiteralNodeKind};

#[derive(Debug, Clone, Copy)]
pub struct DebugFlags {
    pub print_instructions: bool,
    pub print_current_instruction: bool,
    pub print_variable_table: bool,
    pub print_tokens: bool,
}

impl Default for DebugFlags {
    fn default() -> Self {
        Self {
            print_instructions: false,
            print_variable_table: false,
            print_current_instruction: false,
            print_tokens: false,
        }
    }
}

impl DebugFlags {
    /// Get a reference to the debug flags's print instructions.
    pub fn print_instructions(&self) -> bool {
        self.print_instructions
    }
    /// Get a reference to the debug flags's print current instruction.
    pub fn print_current_instruction(&self) -> bool {
        self.print_instructions
    }
    /// Get a reference to the debug flags's print variable table.
    pub fn print_variable_table(&self) -> bool {
        self.print_variable_table
    }

    /// Get a reference to the debug flags's print tokens.
    pub fn print_tokens(&self) -> bool {
        self.print_tokens
    }
}

#[derive(Clone, Copy, Debug, Default)]
struct DebugPrinter {
    indent: usize,
}

impl DebugPrinter {
    fn print_indentation(&self) {
        for _ in 0..self.indent {
            print!(" ");
        }
    }
}


pub fn print_bound_node_as_code(node: &BoundNode) {
    print_bound_node_as_code_with_indent(node, DebugPrinter::default())
}

fn print_bound_node_as_code_with_indent(node: &BoundNode, printer: DebugPrinter) {
    match &node.kind {
        BoundNodeKind::ErrorExpression => print_bound_node_error_expression_as_code(printer),
        BoundNodeKind::LiteralExpression(literal_expression) => print_bound_node_literal_expression_as_code(literal_expression, printer),
        BoundNodeKind::ArrayLiteralExpression(array_literal_expression) => print_bound_node_array_literal_expression_as_code(array_literal_expression, printer),
        BoundNodeKind::VariableExpression(variable_expression) => print_bound_node_variable_expression_as_code(variable_expression, printer),
        BoundNodeKind::UnaryExpression(unary_expression) => print_bound_node_unary_expression_as_code(unary_expression, printer),
        BoundNodeKind::BinaryExpression(binary_expression) => print_bound_node_binary_expression_as_code(binary_expression, printer),
        BoundNodeKind::_FunctionCall(function_call) => print_bound_node_function_call_as_code(function_call, printer),
        BoundNodeKind::SystemCall(system_call) => print_bound_node_system_call_as_code(system_call, printer),
        BoundNodeKind::ArrayIndex(array_index) => print_bound_node_array_index_as_code(array_index, printer),
        BoundNodeKind::FieldAccess(field_access) => print_bound_node_field_access_as_code(field_access, printer),
        BoundNodeKind::BlockStatement(block_statement) => print_bound_node_block_statement_as_code(block_statement, printer),
        BoundNodeKind::IfStatement(if_statement) => print_bound_node_if_statement_as_code(if_statement, printer),
        BoundNodeKind::VariableDeclaration(variable_declaration) => print_bound_node_variable_declaration_as_code(variable_declaration, printer),
        BoundNodeKind::WhileStatement(while_statement) => print_bound_node_while_statement_as_code(while_statement, printer),
        BoundNodeKind::Assignment(assignment) => print_bound_node_assignment_as_code(assignment, printer),
        BoundNodeKind::ExpressionStatement(expression_statement) => print_bound_node_expression_statement_as_code(expression_statement, printer),
    }
}

fn print_bound_node_error_expression_as_code(_: DebugPrinter) {
    print!("#error");
}

fn print_bound_node_literal_expression_as_code(literal_expression: &LiteralNodeKind, _: DebugPrinter) {
    print!("{}", literal_expression.value);
}

fn print_bound_node_array_literal_expression_as_code(array_literal_expression: &BoundArrayLiteralNodeKind, printer: DebugPrinter) {
    print!("[ ");
    for element in &array_literal_expression.children {
        print_bound_node_as_code_with_indent(element, printer);
        print!(", ");
    }
    print!("]");
}

fn print_bound_node_variable_expression_as_code(variable_expression: &BoundVariableNodeKind, _: DebugPrinter) {
    print!("r#{}", variable_expression.variable_index);
}

fn print_bound_node_unary_expression_as_code(unary_expression: &BoundUnaryNodeKind, printer: DebugPrinter) {
    print!("{}", unary_expression.operator_token);
    print_bound_node_as_code_with_indent(&unary_expression.operand, printer);
}

fn print_bound_node_binary_expression_as_code(binary_expression: &BoundBinaryNodeKind, printer: DebugPrinter) {
    print_bound_node_as_code_with_indent(&binary_expression.lhs, printer);
    print!(" {} ", binary_expression.operator_token);
    print_bound_node_as_code_with_indent(&binary_expression.rhs, printer);
}

fn print_bound_node_function_call_as_code(function_call: &BoundFunctionCallNodeKind, printer: DebugPrinter) {
    print_bound_node_as_code_with_indent(&function_call.base, printer);
    print!("(");
    for argument in &function_call.arguments {
        print_bound_node_as_code_with_indent(argument, printer);
        print!(", ");
    }
    print!(")");
}

fn print_bound_node_system_call_as_code(system_call: &BoundSystemCallNodeKind, printer: DebugPrinter) {
    print!("{}(", system_call.base);
    for argument in &system_call.arguments {
        print_bound_node_as_code_with_indent(argument, printer);
        print!(", ");
    }
    print!(")");
}

fn print_bound_node_array_index_as_code(array_index: &BoundArrayIndexNodeKind, printer: DebugPrinter) {
    print_bound_node_as_code_with_indent(&array_index.base, printer);
    print!("[");
    print_bound_node_as_code_with_indent(&array_index.index, printer);
    print!("]");
}

fn print_bound_node_field_access_as_code(field_access: &BoundFieldAccessNodeKind, printer: DebugPrinter) {
    print_bound_node_as_code_with_indent(&field_access.base, printer);
    print!(".length");
}

fn print_bound_node_block_statement_as_code(block_statement: &BoundBlockStatementNodeKind, mut printer: DebugPrinter) {
    println!("{{");
    printer.indent += 4;
    for statement in &block_statement.statements {
        printer.print_indentation();
        print_bound_node_as_code_with_indent(statement, printer);
    }
    printer.indent -= 4;
    printer.print_indentation();
    println!("}}");
}

fn print_bound_node_if_statement_as_code(if_statement: &BoundIfStatementNodeKind, printer: DebugPrinter) {
    print!("if ");
    print_bound_node_as_code_with_indent(&if_statement.condition, printer);
    print!(" ");
    print_bound_node_as_code_with_indent(&if_statement.body, printer);
}

fn print_bound_node_variable_declaration_as_code(variable_declaration: &BoundVariableDeclarationNodeKind, printer: DebugPrinter) {
    print!("let r#{} = ",variable_declaration.variable_index);
    print_bound_node_as_code_with_indent(&variable_declaration.initializer, printer);
    println!(";");
}

fn print_bound_node_while_statement_as_code(while_statement: &BoundWhileStatementNodeKind, printer: DebugPrinter) {
    print!("while ");
    print_bound_node_as_code_with_indent(&while_statement.condition, printer);
    print!(" ");
    print_bound_node_as_code_with_indent(&while_statement.body, printer);
}

fn print_bound_node_assignment_as_code(assignment: &BoundAssignmentNodeKind, printer: DebugPrinter) {
    print_bound_node_as_code_with_indent(&assignment.variable, printer);
    print!(" = ");
    print_bound_node_as_code_with_indent(&assignment.expression, printer);
    println!(";");
}

fn print_bound_node_expression_statement_as_code(expression_statement: &BoundExpressionStatementNodeKind, printer: DebugPrinter) {
    print_bound_node_as_code_with_indent(&expression_statement.expression, printer);
    println!(";");
}