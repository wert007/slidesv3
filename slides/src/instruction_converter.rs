pub mod instruction;
#[cfg(test)]
mod tests;

use crate::{binder::{self, bound_nodes::{BoundArrayIndexNodeKind, BoundArrayLiteralNodeKind, BoundAssignmentNodeKind, BoundBinaryNodeKind, BoundBlockStatementNodeKind, BoundExpressionStatementNodeKind, BoundFunctionCallNodeKind, BoundIfStatementNodeKind, BoundNode, BoundNodeKind, BoundSystemCallNodeKind, BoundUnaryNodeKind, BoundVariableDeclarationNodeKind, BoundVariableNodeKind, BoundWhileStatementNodeKind}, operators::{BoundBinaryOperator, BoundUnaryOperator}, typing::Type}, debug::DebugFlags, diagnostics::DiagnosticBag, parser::syntax_nodes::LiteralNodeKind, text::SourceText, value::Value};

use self::instruction::Instruction;

pub fn convert<'a>(
    source_text: &'a SourceText<'a>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
    debug_flags: DebugFlags,
) -> Vec<Instruction> {
    let bound_node = binder::bind(source_text, diagnostic_bag, debug_flags);
    if diagnostic_bag.has_errors() {
        return vec![];
    }

    let result = convert_node(bound_node, diagnostic_bag);
    if debug_flags.print_instructions() {
        for (i, instruction) in result.iter().enumerate() {
            println!("  {:000}: {:?}", i, instruction);
        }
    }
    result
}

fn convert_node(node: BoundNode, diagnostic_bag: &mut DiagnosticBag) -> Vec<Instruction> {
    match node.kind {
        BoundNodeKind::ErrorExpression => unreachable!(),
        BoundNodeKind::LiteralExpression(literal) => convert_literal(literal, diagnostic_bag),
        BoundNodeKind::ArrayLiteralExpression(array_literal) => convert_array_literal(array_literal, diagnostic_bag),
        BoundNodeKind::VariableExpression(variable) => convert_variable(variable, diagnostic_bag),
        BoundNodeKind::UnaryExpression(unary) => convert_unary(unary, diagnostic_bag),
        BoundNodeKind::BinaryExpression(binary) => convert_binary(binary, diagnostic_bag),
        BoundNodeKind::FunctionCall(function_call) => {
            convert_function_call(function_call, diagnostic_bag)
        }
        BoundNodeKind::SystemCall(system_call) => convert_system_call(system_call, diagnostic_bag),
        BoundNodeKind::ArrayIndex(array_index) => {
            convert_array_index(array_index, diagnostic_bag)
        }
        
        BoundNodeKind::BlockStatement(block_statement) => {
            convert_block_statement(block_statement, diagnostic_bag)
        }
        BoundNodeKind::IfStatement(if_statement) => {
            convert_if_statement(if_statement, diagnostic_bag)
        }
        BoundNodeKind::VariableDeclaration(variable_declaration) => {
            convert_variable_declaration(variable_declaration, diagnostic_bag)
        }
        BoundNodeKind::WhileStatement(while_statement) => {
            convert_while_statement(while_statement, diagnostic_bag)
        }
        BoundNodeKind::Assignment(assignment) => convert_assignment(assignment, diagnostic_bag),
        BoundNodeKind::ExpressionStatement(expression_statement) => {
            convert_expression_statement(expression_statement, diagnostic_bag)
        }
    }
}

fn convert_node_for_assignment(
    node: BoundNode,
    diagnostic_bag: &mut DiagnosticBag,
) -> Vec<Instruction> {
    match node.kind {
        BoundNodeKind::VariableExpression(variable) => {
            convert_variable_for_assignment(variable, diagnostic_bag)
        }
        BoundNodeKind::ArrayIndex(array_index) => {
            convert_array_index_for_assignment(array_index, diagnostic_bag)
        }
        _ => unreachable!(),
    }
}

fn convert_literal(literal: LiteralNodeKind, _: &mut DiagnosticBag) -> Vec<Instruction> {
    let value = match literal.value {
        Value::Integer(value) => value as u64,
        Value::Boolean(value) => {
            if value {
                1
            } else {
                0
            }
        }
        Value::SystemCall(kind) => kind as u64,
    };
    vec![Instruction::load_immediate(value)]
}

fn convert_array_literal(array_literal: BoundArrayLiteralNodeKind, diagnostic_bag: &mut DiagnosticBag) -> Vec<Instruction> {
    let mut result = vec![];
    let count_in_bytes = array_literal.children.len() * 4;
    result.push(Instruction::load_immediate(count_in_bytes as u64));
    for child in array_literal.children {
        result.append(&mut convert_node(child, diagnostic_bag));
    }
    result.push(Instruction::array_length(count_in_bytes));
    result
}

fn convert_variable(variable: BoundVariableNodeKind, _: &mut DiagnosticBag) -> Vec<Instruction> {
    vec![Instruction::load_register(variable.variable_index)]
}

fn convert_variable_for_assignment(
    variable: BoundVariableNodeKind,
    _: &mut DiagnosticBag,
) -> Vec<Instruction> {
    vec![Instruction::store_in_register(variable.variable_index)]
}

fn convert_array_index_for_assignment(
    array_index: BoundArrayIndexNodeKind,
    diagnostic_bag: &mut DiagnosticBag,
) -> Vec<Instruction> {
    let mut result = convert_node(*array_index.base, diagnostic_bag);
    result.append(&mut convert_node(*array_index.index, diagnostic_bag));
    result.push(Instruction::store_in_memory());
    result
}

fn convert_unary(
    unary: BoundUnaryNodeKind,
    diagnostic_bag: &mut DiagnosticBag,
) -> Vec<Instruction> {
    let mut result = convert_node(*unary.operand, diagnostic_bag);
    match unary.operator_token {
        BoundUnaryOperator::ArithmeticNegate => result.push(Instruction::twos_complement()),
        BoundUnaryOperator::ArithmeticIdentity => {}
    }
    result
}

fn convert_binary(
    binary: BoundBinaryNodeKind,
    diagnostic_bag: &mut DiagnosticBag,
) -> Vec<Instruction> {
    let mut result = convert_node(*binary.lhs, diagnostic_bag);
    result.append(&mut convert_node(*binary.rhs, diagnostic_bag));
    result.push(match binary.operator_token {
        BoundBinaryOperator::ArithmeticAddition => Instruction::addition(),
        BoundBinaryOperator::ArithmeticSubtraction => Instruction::subtraction(),
        BoundBinaryOperator::ArithmeticMultiplication => Instruction::multiplication(),
        BoundBinaryOperator::ArithmeticDivision => Instruction::division(),
        BoundBinaryOperator::Equals => Instruction::equals(),
        BoundBinaryOperator::NotEquals => Instruction::not_equals(),
        BoundBinaryOperator::LessThan => Instruction::less_than(),
        BoundBinaryOperator::GreaterThan => Instruction::greater_than(),
        BoundBinaryOperator::LessThanEquals => Instruction::less_than_equals(),
        BoundBinaryOperator::GreaterThanEquals => Instruction::greater_than_equals(),
    });
    result
}

fn convert_function_call(
    _function_call: BoundFunctionCallNodeKind,
    _diagnostic_bag: &mut DiagnosticBag,
) -> Vec<Instruction> {
    todo!()
}

fn convert_system_call(
    system_call: BoundSystemCallNodeKind,
    diagnostic_bag: &mut DiagnosticBag,
) -> Vec<Instruction> {
    let mut result = vec![];
    let argument_count = system_call.arguments.len();
    for argument in system_call.arguments {
        result.append(&mut convert_node(argument, diagnostic_bag));
    }
    result.push(Instruction::system_call(system_call.base, argument_count));
    result
}

fn convert_array_index(
    array_index: BoundArrayIndexNodeKind,
    diagnostic_bag: &mut DiagnosticBag,
) -> Vec<Instruction> {
    let mut result = convert_node(*array_index.base, diagnostic_bag);
    result.append(&mut convert_node(*array_index.index, diagnostic_bag));
    result.push(Instruction::array_index());
    result
}

fn convert_if_statement(
    if_statement: BoundIfStatementNodeKind,
    diagnostic_bag: &mut DiagnosticBag,
) -> Vec<Instruction> {
    let mut result = convert_node(*if_statement.condition, diagnostic_bag);
    let mut body = convert_node(*if_statement.body, diagnostic_bag);
    let jmp = Instruction::jump_if_false(body.len() as i64);
    result.push(jmp);
    result.append(&mut body);
    result
}

fn convert_variable_declaration(
    variable_declaration: BoundVariableDeclarationNodeKind,
    diagnostic_bag: &mut DiagnosticBag,
) -> Vec<Instruction> {
    let mut result = convert_node(*variable_declaration.initializer, diagnostic_bag);
    result.push(Instruction::store_in_register(
        variable_declaration.variable_index,
    ));
    result
}

fn convert_while_statement(
    while_statement: BoundWhileStatementNodeKind,
    diagnostic_bag: &mut DiagnosticBag,
) -> Vec<Instruction> {
    let mut result = convert_node(*while_statement.condition, diagnostic_bag);
    let mut body = convert_node(*while_statement.body, diagnostic_bag);
    let jmp = Instruction::jump_if_false(body.len() as i64 + 1);
    result.push(jmp);
    result.append(&mut body);
    let jmp = Instruction::jump_relative(-(result.len() as i64 + 1));
    result.push(jmp);
    result
}

fn convert_assignment(
    assignment: BoundAssignmentNodeKind,
    diagnostic_bag: &mut DiagnosticBag,
) -> Vec<Instruction> {
    let mut result = convert_node(*assignment.expression, diagnostic_bag);
    result.append(&mut convert_node_for_assignment(
        *assignment.variable,
        diagnostic_bag,
    ));
    result
}

fn convert_block_statement(
    block_statement: BoundBlockStatementNodeKind,
    diagnostic_bag: &mut DiagnosticBag,
) -> Vec<Instruction> {
    let mut result = vec![];
    for node in block_statement.statements {
        result.append(&mut convert_node(node, diagnostic_bag));
    }
    result
}

fn convert_expression_statement(
    expression_statement: BoundExpressionStatementNodeKind,
    diagnostic_bag: &mut DiagnosticBag,
) -> Vec<Instruction> {
    let pushes_on_stack = !matches!(expression_statement.expression.type_, Type::Void);
    let mut result = convert_node(*expression_statement.expression, diagnostic_bag);
    if pushes_on_stack {
        result.push(Instruction::pop());
    }
    result
}
