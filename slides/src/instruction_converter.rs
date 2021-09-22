pub mod instruction;
#[cfg(test)]
mod tests;

mod label_replacer;

use crate::{binder::{self, bound_nodes::{BoundArrayIndexNodeKind, BoundArrayLiteralNodeKind, BoundAssignmentNodeKind, BoundBinaryNodeKind, BoundBlockStatementNodeKind, BoundExpressionStatementNodeKind, BoundFieldAccessNodeKind, BoundFunctionCallNodeKind, BoundFunctionDeclarationNodeKind, BoundIfStatementNodeKind, BoundNode, BoundNodeKind, BoundReturnStatementNodeKind, BoundSystemCallNodeKind, BoundUnaryNodeKind, BoundVariableDeclarationNodeKind, BoundVariableNodeKind, BoundWhileStatementNodeKind}, operators::{BoundBinaryOperator, BoundUnaryOperator}, typing::{SystemCallKind, Type}}, debug::DebugFlags, diagnostics::DiagnosticBag, evaluator::{WORD_SIZE_IN_BYTES, bytes_to_word}, parser::syntax_nodes::LiteralNodeKind, text::SourceText, value::Value};

use self::instruction::Instruction;
use super::evaluator::stack::Stack;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Label(usize);

#[derive(Debug, Clone, Copy)]
pub(crate) enum InstructionOrLabel {
    Instruction(Instruction),
    Label(Label),
}

impl From<Instruction> for InstructionOrLabel {
    fn from(it: Instruction) -> Self {
        Self::Instruction(it)
    }
}

impl From<Label> for InstructionOrLabel {
    fn from(it: Label) -> Self {
        Self::Label(it)
    }
}

pub struct Program {
    pub stack: Stack,
    pub instructions: Vec<Instruction>,
}

impl Program {
    pub fn error() -> Self {
        Self {
            stack: Stack::new(DebugFlags::default()),
            instructions: vec![],
        }
    }
}

fn allocate(stack: &mut Stack, size_in_bytes: u64) -> usize {
    let size_in_words = bytes_to_word(size_in_bytes);
    let pointer = stack.len() * WORD_SIZE_IN_BYTES as usize;
    for _ in 0..size_in_words {
        stack.push(0);
    }
    pointer
}

pub fn convert<'a>(
    source_text: &'a SourceText<'a>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
    debug_flags: DebugFlags,
) -> Program {
    let bound_node = binder::bind(source_text, diagnostic_bag, debug_flags);
    if diagnostic_bag.has_errors() {
        return Program::error();
    }
    let mut stack = Stack::new(debug_flags);
    let instructions = convert_node(bound_node, &mut stack);
    let instructions = label_replacer::replace_labels(instructions);
    if debug_flags.print_instructions() {
        for (i, instruction) in instructions.iter().enumerate() {
            println!("  {:000}: {:?}", i, instruction);
        }
    }
    Program {
        instructions,
        stack,
    }
}

fn convert_node(node: BoundNode, stack: &mut Stack) -> Vec<InstructionOrLabel> {
    match node.kind {
        BoundNodeKind::ErrorExpression => unreachable!(),
        BoundNodeKind::FunctionDeclaration(function_declaration) => convert_function_declaration(function_declaration, stack),
        BoundNodeKind::LiteralExpression(literal) => convert_literal(literal, stack),
        BoundNodeKind::ArrayLiteralExpression(array_literal) => {
            convert_array_literal(array_literal, stack)
        }
        BoundNodeKind::VariableExpression(variable) => convert_variable(variable),
        BoundNodeKind::UnaryExpression(unary) => convert_unary(unary, stack),
        BoundNodeKind::BinaryExpression(binary) => convert_binary(binary, stack),
        BoundNodeKind::FunctionCall(function_call) => {
            convert_function_call(function_call, stack)
        }
        BoundNodeKind::SystemCall(system_call) => convert_system_call(system_call, stack),
        BoundNodeKind::ArrayIndex(array_index) => convert_array_index(array_index, stack),
        BoundNodeKind::FieldAccess(field_access) => convert_field_access(field_access, stack),
        BoundNodeKind::BlockStatement(block_statement) => {
            convert_block_statement(block_statement, stack)
        }
        BoundNodeKind::IfStatement(if_statement) => {
            convert_if_statement(if_statement, stack)
        }
        BoundNodeKind::VariableDeclaration(variable_declaration) => {
            convert_variable_declaration(variable_declaration, stack)
        }
        BoundNodeKind::WhileStatement(while_statement) => {
            convert_while_statement(while_statement, stack)
        }
        BoundNodeKind::Assignment(assignment) => convert_assignment(assignment, stack),
        BoundNodeKind::ExpressionStatement(expression_statement) => {
            convert_expression_statement(expression_statement, stack)
        }
        BoundNodeKind::ReturnStatement(return_statement) => {
            convert_return_statement(return_statement, stack)
        }
        BoundNodeKind::LabelAddress(label_address) => convert_label_address(label_address),
    }
}

fn convert_function_declaration(function_declaration: BoundFunctionDeclarationNodeKind, stack: &mut Stack) -> Vec<InstructionOrLabel> {
    let mut result = vec![];
    result.push(Instruction::label(function_declaration.index).into());
    for parameter in function_declaration.parameters {
        result.push(Instruction::store_in_register(parameter).into());
    }
    result.append(&mut convert_node(*function_declaration.body, stack));
    result
}

fn convert_node_for_assignment(
    node: BoundNode,
    stack: &mut Stack,
) -> Vec<InstructionOrLabel> {
    match node.kind {
        BoundNodeKind::VariableExpression(variable) => {
            convert_variable_for_assignment(variable)
        }
        BoundNodeKind::ArrayIndex(array_index) => {
            convert_array_index_for_assignment(array_index, stack)
        }
        _ => unreachable!(),
    }
}

fn convert_literal(
    literal: LiteralNodeKind,
    stack: &mut Stack,
) -> Vec<InstructionOrLabel> {
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
        Value::String(value) => return convert_string_literal(value, stack),
    };
    vec![Instruction::load_immediate(value).into()]
}

fn convert_string_literal(value: String, stack: &mut Stack) -> Vec<InstructionOrLabel> {
    let length_in_bytes = value.len() as u64;
    let mut address = allocate(stack, length_in_bytes + WORD_SIZE_IN_BYTES);
    let pointer = address as _;
    stack.write_word(address, length_in_bytes);
    address += WORD_SIZE_IN_BYTES as usize;
    let byte_groups = value.as_bytes().chunks_exact(WORD_SIZE_IN_BYTES as _);
    let remainder = byte_groups.remainder();
    for word in byte_groups {
        let word = [word[0], word[1], word[2], word[3], word[4], word[5], word[6], word[7], ];
        let word = u64::from_be_bytes(word);
        stack.write_word(address, word);
        address += WORD_SIZE_IN_BYTES as usize;
    }
    if !remainder.is_empty() {
        let word = [
            *remainder.get(0).unwrap_or(&0),
            *remainder.get(1).unwrap_or(&0),
            *remainder.get(2).unwrap_or(&0),
            *remainder.get(3).unwrap_or(&0),
            *remainder.get(4).unwrap_or(&0),
            *remainder.get(5).unwrap_or(&0),
            *remainder.get(6).unwrap_or(&0),
            *remainder.get(7).unwrap_or(&0),
        ];
        let word = u64::from_be_bytes(word);
        stack.write_word(address, word);
    }
    vec![Instruction::load_pointer(pointer).into()]
}

fn convert_array_literal(
    array_literal: BoundArrayLiteralNodeKind,
    stack: &mut Stack,
) -> Vec<InstructionOrLabel> {
    let mut result = vec![];
    let element_count = array_literal.children.len();
    let length_in_bytes = element_count as u64 * WORD_SIZE_IN_BYTES;
    let mut address = allocate(stack, length_in_bytes + WORD_SIZE_IN_BYTES) as u64;
    let pointer = address;
    stack.write_word(address as _, length_in_bytes as _);
    address += WORD_SIZE_IN_BYTES;
    match &array_literal.children[0].type_ {
        Type::Error | Type::Void => unreachable!(),
        Type::Any => todo!(),
        Type::Function(_) => todo!(),
        Type::Array(_) | Type::String |
        Type::Integer | Type::Boolean | Type::SystemCall(_) => {
            for child in array_literal.children.into_iter() {
                result.append(&mut convert_node(child, stack));
                result.push(Instruction::write_to_stack(address as _).into());
                address += WORD_SIZE_IN_BYTES;
            }
        }
    }
    result.push(Instruction::load_pointer(pointer).into());
    result
}

fn convert_variable(variable: BoundVariableNodeKind) -> Vec<InstructionOrLabel> {
    vec![Instruction::load_register(variable.variable_index).into()]
}

fn convert_variable_for_assignment(
    variable: BoundVariableNodeKind,
) -> Vec<InstructionOrLabel> {
    vec![Instruction::store_in_register(variable.variable_index).into()]
}

fn convert_array_index_for_assignment(
    array_index: BoundArrayIndexNodeKind,
    stack: &mut Stack,
) -> Vec<InstructionOrLabel> {
    let mut result = convert_node(*array_index.base, stack);
    result.append(&mut convert_node(*array_index.index, stack));
    result.push(Instruction::store_in_memory().into());
    result
}

fn convert_unary(
    unary: BoundUnaryNodeKind,
    stack: &mut Stack,
) -> Vec<InstructionOrLabel> {
    let mut result = convert_node(*unary.operand, stack);
    match unary.operator_token {
        BoundUnaryOperator::ArithmeticNegate => result.push(Instruction::twos_complement().into()),
        BoundUnaryOperator::ArithmeticIdentity => {}
    }
    result
}

fn convert_binary(
    binary: BoundBinaryNodeKind,
    stack: &mut Stack,
) -> Vec<InstructionOrLabel> {
    let operator_instruction = match binary.operator_token {
        BoundBinaryOperator::ArithmeticAddition => Instruction::addition(),
        BoundBinaryOperator::ArithmeticSubtraction => Instruction::subtraction(),
        BoundBinaryOperator::ArithmeticMultiplication => Instruction::multiplication(),
        BoundBinaryOperator::ArithmeticDivision => Instruction::division(),
        BoundBinaryOperator::Equals => {
            if matches!(binary.lhs.type_, Type::Array(_) | Type::String) && matches!(binary.rhs.type_, Type::Array(_) | Type::String) {
                Instruction::array_equals()
            } else {
                Instruction::equals()
            }
        }
        BoundBinaryOperator::NotEquals => {
            if matches!(binary.lhs.type_, Type::Array(_) | Type::String) && matches!(binary.rhs.type_, Type::Array(_) | Type::String) {
                Instruction::array_not_equals()
            } else {
                Instruction::not_equals()
            }
        }
        BoundBinaryOperator::LessThan => Instruction::less_than(),
        BoundBinaryOperator::GreaterThan => Instruction::greater_than(),
        BoundBinaryOperator::LessThanEquals => Instruction::less_than_equals(),
        BoundBinaryOperator::GreaterThanEquals => Instruction::greater_than_equals(),
        BoundBinaryOperator::StringConcat => Instruction::string_concat(),
    };
    let mut result = vec![];
    let lhs_is_string = matches!(binary.lhs.type_, Type::String);
    let lhs_type_identifier = binary.lhs.type_.type_identifier();
    result.append(&mut convert_node(*binary.lhs, stack));
    if matches!(binary.operator_token, BoundBinaryOperator::StringConcat) && !lhs_is_string {
        result.push(Instruction::type_identifier(lhs_type_identifier).into());
        result.push(Instruction::system_call(SystemCallKind::ToString, 1).into());
    }
    let rhs_is_string = matches!(binary.rhs.type_, Type::String);
    let rhs_type_identifier = binary.rhs.type_.type_identifier();
    result.append(&mut convert_node(*binary.rhs, stack));
    if matches!(binary.operator_token, BoundBinaryOperator::StringConcat) && !rhs_is_string {
        result.push(Instruction::type_identifier(rhs_type_identifier).into());
        result.push(Instruction::system_call(SystemCallKind::ToString, 1).into());
    }
    result.push(operator_instruction.into());
    result
}

fn convert_function_call(
    function_call: BoundFunctionCallNodeKind,
    stack: &mut Stack,
) -> Vec<InstructionOrLabel> {
    let mut result = vec![];
    let argument_count = function_call.arguments.len();
    for argument in function_call.arguments {
        result.append(&mut convert_node(argument, stack));
    }
    result.append(&mut convert_node(*function_call.base, stack));
    result.push(Instruction::function_call(argument_count).into());
    result
}

fn convert_system_call(
    system_call: BoundSystemCallNodeKind,
    stack: &mut Stack,
) -> Vec<InstructionOrLabel> {
    let mut result = vec![];
    let argument_count = system_call.arguments.len();
    for argument in system_call.arguments {
        let type_identifier = argument.type_.type_identifier();
        result.append(&mut convert_node(argument, stack));
        result.push(Instruction::type_identifier(type_identifier).into());
    }
    result.push(Instruction::system_call(system_call.base, argument_count).into());
    result
}

fn convert_array_index(
    array_index: BoundArrayIndexNodeKind,
    stack: &mut Stack,
) -> Vec<InstructionOrLabel> {
    let mut result = convert_node(*array_index.base, stack);
    result.append(&mut convert_node(*array_index.index, stack));
    result.push(Instruction::array_index().into());
    result
}

fn convert_field_access(
    field_access: BoundFieldAccessNodeKind,
    stack: &mut Stack,
) -> Vec<InstructionOrLabel> {
    convert_node(*field_access.base, stack)
}

fn convert_if_statement(
    if_statement: BoundIfStatementNodeKind,
    stack: &mut Stack,
) -> Vec<InstructionOrLabel> {
    let mut result = convert_node(*if_statement.condition, stack);
    let mut body = convert_node(*if_statement.body, stack);
    let jmp = Instruction::jump_if_false(body.len() as i64);
    result.push(jmp.into());
    result.append(&mut body);
    result
}

fn convert_variable_declaration(
    variable_declaration: BoundVariableDeclarationNodeKind,
    stack: &mut Stack,
) -> Vec<InstructionOrLabel> {
    let mut result = convert_node(*variable_declaration.initializer, stack);
    result.push(Instruction::store_in_register(
        variable_declaration.variable_index,
    ).into());
    result
}

fn convert_while_statement(
    while_statement: BoundWhileStatementNodeKind,
    stack: &mut Stack,
) -> Vec<InstructionOrLabel> {
    let mut result = convert_node(*while_statement.condition, stack);
    let mut body = convert_node(*while_statement.body, stack);
    let jmp = Instruction::jump_if_false(body.len() as i64 + 1);
    result.push(jmp.into());
    result.append(&mut body);
    let jmp = Instruction::jump_relative(-(result.len() as i64 + 1));
    result.push(jmp.into());
    result
}

fn convert_assignment(
    assignment: BoundAssignmentNodeKind,
    stack: &mut Stack,
) -> Vec<InstructionOrLabel> {
    let mut result = convert_node(*assignment.expression, stack);
    result.append(&mut convert_node_for_assignment(
        *assignment.variable,
        stack,
    ));
    result
}

fn convert_block_statement(
    block_statement: BoundBlockStatementNodeKind,
    stack: &mut Stack,
) -> Vec<InstructionOrLabel> {
    let mut result = vec![];
    for node in block_statement.statements {
        result.append(&mut convert_node(node, stack));
    }
    result
}

fn convert_expression_statement(
    expression_statement: BoundExpressionStatementNodeKind,
    stack: &mut Stack,
) -> Vec<InstructionOrLabel> {
    let pushes_on_stack = !matches!(expression_statement.expression.type_, Type::Void);
    let mut result = convert_node(*expression_statement.expression, stack);
    if pushes_on_stack {
        result.push(Instruction::pop().into());
    }
    result
}

fn convert_return_statement(
    return_statement: BoundReturnStatementNodeKind,
    stack: &mut Stack,
) -> Vec<InstructionOrLabel> {
    let mut pushes_on_stack = false;
    let mut result = if let Some(expression) = return_statement.expression {
        pushes_on_stack = true;
        convert_node(*expression, stack)
    } else {
        vec![]
    };
    result.push(Instruction::return_from_function(pushes_on_stack).into());
    result
}

fn convert_label_address(
    label_address: usize,
) -> Vec<InstructionOrLabel> {
    vec![Label(label_address).into()]
}