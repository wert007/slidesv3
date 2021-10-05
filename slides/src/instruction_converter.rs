pub mod instruction;
#[cfg(test)]
mod tests;

mod label_replacer;

use crate::{
    binder::{
        self,
        bound_nodes::{
            BoundArrayIndexNodeKind, BoundArrayLiteralNodeKind, BoundAssignmentNodeKind,
            BoundBinaryNodeKind, BoundBlockStatementNodeKind, BoundConstructorCallNodeKind,
            BoundExpressionStatementNodeKind, BoundFieldAccessNodeKind, BoundFunctionCallNodeKind,
            BoundFunctionDeclarationNodeKind, BoundJumpNodeKind, BoundNode, BoundNodeKind,
            BoundReturnStatementNodeKind, BoundSystemCallNodeKind, BoundUnaryNodeKind,
            BoundVariableDeclarationNodeKind, BoundVariableNodeKind,
        },
        operators::{BoundBinaryOperator, BoundUnaryOperator},
        typing::{SystemCallKind, Type},
    },
    debug::DebugFlags,
    diagnostics::DiagnosticBag,
    evaluator::{bytes_to_word, WORD_SIZE_IN_BYTES},
    parser::syntax_nodes::LiteralNodeKind,
    text::{SourceText, TextSpan},
    value::Value,
};

use self::instruction::Instruction;
use super::evaluator::stack::Stack;

#[derive(Debug, Clone, Copy)]
pub(crate) struct LabelReference {
    label_reference: usize,
    span: TextSpan,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum InstructionOrLabelReference {
    Instruction(Instruction),
    LabelReference(LabelReference),
}

impl std::fmt::Display for InstructionOrLabelReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InstructionOrLabelReference::Instruction(i) => write!(f, "  {:?}", i),
            InstructionOrLabelReference::LabelReference(l) => write!(f, "L{}:", l.label_reference),
        }
    }
}

impl InstructionOrLabelReference {
    pub fn as_label(self) -> Option<LabelReference> {
        match self {
            InstructionOrLabelReference::Instruction(_) => None,
            InstructionOrLabelReference::LabelReference(it) => Some(it),
        }
    }
}

impl From<Instruction> for InstructionOrLabelReference {
    fn from(it: Instruction) -> Self {
        Self::Instruction(it)
    }
}

impl From<LabelReference> for InstructionOrLabelReference {
    fn from(it: LabelReference) -> Self {
        Self::LabelReference(it)
    }
}

struct InstructionConverter {
    pub stack: Stack,
    pub fixed_variable_count: usize,
}

pub struct Program {
    pub stack: Stack,
    pub instructions: Vec<Instruction>,
    pub max_used_variables: usize,
    pub protected_variables: usize,
}

impl Program {
    pub fn error() -> Self {
        Self {
            stack: Stack::new(DebugFlags::default()),
            instructions: vec![],
            max_used_variables: 0,
            protected_variables: 0,
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
    let bound_program = binder::bind(source_text, diagnostic_bag, debug_flags);
    if diagnostic_bag.has_errors() {
        return Program::error();
    }
    let bound_node = bound_program.program;
    let mut converter = InstructionConverter {
        stack: Stack::new(debug_flags),
        fixed_variable_count: bound_program.fixed_variable_count,
    };
    let instructions = convert_node(bound_node, &mut converter);
    if debug_flags.print_instructions_and_labels {
        for (i, instruction) in instructions.iter().enumerate() {
            println!("  {:000}: {}", i, instruction);
        }
    }

    let instructions = label_replacer::replace_labels(instructions, debug_flags);
    if debug_flags.print_instructions() {
        for (i, instruction) in instructions.iter().enumerate() {
            println!("  {:000}: {:?}", i, instruction);
        }
    }
    Program {
        instructions,
        stack: converter.stack,
        max_used_variables: bound_program.max_used_variables,
        protected_variables: converter.fixed_variable_count,
    }
}

fn convert_node(
    node: BoundNode,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    match node.kind {
        BoundNodeKind::IfStatement(_)
        | BoundNodeKind::WhileStatement(_)
        | BoundNodeKind::ErrorExpression => unreachable!(),
        BoundNodeKind::FunctionDeclaration(function_declaration) => {
            convert_function_declaration(node.span, function_declaration, converter)
        }
        BoundNodeKind::LiteralExpression(literal) => convert_literal(node.span, literal, converter),
        BoundNodeKind::ArrayLiteralExpression(array_literal) => {
            convert_array_literal(node.span, array_literal, converter)
        }
        BoundNodeKind::VariableExpression(variable) => convert_variable(node.span, variable),
        BoundNodeKind::UnaryExpression(unary) => convert_unary(node.span, unary, converter),
        BoundNodeKind::BinaryExpression(binary) => convert_binary(node.span, binary, converter),
        BoundNodeKind::FunctionCall(function_call) => {
            convert_function_call(node.span, function_call, converter)
        }
        BoundNodeKind::ConstructorCall(constructor_call) => {
            convert_constructor_call(node.span, constructor_call, converter)
        }
        BoundNodeKind::SystemCall(system_call) => {
            convert_system_call(node.span, system_call, converter)
        }
        BoundNodeKind::ArrayIndex(array_index) => {
            convert_array_index(node.span, array_index, converter)
        }
        BoundNodeKind::FieldAccess(field_access) => {
            convert_field_access(node.span, field_access, converter)
        }
        BoundNodeKind::BlockStatement(block_statement) => {
            convert_block_statement(node.span, block_statement, converter)
        }
        BoundNodeKind::VariableDeclaration(variable_declaration) => {
            convert_variable_declaration(node.span, variable_declaration, converter)
        }
        BoundNodeKind::Assignment(assignment) => {
            convert_assignment(node.span, assignment, converter)
        }
        BoundNodeKind::ExpressionStatement(expression_statement) => {
            convert_expression_statement(node.span, expression_statement, converter)
        }
        BoundNodeKind::ReturnStatement(return_statement) => {
            convert_return_statement(node.span, return_statement, converter)
        }
        BoundNodeKind::Label(index) => convert_label(node.span, index),
        BoundNodeKind::LabelReference(label_reference) => {
            convert_label_reference(node.span, label_reference)
        }
        BoundNodeKind::Jump(jump) => convert_jump(node.span, jump, converter),
    }
}

fn convert_function_declaration(
    span: TextSpan,
    function_declaration: BoundFunctionDeclarationNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = vec![Instruction::label(function_declaration.index)
        .span(span)
        .into()];
    for parameter in function_declaration.parameters.into_iter().rev() {
        result.push(Instruction::store_in_register(parameter).span(span).into());
    }
    result.append(&mut convert_node(*function_declaration.body, converter));
    result
}

fn convert_node_for_assignment(
    node: BoundNode,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    match node.kind {
        BoundNodeKind::VariableExpression(variable) => {
            convert_variable_for_assignment(node.span, variable)
        }
        BoundNodeKind::ArrayIndex(array_index) => {
            convert_array_index_for_assignment(node.span, array_index, converter)
        }
        BoundNodeKind::FieldAccess(field_access) => {
            convert_field_access_for_assignment(node.span, field_access, converter)
        }
        _ => unreachable!(),
    }
}

fn convert_literal(
    span: TextSpan,
    literal: LiteralNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
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
        Value::String(value) => return convert_string_literal(span, value, converter),
    };
    vec![Instruction::load_immediate(value).span(span).into()]
}

fn convert_string_literal(
    span: TextSpan,
    value: String,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let length_in_bytes = value.len() as u64;
    let mut address = allocate(&mut converter.stack, length_in_bytes + WORD_SIZE_IN_BYTES);
    let pointer = address as _;
    converter.stack.write_word(address, length_in_bytes);
    address += WORD_SIZE_IN_BYTES as usize;
    let byte_groups = value.as_bytes().chunks_exact(WORD_SIZE_IN_BYTES as _);
    let remainder = byte_groups.remainder();
    for word in byte_groups {
        let word = [
            word[0], word[1], word[2], word[3], word[4], word[5], word[6], word[7],
        ];
        let word = u64::from_be_bytes(word);
        converter.stack.write_word(address, word);
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
        converter.stack.write_word(address, word);
    }
    vec![Instruction::load_pointer(pointer).span(span).into()]
}

fn convert_array_literal(
    span: TextSpan,
    array_literal: BoundArrayLiteralNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = vec![];
    let element_count = array_literal.children.len();
    let length_in_bytes = element_count as u64 * WORD_SIZE_IN_BYTES;
    let mut address = allocate(&mut converter.stack, length_in_bytes + WORD_SIZE_IN_BYTES) as u64;
    let pointer = address;
    converter
        .stack
        .write_word(address as _, length_in_bytes as _);
    address += WORD_SIZE_IN_BYTES;
    match &array_literal.children[0].type_ {
        Type::Error | Type::Void => unreachable!(),
        Type::Any => todo!(),
        Type::Struct(_) => todo!(),
        Type::Function(_)
        | Type::Array(_)
        | Type::String
        | Type::Integer
        | Type::Boolean
        | Type::SystemCall(_) => {
            for child in array_literal.children.into_iter() {
                result.append(&mut convert_node(child, converter));
                result.push(Instruction::write_to_stack(address as _).span(span).into());
                address += WORD_SIZE_IN_BYTES;
            }
        }
    }
    result.push(Instruction::load_pointer(pointer).span(span).into());
    result
}

fn convert_variable(
    span: TextSpan,
    variable: BoundVariableNodeKind,
) -> Vec<InstructionOrLabelReference> {
    vec![Instruction::load_register(variable.variable_index)
        .span(span)
        .into()]
}

fn convert_variable_for_assignment(
    span: TextSpan,
    variable: BoundVariableNodeKind,
) -> Vec<InstructionOrLabelReference> {
    vec![Instruction::store_in_register(variable.variable_index)
        .span(span)
        .into()]
}

fn convert_array_index_for_assignment(
    span: TextSpan,
    array_index: BoundArrayIndexNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    // array
    let mut result = convert_node(*array_index.base, converter);
    // index
    result.append(&mut convert_node(*array_index.index, converter));
    result.push(Instruction::load_immediate(1).span(span).into());
    result.push(Instruction::addition().span(span).into());
    result.push(Instruction::load_immediate(WORD_SIZE_IN_BYTES).span(span).into());
    result.push(Instruction::multiplication().span(span).into());

    result.push(Instruction::check_array_bounds().span(span).into());
    result.push(Instruction::store_in_memory().span(span).into());
    result
}

fn convert_field_access_for_assignment(
    span: TextSpan,
    field_access: BoundFieldAccessNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = convert_node(*field_access.base, converter);
    result.push(Instruction::load_immediate(field_access.offset).span(span).into());
    result.push(Instruction::store_in_memory().span(span).into());
    result
}

fn convert_unary(
    span: TextSpan,
    unary: BoundUnaryNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = convert_node(*unary.operand, converter);
    match unary.operator_token {
        BoundUnaryOperator::ArithmeticNegate => {
            result.push(Instruction::twos_complement().span(span).into())
        }
        BoundUnaryOperator::ArithmeticIdentity => {}
    }
    result
}

fn convert_binary(
    span: TextSpan,
    binary: BoundBinaryNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let operator_instruction = match binary.operator_token {
        BoundBinaryOperator::ArithmeticAddition => Instruction::addition(),
        BoundBinaryOperator::ArithmeticSubtraction => Instruction::subtraction(),
        BoundBinaryOperator::ArithmeticMultiplication => Instruction::multiplication(),
        BoundBinaryOperator::ArithmeticDivision => Instruction::division(),
        BoundBinaryOperator::Equals => {
            if matches!(binary.lhs.type_, Type::Array(_) | Type::String)
                && matches!(binary.rhs.type_, Type::Array(_) | Type::String)
            {
                Instruction::array_equals()
            } else {
                Instruction::equals()
            }
        }
        BoundBinaryOperator::NotEquals => {
            if matches!(binary.lhs.type_, Type::Array(_) | Type::String)
                && matches!(binary.rhs.type_, Type::Array(_) | Type::String)
            {
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
    }
    .span(span);
    let mut result = vec![];
    let lhs_is_string = matches!(binary.lhs.type_, Type::String);
    let lhs_type_identifier = binary.lhs.type_.type_identifier();
    result.append(&mut convert_node(*binary.lhs, converter));
    if matches!(binary.operator_token, BoundBinaryOperator::StringConcat) && !lhs_is_string {
        result.push(
            Instruction::type_identifier(lhs_type_identifier)
                .span(span)
                .into(),
        );
        result.push(
            Instruction::system_call(SystemCallKind::ToString, 1)
                .span(span)
                .into(),
        );
    }
    let rhs_is_string = matches!(binary.rhs.type_, Type::String);
    let rhs_type_identifier = binary.rhs.type_.type_identifier();
    result.append(&mut convert_node(*binary.rhs, converter));
    if matches!(binary.operator_token, BoundBinaryOperator::StringConcat) && !rhs_is_string {
        result.push(
            Instruction::type_identifier(rhs_type_identifier)
                .span(span)
                .into(),
        );
        result.push(
            Instruction::system_call(SystemCallKind::ToString, 1)
                .span(span)
                .into(),
        );
    }
    result.push(operator_instruction.into());
    result
}

fn convert_function_call(
    span: TextSpan,
    function_call: BoundFunctionCallNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = vec![];
    let argument_count = function_call.arguments.len();
    for argument in function_call.arguments {
        result.append(&mut convert_node(argument, converter));
    }
    result.append(&mut convert_node(*function_call.base, converter));
    result.push(Instruction::function_call(argument_count).span(span).into());
    result
}

fn convert_constructor_call(
    span: TextSpan,
    constructor_call: BoundConstructorCallNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = vec![];
    let pointer = allocate(
        &mut converter.stack,
        constructor_call.base_type.size_in_bytes(),
    ) as _;
    let mut writing_pointer = pointer;
    for (argument, type_) in constructor_call
        .arguments
        .into_iter()
        .zip(constructor_call.base_type.fields.iter())
    {
        let argument_span = argument.span;
        result.append(&mut convert_node(argument, converter));
        result.push(
            Instruction::write_to_stack(writing_pointer)
                .span(argument_span)
                .into(),
        );
        writing_pointer += type_.size_in_bytes();
    }
    result.push(Instruction::load_pointer(pointer).span(span).into());
    result
}

fn convert_system_call(
    span: TextSpan,
    system_call: BoundSystemCallNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = vec![];
    let argument_count = system_call.arguments.len();
    for argument in system_call.arguments {
        let type_identifier = argument.type_.type_identifier();
        result.append(&mut convert_node(argument, converter));
        result.push(
            Instruction::type_identifier(type_identifier)
                .span(span)
                .into(),
        );
    }
    result.push(
        Instruction::system_call(system_call.base, argument_count)
            .span(span)
            .into(),
    );
    result
}

fn convert_array_index(
    span: TextSpan,
    array_index: BoundArrayIndexNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = convert_node(*array_index.base, converter);
    result.append(&mut convert_node(*array_index.index, converter));
    result.push(Instruction::array_index().span(span).into());
    result
}

fn convert_field_access(
    span: TextSpan,
    field_access: BoundFieldAccessNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = convert_node(*field_access.base, converter);
    if !matches!(field_access.type_, Type::SystemCall(_)) {
        result.push(Instruction::read_word_with_offset(field_access.offset).span(span).into());
    }
    result
}

fn convert_variable_declaration(
    span: TextSpan,
    variable_declaration: BoundVariableDeclarationNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = convert_node(*variable_declaration.initializer, converter);
    result.push(
        Instruction::store_in_register(variable_declaration.variable_index)
            .span(span)
            .into(),
    );
    result
}

fn convert_assignment(
    _span: TextSpan,
    assignment: BoundAssignmentNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = convert_node(*assignment.expression, converter);
    result.append(&mut convert_node_for_assignment(
        *assignment.variable,
        converter,
    ));
    result
}

fn convert_block_statement(
    _span: TextSpan,
    block_statement: BoundBlockStatementNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = vec![];
    for node in block_statement.statements {
        result.append(&mut convert_node(node, converter));
    }
    result
}

fn convert_expression_statement(
    span: TextSpan,
    expression_statement: BoundExpressionStatementNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let pushes_on_stack = !matches!(expression_statement.expression.type_, Type::Void);
    let mut result = convert_node(*expression_statement.expression, converter);
    if pushes_on_stack {
        result.push(Instruction::pop().span(span).into());
    }
    result
}

fn convert_return_statement(
    span: TextSpan,
    return_statement: BoundReturnStatementNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut pushes_on_stack = false;
    let mut result = if let Some(expression) = return_statement.expression {
        pushes_on_stack = true;
        convert_node(*expression, converter)
    } else {
        vec![]
    };
    result.push(
        Instruction::return_from_function(pushes_on_stack, return_statement.restores_variables)
            .span(span)
            .into(),
    );
    result
}

fn convert_label(span: TextSpan, index: usize) -> Vec<InstructionOrLabelReference> {
    vec![Instruction::label(index).span(span).into()]
}

fn convert_label_reference(
    span: TextSpan,
    label_reference: usize,
) -> Vec<InstructionOrLabelReference> {
    vec![LabelReference {
        label_reference,
        span,
    }
    .into()]
}

fn convert_jump(
    span: TextSpan,
    jump: BoundJumpNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut label = convert_node(*jump.target, converter);
    assert_eq!(label.len(), 1);
    let label = label.pop().unwrap();
    let label = label
        .as_label()
        .unwrap_or_else(|| panic!("{:?}", label))
        .label_reference;
    match jump.condition {
        Some(condition) => {
            let mut result = convert_node(*condition, converter);
            result.push(
                Instruction::jump_to_label_conditionally(label, jump.jump_if_true)
                    .span(span)
                    .into(),
            );
            result
        }
        None => vec![Instruction::jump_to_label(label).span(span).into()],
    }
}
