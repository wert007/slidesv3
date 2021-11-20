pub mod instruction;
#[cfg(test)]
mod tests;

mod label_replacer;

use std::path::PathBuf;

use crate::{binder::{self, bound_nodes::{
            BoundArrayIndexNodeKind, BoundArrayLiteralNodeKind, BoundAssignmentNodeKind,
            BoundBinaryNodeKind, BoundBlockStatementNodeKind, BoundClosureNodeKind,
            BoundConstructorCallNodeKind, BoundConversionNodeKind,
            BoundExpressionStatementNodeKind, BoundFieldAccessNodeKind, BoundFunctionCallNodeKind,
            BoundFunctionDeclarationNodeKind, BoundJumpNodeKind, BoundNode, BoundNodeKind,
            BoundReturnStatementNodeKind, BoundSystemCallNodeKind, BoundUnaryNodeKind,
            BoundVariableDeclarationNodeKind, BoundVariableNodeKind, ConversionKind,
        }, operators::{BoundBinaryOperator, BoundUnaryOperator}, symbols::Library, typing::{FunctionKind, SystemCallKind, Type}}, debug::DebugFlags, diagnostics::DiagnosticBag, evaluator::memory::{WORD_SIZE_IN_BYTES, bytes_to_word, static_memory::StaticMemory}, parser::syntax_nodes::LiteralNodeKind, text::{SourceText, TextSpan}, value::Value};

use self::instruction::{Instruction, op_codes::OpCode};

#[derive(Debug, Clone, Copy)]
pub struct LabelReference {
    pub label_reference: usize,
    pub span: TextSpan,
}

#[derive(Debug, Clone, Copy)]
pub enum InstructionOrLabelReference {
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

    pub fn span(&self) -> Option<TextSpan> {
        match self {
            InstructionOrLabelReference::Instruction(instruction) => instruction.span,
            InstructionOrLabelReference::LabelReference(label) => Some(label.span),
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
    pub static_memory: StaticMemory,
    pub fixed_variable_count: usize,
    next_label_index: usize,
}

impl InstructionConverter {
    pub fn generate_label(&mut self) -> usize {
        self.next_label_index += 1;
        self.next_label_index
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub static_memory: StaticMemory,
    pub startup_instructions: Vec<Instruction>,
    pub instructions: Vec<Instruction>,
    pub max_used_variables: usize,
    pub protected_variables: usize,
    pub label_count: usize,
    pub entry_point: usize,
}

impl Program {
    pub fn error() -> Self {
        Self {
            static_memory: StaticMemory::new(DebugFlags::default()),
            startup_instructions: vec![],
            instructions: vec![],
            max_used_variables: 0,
            protected_variables: 0,
            label_count: 0,
            entry_point: 0,
        }
    }
}

pub fn convert<'a>(
    source_text: &'a SourceText<'a>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
    debug_flags: DebugFlags,
) -> Program {
    let mut bound_program = binder::bind_program(source_text, diagnostic_bag, debug_flags);
    if diagnostic_bag.has_errors() {
        return Program::error();
    }
    let bound_node = bound_program.functions;
    let mut converter = InstructionConverter {
        static_memory: StaticMemory::new(debug_flags),
        fixed_variable_count: bound_program.fixed_variable_count,
        next_label_index: bound_program.label_count,
    };
    converter.static_memory.allocate_null();
    for lib in bound_program.referenced_libraries.iter_mut() {
        let static_memory_size = converter.static_memory.size_in_bytes();
        for inst in lib.instructions.iter_mut() {
            if let InstructionOrLabelReference::Instruction(Instruction { arg, op_code: OpCode::LoadPointer, ..}) = inst {
                *arg += static_memory_size;
            }
        }
        converter.static_memory.insert(&mut lib.program.static_memory);
    }
    let mut instructions = bound_program.startup;
    let foreign_instruction_start = instructions.len();
    instructions.append(&mut bound_program.referenced_libraries.into_iter().map(|l| l.instructions).flatten().collect());
    let foreign_instruction_end = instructions.len();
    let entry_point = 0;
    instructions.append(&mut convert_node(bound_node, &mut converter));
    if debug_flags.print_instructions_and_labels {
        for (i, instruction) in instructions.iter().enumerate() {
            println!("  {:000}: {}", i, instruction);
        }
    }

    let instructions = label_replacer::replace_labels(instructions, debug_flags);
    if debug_flags.print_instructions() {
        crate::debug::print_instructions_with_source_code(0, &instructions[..foreign_instruction_start], source_text);
        println!("> Skipping library code right now, their code will be emitted extra.");
        crate::debug::print_instructions_with_source_code(foreign_instruction_end, &instructions[foreign_instruction_end..], source_text);
    }
    if debug_flags.output_instructions_to_sldasm {
        crate::debug::output_instructions_with_source_code_to_sldasm_skip(foreign_instruction_start, foreign_instruction_end, &instructions, source_text);
    }
    Program {
        startup_instructions: vec![],
        instructions,
        static_memory: converter.static_memory,
        max_used_variables: bound_program.max_used_variables,
        protected_variables: converter.fixed_variable_count,
        label_count: converter.next_label_index,
        entry_point,
    }
}


pub fn convert_library<'a>(
    source_text: &'a SourceText<'a>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
    debug_flags: DebugFlags,
) -> Library {
    let bound_program = binder::bind_library(source_text, diagnostic_bag, debug_flags);
    if diagnostic_bag.has_errors() {
        return Library::error();
    }
    let exported_functions = bound_program.exported_functions;
    let exported_structs = bound_program.exported_structs;
    let mut bound_program = bound_program.program;
    let bound_node = bound_program.functions;
    let mut converter = InstructionConverter {
        static_memory: StaticMemory::new(debug_flags),
        fixed_variable_count: bound_program.fixed_variable_count,
        next_label_index: bound_program.label_count,
    };
    for lib in bound_program.referenced_libraries.iter_mut() {
        let static_memory_size = converter.static_memory.size_in_bytes();
        for inst in lib.instructions.iter_mut() {
            if let InstructionOrLabelReference::Instruction(Instruction { arg, op_code: OpCode::LoadPointer, ..}) = inst {
                *arg += static_memory_size;
            }
        }
        converter.static_memory.insert(&mut lib.program.static_memory);
    }
    let instructions = convert_node(bound_node, &mut converter);
    if debug_flags.print_instructions_and_labels {
        for (i, instruction) in instructions.iter().enumerate() {
            println!("  {:000}: {}", i, instruction);
        }
    }

    // let instructions = label_replacer::replace_labels(instructions, debug_flags);
    if debug_flags.print_instructions() {
        crate::debug::print_instructions_or_labels_with_source_code(0, &instructions, source_text);
    }
    if debug_flags.output_instructions_to_sldasm {
        crate::debug::output_instructions_or_labels_with_source_code_to_sldasm(0, &instructions, source_text);
    }
    Library {
        path: source_text.file_name.into(),
        // FIXME: Every library should have a library statement, which also
        // specifies its name!
        // NOTE: This value is overwritten anyway by the binder.
        name: PathBuf::from(source_text.file_name).file_stem().unwrap().to_string_lossy().into_owned(),
        referenced_libraries: bound_program.referenced_libraries,
        instructions,
        startup: bound_program.startup,
        program: Program {
            startup_instructions: vec![],
            instructions: vec![],
            static_memory: converter.static_memory,
            max_used_variables: bound_program.max_used_variables,
            protected_variables: converter.fixed_variable_count,
            label_count: converter.next_label_index,
            entry_point: !0,
        },
        functions: exported_functions,
        structs: exported_structs,
        has_errors: false,
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
        BoundNodeKind::Closure(closure) => convert_closure(node.span, closure, converter),
        BoundNodeKind::Conversion(conversion) => {
            convert_conversion(node.span, conversion, converter)
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
        Value::None => return vec![Instruction::load_pointer(0).span(span).into()],
    };
    vec![Instruction::load_immediate(value).span(span).into()]
}

fn convert_string_literal(
    span: TextSpan,
    value: String,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let pointer = converter.static_memory.allocate_string(value);
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
    for child in array_literal.children.into_iter().rev() {
        result.append(&mut convert_node(child, converter));
    }
    result.push(
        Instruction::load_immediate(length_in_bytes)
            .span(span)
            .into(),
    );
    result.push(
        Instruction::write_to_heap((element_count + 1) as _)
            .span(span)
            .into(),
    );
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
    result.push(
        Instruction::load_immediate(WORD_SIZE_IN_BYTES)
            .span(span)
            .into(),
    );
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
    result.push(
        Instruction::load_immediate(field_access.offset)
            .span(span)
            .into(),
    );
    result.push(Instruction::store_in_memory().span(span).into());
    result
}

fn convert_unary(
    span: TextSpan,
    unary: BoundUnaryNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let is_pointer = unary.operand.type_.is_pointer();
    let mut result = convert_node(*unary.operand, converter);
    match unary.operator_token {
        BoundUnaryOperator::ArithmeticNegate => {
            result.push(Instruction::twos_complement().span(span).into())
        }
        BoundUnaryOperator::ArithmeticIdentity => {}
        BoundUnaryOperator::LogicalNegation if is_pointer => {
            result.push(Instruction::load_pointer(0).span(span).into());
            result.push(Instruction::noneable_equals(1).span(span).into());
        }
        BoundUnaryOperator::LogicalNegation => {
            result.push(Instruction::load_immediate(0).span(span).into());
            result.push(Instruction::equals().span(span).into());
        }
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
            match (&binary.lhs.type_, &binary.rhs.type_) {
                (Type::Array(_) | Type::String, Type::Array(_) | Type::String) => {
                    Instruction::array_equals()
                }
                (Type::Noneable(base_type), Type::Noneable(_)) if !base_type.is_pointer() => {
                    Instruction::noneable_equals(base_type.size_in_bytes())
                }
                (Type::Noneable(base_type), Type::Noneable(_)) if base_type.is_pointer() => {
                    todo!(
                        "Not completely implemented. None must become an actual
                    pointer, which points to invalid memory. And array_equals
                    must return false if one of those is the none pointer,
                    but not the other.
                    "
                    );
                    // Instruction::array_equals()
                }
                _ => Instruction::equals(),
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
        BoundBinaryOperator::NoneableOrValue => {
            Instruction::noneable_or_value(!binary.lhs.type_.is_pointer())
        }
    }
    .span(span);
    let mut result = vec![];
    result.append(&mut convert_node(*binary.lhs, converter));
    if matches!(binary.operator_token, BoundBinaryOperator::StringConcat) {
        result.push(Instruction::load_immediate(1).span(span).into());
        result.push(
            Instruction::system_call(SystemCallKind::ToString)
                .span(span)
                .into(),
        );
    }
    result.append(&mut convert_node(*binary.rhs, converter));
    if matches!(binary.operator_token, BoundBinaryOperator::StringConcat) {
        result.push(Instruction::load_immediate(1).span(span).into());
        result.push(
            Instruction::system_call(SystemCallKind::ToString)
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
    let argument_count = function_call.arguments.len()
        + if function_call.has_this_argument {
            1
        } else {
            0
        };
    for argument in function_call.arguments {
        result.append(&mut convert_node(argument, converter));
    }
    let is_closure = matches!(function_call.base.type_, Type::Closure(_));
    result.append(&mut convert_node(*function_call.base, converter));
    if is_closure {
        result.push(
            Instruction::decode_closure(argument_count as _, true)
                .span(span)
                .into(),
        );
    } else {
        result.push(
            Instruction::load_immediate(argument_count as _)
                .span(span)
                .into(),
        );
    }
    result.push(Instruction::function_call().span(span).into());
    result
}

fn convert_constructor_call(
    span: TextSpan,
    constructor_call: BoundConstructorCallNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = vec![];
    let word_count = bytes_to_word(constructor_call.base_type.size_in_bytes());
    for argument in constructor_call.arguments.into_iter().rev() {
        result.append(&mut convert_node(argument, converter));
    }
    result.push(Instruction::write_to_heap(word_count).span(span).into());
    result
}

fn convert_system_call(
    span: TextSpan,
    system_call: BoundSystemCallNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    match system_call.base {
        SystemCallKind::Print | SystemCallKind::ToString | SystemCallKind::DebugHeapDump => {
            let mut result = vec![];
            let argument_count = match system_call.base {
                SystemCallKind::Print
                | SystemCallKind::ToString
                | SystemCallKind::ArrayLength
                | SystemCallKind::DebugHeapDump => 1,
            };

            for argument in system_call.arguments {
                result.append(&mut convert_node(argument, converter));
            }
            result.push(
                Instruction::load_immediate(argument_count)
                    .span(span)
                    .into(),
            );
            result.push(Instruction::system_call(system_call.base).span(span).into());
            result
        }
        SystemCallKind::ArrayLength => {
            convert_array_length_system_call(span, system_call, converter)
        }
    }
}

fn convert_array_length_system_call(
    span: TextSpan,
    mut system_call: BoundSystemCallNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = vec![];
    let base = system_call.arguments.pop().unwrap();
    let is_closure = matches!(base.type_, Type::Closure(_));
    result.append(&mut convert_node(base, converter));
    if is_closure {
        result.push(Instruction::decode_closure(1, false).span(span).into());
    } else {
        result.push(Instruction::load_immediate(1).span(span).into());
    }

    result.push(Instruction::system_call(system_call.base).span(span).into());
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
    match field_access.type_ {
        Type::SystemCall(_) => {}
        Type::Function(_) => {
            result.push(
                Instruction::load_register(field_access.offset)
                    .span(span)
                    .into(),
            );
        }
        _ => {
            result.push(
                Instruction::read_word_with_offset(field_access.offset)
                    .span(span)
                    .into(),
            );
        }
    }
    result
}

fn convert_closure(
    span: TextSpan,
    closure: BoundClosureNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = vec![];
    let arguments = closure.arguments();
    let is_system_call = matches!(closure.function, FunctionKind::SystemCall(_));
    let size_in_words = if is_system_call {
        arguments.len()
    } else {
        1 + arguments.len()
    } as u64;
    let size_in_bytes = size_in_words * WORD_SIZE_IN_BYTES;

    for argument in arguments.into_iter().rev() {
        result.append(&mut convert_node(argument, converter));
    }
    match closure.function {
        FunctionKind::FunctionId(id) => {
            result.push(Instruction::load_register(id).span(span).into());
        }
        FunctionKind::SystemCall(_) => {}
    }
    result.push(Instruction::load_immediate(size_in_bytes).span(span).into());
    result.push(
        Instruction::write_to_heap(size_in_words + 1)
            .span(span)
            .into(),
    );
    result
}

fn convert_conversion(
    span: TextSpan,
    conversion: BoundConversionNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let conversion_kind = conversion.kind();
    let base_type = conversion.base.type_.clone();
    let mut result = convert_node(*conversion.base, converter);
    match conversion_kind {
        ConversionKind::None => {}
        ConversionKind::TypeBoxing => {
            let type_word_count = convert_type_identifier(base_type, &mut result);
            result.push(
                Instruction::write_to_heap(type_word_count + 1)
                    .span(span)
                    .into(),
            );
        }
        ConversionKind::TypeUnboxing => {
            let type_word_count = convert_type_identifier(
                conversion.type_.noneable_base_type().unwrap().clone(),
                &mut result,
            );
            // Write to heap to keep code simpler. This can be optimized later!
            result.push(
                Instruction::write_to_heap(type_word_count)
                    .span(span)
                    .into(),
            );
            result.push(Instruction::type_identifier_equals().span(span).into());
            let label = converter.generate_label();
            result.push(
                Instruction::jump_to_label_conditionally(label, false)
                    .span(span)
                    .into(),
            );
            // If types are equal, the value of the type needs to be converted into a noneable
            if !base_type.is_pointer() {
                result.push(Instruction::write_to_heap(1).span(span).into());
            }
            result.push(Instruction::label(label).span(span).into());
        }
        ConversionKind::Boxing => {
            result.push(Instruction::write_to_heap(1).span(span).into());
        }
        ConversionKind::Unboxing => {
            result.push(Instruction::read_word_with_offset(0).span(span).into());
        }
    }
    result
}

fn convert_type_identifier(base_type: Type, result: &mut Vec<InstructionOrLabelReference>) -> u64 {
    let size_in_words = base_type.type_identifier_size_in_words();
    let size_in_bytes = size_in_words * WORD_SIZE_IN_BYTES;
    let type_identifier_kind = base_type.type_identifier_kind();
    let type_word_count = match base_type {
        Type::Array(base_type) => {
            let mut base_type = *base_type;
            let mut dimension_count = 0;
            while let Type::Array(inner) = base_type {
                base_type = *inner;
                dimension_count += 1;
            }
            let type_word_count = convert_type_identifier(base_type, result) + 3;
            result.push(Instruction::load_immediate(dimension_count).into());
            type_word_count
        }
        Type::Noneable(base_type) => convert_type_identifier(*base_type, result) + 2,
        Type::Function(function_type) => {
            let mut type_word_count = 3;
            let parameter_count = function_type.parameter_types.len() as u64;
            // ParameterTypes
            for parameter in function_type.parameter_types.into_iter().rev() {
                type_word_count += convert_type_identifier(parameter, result);
            }
            // ReturnType
            type_word_count += convert_type_identifier(function_type.return_type, result);
            // ThisType or Void
            type_word_count +=
                convert_type_identifier(function_type.this_type.unwrap_or(Type::Void), result);
            // ParameterCount + 2
            result.push(Instruction::load_immediate(parameter_count + 2).into());
            type_word_count
        }
        Type::Closure(closure_type) => {
            let mut type_word_count = 3;
            let parameter_count = closure_type.base_function_type.parameter_types.len() as u64;
            // ParameterTypes
            for parameter in closure_type
                .base_function_type
                .parameter_types
                .into_iter()
                .rev()
            {
                type_word_count += convert_type_identifier(parameter, result);
            }
            // ReturnType
            type_word_count +=
                convert_type_identifier(closure_type.base_function_type.return_type, result);
            // ThisType or Void
            type_word_count += convert_type_identifier(
                closure_type
                    .base_function_type
                    .this_type
                    .unwrap_or(Type::Void),
                result,
            );
            // ParameterCount + 2
            result.push(Instruction::load_immediate(parameter_count + 2).into());
            type_word_count
        }
        Type::Struct(struct_type) => {
            result.push(Instruction::load_immediate(struct_type.id).into());
            3
        }
        Type::StructReference(id) => {
            result.push(Instruction::load_immediate(id).into());
            3
        }
        _ => 2,
    };
    result.push(Instruction::load_immediate(type_identifier_kind).into());
    result.push(Instruction::load_immediate(size_in_bytes).into());
    type_word_count
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
