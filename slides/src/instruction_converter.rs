pub mod instruction;
// #[cfg(test)]
// mod tests;

mod control_flow_analyzer;
mod label_replacer;

use std::path::PathBuf;

use crate::{
    binder::{
        self,
        bound_nodes::{
            BoundArrayIndexNodeKind, BoundArrayLiteralNodeKind, BoundAssignmentNodeKind,
            BoundBinaryNodeKind, BoundBlockStatementNodeKind, BoundClosureNodeKind,
            BoundConstructorCallNodeKind, BoundConversionNodeKind,
            BoundExpressionStatementNodeKind, BoundFieldAccessNodeKind, BoundFunctionCallNodeKind,
            BoundFunctionDeclarationNodeKind, BoundJumpNodeKind, BoundLiteralNodeKind, BoundNode,
            BoundNodeKind, BoundReturnStatementNodeKind, BoundSystemCallNodeKind,
            BoundUnaryNodeKind, BoundVariableDeclarationNodeKind, BoundVariableNodeKind,
            ConversionKind,
        },
        operators::{BoundBinaryOperator, BoundUnaryOperator},
        symbols::Library,
        typing::{FunctionKind, SystemCallKind, Type, TypeId},
    },
    debug::DebugFlags,
    diagnostics::DiagnosticBag,
    evaluator::memory::{self, bytes_to_word, static_memory::StaticMemory, WORD_SIZE_IN_BYTES},
    text::{SourceTextId, TextLocation},
    value::Value,
    Project,
};

use self::instruction::{op_codes::OpCode, Instruction};

#[derive(Debug, Clone, Copy)]
pub struct LabelReference {
    pub label_reference: usize,
    pub location: TextLocation,
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

    pub fn location(&self) -> TextLocation {
        match self {
            InstructionOrLabelReference::Instruction(instruction) => instruction.location,
            InstructionOrLabelReference::LabelReference(label) => label.location,
        }
    }

    pub fn location_mut(&mut self) -> &mut TextLocation {
        match self {
            InstructionOrLabelReference::Instruction(instruction) => &mut instruction.location,
            InstructionOrLabelReference::LabelReference(label) => &mut label.location,
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

#[derive(Debug)]
pub struct InstructionConverter<'a> {
    pub static_memory: StaticMemory,
    pub fixed_variable_count: usize,
    project: &'a mut Project,
    next_label_index: usize,
}

impl InstructionConverter<'_> {
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

pub fn convert_with_project_parameter<'a>(
    source_text: SourceTextId,
    diagnostic_bag: &mut DiagnosticBag,
    project: &'a mut Project,
) -> Program {
    let mut bound_program =
        binder::bind_program_with_project_parameter(source_text, diagnostic_bag, project);
    if diagnostic_bag.has_errors() {
        return Program::error();
    }
    let bound_node = bound_program.functions;
    let mut converter = InstructionConverter {
        static_memory: StaticMemory::new(project.debug_flags),
        fixed_variable_count: bound_program.fixed_variable_count,
        next_label_index: bound_program.label_count + 1,
        project,
    };
    converter.static_memory.allocate_null();
    let minimum_memory_relocation = converter.static_memory.size_in_bytes();
    for lib in bound_program.referenced_libraries.iter_mut() {
        let static_memory_size = if lib.is_already_loaded {
            minimum_memory_relocation
        } else {
            converter.static_memory.size_in_bytes()
        };
        for inst in lib.instructions.iter_mut() {
            if let InstructionOrLabelReference::Instruction(Instruction {
                arg,
                op_code: OpCode::LoadPointer,
                ..
            }) = inst
            {
                // None pointers are saved as u64::MAX value to not interfere
                // with the actual static memory, since none gets "allocated"
                // way later. This fixes all these pointer to point to none.
                if *arg == u64::MAX {
                    *arg = 0
                } else {
                    *arg += static_memory_size;
                }
            }
        }
        if !lib.is_already_loaded {
            converter
                .static_memory
                .insert(&mut lib.program.static_memory);
        }
    }
    let mut instructions = bound_program.startup;
    let mut foreign_instructions: Vec<_> = bound_program
        .referenced_libraries
        .into_iter()
        .map(|l| l.instructions)
        .flatten()
        .collect();
    // foreign_instructions.iter_mut().for_each(|i| {
    //     if let Some(span) = i.span_mut() {
    //         span.set_is_foreign(true);
    //     }
    // });
    instructions.append(&mut foreign_instructions);
    let entry_point = 0;
    let mut program_code = convert_node(bound_node, &mut converter);
    for inst in &mut program_code {
        if let InstructionOrLabelReference::Instruction(Instruction {
            op_code: OpCode::LoadPointer,
            arg,
            ..
        }) = inst
        {
            // All none pointers are saved as u64::MAX value to not interfere
            // with the actual static memory, since normally none gets
            // "allocated" way later. And the converter does not know if none is
            // allocated or not. This fixes all these pointer to point to none.
            if *arg == u64::MAX {
                *arg = 0;
            }
        }
    }
    instructions.append(&mut program_code);
    if converter
        .project
        .debug_flags
        .output_instructions_and_labels_to_sldasm
    {
        crate::debug::output_instructions_or_labels_with_source_code_to_sldasm(
            &converter.project.source_text_collection[source_text].file_name,
            &instructions,
            &converter.project.source_text_collection,
        );
    }

    let debug_flags = converter.project.debug_flags;
    let instructions =
        label_replacer::replace_labels(instructions, debug_flags, &mut converter.project.types);
    if converter.project.debug_flags.output_instructions_to_sldasm {
        crate::debug::output_instructions_with_source_code_to_sldasm(
            &converter.project.source_text_collection[source_text].file_name,
            &instructions,
            &converter.project.source_text_collection,
        );
    }
    if converter.project.debug_flags.print_static_memory_as_string {
        println!(
            "{:?}",
            memory::static_memory::print_static_memory_as_string(&converter.static_memory)
        );
    }
    Program {
        startup_instructions: vec![],
        instructions,
        static_memory: converter.static_memory,
        max_used_variables: bound_program.max_used_variables,
        protected_variables: converter.fixed_variable_count,
        label_count: converter.next_label_index + 1,
        entry_point,
    }
}

pub fn convert_library_with_project_parameter<'a>(
    source_text: SourceTextId,
    diagnostic_bag: &mut DiagnosticBag,
    project: &mut Project,
    import_std_lib: bool,
) -> Library {
    let bound_program = binder::bind_library_with_project_parameter(
        source_text,
        diagnostic_bag,
        project,
        import_std_lib,
    );
    if diagnostic_bag.has_errors() {
        return Library::error();
    }
    let exported_functions = bound_program.exported_functions;
    let exported_structs = bound_program.exported_structs;
    let exported_enums = bound_program.exported_enums;
    let exported_generic_structs = bound_program.exported_generic_structs;
    let mut bound_program = bound_program.program;
    let bound_node = bound_program.functions;
    let mut converter = InstructionConverter {
        static_memory: StaticMemory::new(project.debug_flags),
        fixed_variable_count: bound_program.fixed_variable_count,
        next_label_index: bound_program.label_count,
        project,
    };
    for lib in bound_program
        .referenced_libraries
        .iter_mut()
        .filter(|l| !l.is_already_loaded)
    {
        let static_memory_size = converter.static_memory.size_in_bytes();
        for inst in lib.instructions.iter_mut() {
            if let InstructionOrLabelReference::Instruction(Instruction {
                arg,
                op_code: OpCode::LoadPointer,
                ..
            }) = inst
            {
                // None pointers are saved as u64::MAX value to not interfere
                // with the actual static memory, since none gets "allocated"
                // way later. This fixes all these pointer to point to none.
                if *arg == u64::MAX {
                    *arg = 0
                } else {
                    *arg += static_memory_size;
                }
            }
        }
        converter
            .static_memory
            .insert(&mut lib.program.static_memory);
    }
    let instructions = convert_node(bound_node, &mut converter);
    if converter.project.debug_flags.print_instructions_and_labels {
        for (i, instruction) in instructions.iter().enumerate() {
            println!("  {:000}: {}", i, instruction);
        }
    }

    // let instructions = label_replacer::replace_labels(instructions, converter.project.debug_flags);
    if converter.project.debug_flags.output_instructions_to_sldasm {
        crate::debug::output_instructions_or_labels_with_source_code_to_sldasm(
            &converter.project.source_text_collection[source_text].file_name,
            &instructions,
            &converter.project.source_text_collection,
        );
    }
    let source_text = &converter.project.source_text_collection[source_text];
    Library {
        path: source_text.file_name.clone().into(),
        // FIXME: Every library should have a library statement, which also
        // specifies its name!
        // NOTE: This value is overwritten anyway by the binder.
        name: PathBuf::from(source_text.file_name.clone())
            .file_stem()
            .unwrap()
            .to_string_lossy()
            .into_owned(),
        referenced_libraries: bound_program.referenced_libraries,
        instructions,
        startup: bound_program.startup,
        program: Program {
            startup_instructions: vec![],
            instructions: vec![],
            static_memory: converter.static_memory,
            max_used_variables: bound_program.max_used_variables,
            protected_variables: converter.fixed_variable_count,
            label_count: converter.next_label_index + 1,
            entry_point: !0,
        },
        functions: exported_functions,
        structs: exported_structs,
        enums: exported_enums,
        has_errors: false,
        is_already_loaded: false,
        generic_structs: exported_generic_structs,
    }
}

fn convert_node(
    node: BoundNode,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    match node.kind {
        BoundNodeKind::IfStatement(_)
        | BoundNodeKind::WhileStatement(_)
        | BoundNodeKind::MatchStatement(_)
        | BoundNodeKind::ErrorExpression
        | BoundNodeKind::RepetitionNode(_) => unreachable!(),
        BoundNodeKind::FunctionDeclaration(function_declaration) => {
            convert_function_declaration(node.location, function_declaration, converter)
        }
        BoundNodeKind::LiteralExpression(literal) => {
            convert_literal(node.location, literal, converter)
        }
        BoundNodeKind::ArrayLiteralExpression(array_literal) => {
            convert_array_literal(node.location, node.type_, array_literal, converter)
        }
        BoundNodeKind::VariableExpression(variable) => convert_variable(node.location, variable),
        BoundNodeKind::UnaryExpression(unary) => convert_unary(node.location, unary, converter),
        BoundNodeKind::BinaryExpression(binary) => convert_binary(node.location, binary, converter),
        BoundNodeKind::FunctionCall(function_call) => {
            convert_function_call(node.location, function_call, converter)
        }
        BoundNodeKind::ConstructorCall(constructor_call) => {
            convert_constructor_call(node.location, *constructor_call, converter)
        }
        BoundNodeKind::SystemCall(system_call) => {
            convert_system_call(node.location, system_call, converter)
        }
        BoundNodeKind::ArrayIndex(array_index) => {
            convert_array_index(node.location, array_index, converter)
        }
        BoundNodeKind::FieldAccess(field_access) => {
            convert_field_access(node.location, field_access, converter)
        }
        BoundNodeKind::Closure(closure) => convert_closure(node.location, closure, converter),
        BoundNodeKind::Conversion(conversion) => {
            convert_conversion(node.location, conversion, converter)
        }
        BoundNodeKind::BlockStatement(block_statement) => {
            convert_block_statement(node.location, block_statement, converter)
        }
        BoundNodeKind::VariableDeclaration(variable_declaration) => {
            convert_variable_declaration(node.location, variable_declaration, converter)
        }
        BoundNodeKind::Assignment(assignment) => {
            convert_assignment(node.location, assignment, converter)
        }
        BoundNodeKind::ExpressionStatement(expression_statement) => {
            convert_expression_statement(node.location, expression_statement, converter)
        }
        BoundNodeKind::ReturnStatement(return_statement) => {
            convert_return_statement(node.location, return_statement, converter)
        }
        BoundNodeKind::Label(index) => convert_label(node.location, index),
        BoundNodeKind::LabelReference(label_reference) => {
            convert_label_reference(node.location, label_reference)
        }
        BoundNodeKind::Jump(jump) => convert_jump(node.location, jump, converter),
    }
}

fn convert_function_declaration(
    location: TextLocation,
    function_declaration: BoundFunctionDeclarationNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = vec![Instruction::label(function_declaration.label, location).into()];
    for parameter in function_declaration.parameters.into_iter().rev() {
        result.push(Instruction::store_in_register(parameter, location).into());
    }
    if let Some(register) = function_declaration.base_register {
        result.push(Instruction::load_register(0, location).into());
        result.push(Instruction::store_in_register(register, location).into());
    }
    result.append(&mut convert_node(*function_declaration.body, converter));
    if converter.project.debug_flags.check_stack_corruption {
        control_flow_analyzer::check_stack_usage(
            &result,
            &converter.project.types[function_declaration.function_type]
                .as_function_type()
                .unwrap(),
        );
    }
    result
}

fn convert_node_for_assignment(
    node: BoundNode,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    match node.kind {
        BoundNodeKind::VariableExpression(variable) => {
            convert_variable_for_assignment(node.location, variable)
        }
        BoundNodeKind::ArrayIndex(array_index) => {
            convert_array_index_for_assignment(node.location, array_index, converter)
        }
        BoundNodeKind::FieldAccess(field_access) => {
            convert_field_access_for_assignment(node.location, field_access, converter)
        }
        _ => unreachable!(),
    }
}

fn convert_literal(
    span: TextLocation,
    literal: BoundLiteralNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    convert_value(span, literal.value, converter)
}

pub fn convert_value(
    location: TextLocation,
    value: Value,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let value = match value {
        Value::TypeId(value) => value.as_raw(),
        Value::Integer(value) => value as u64,
        Value::Boolean(value) => {
            if value {
                1
            } else {
                0
            }
        }
        Value::SystemCall(kind) => kind as u64,
        Value::String(value) => return convert_string_literal(location, value, converter),
        Value::None => return vec![Instruction::load_none_pointer(location).into()],
        Value::LabelPointer(label_reference, _) => {
            return vec![LabelReference {
                label_reference,
                location,
            }
            .into()]
        }
        Value::EnumValue(value, _) => value as u64,
        Value::EnumType(_, _) => {
            todo!("This is probably unreachable? This should be meaningless at least.")
        }
    };
    vec![Instruction::load_immediate(value, location).into()]
}

fn convert_string_literal(
    span: TextLocation,
    value: String,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let pointer = converter.static_memory.allocate_string(value);
    vec![Instruction::load_pointer(pointer, span).into()]
}

fn convert_array_literal(
    location: TextLocation,
    type_: TypeId,
    array_literal: BoundArrayLiteralNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = vec![];
    let mut const_element_count = 0;
    let mut variable_element_count = vec![];
    for child in array_literal.children.into_iter().rev() {
        if let BoundNodeKind::RepetitionNode(repetition) = child.kind {
            let mut node = convert_node(*repetition.expression, converter);
            if let Some(count) = repetition.repetition.constant_value {
                let count = count.value.as_integer().unwrap() as u64;
                const_element_count += count;
                for _ in 0..count {
                    result.extend_from_slice(&node);
                }
            } else {
                let location = repetition.repetition.location;
                let start_label = converter.generate_label();
                result.append(&mut convert_node(*repetition.repetition.clone(), converter));
                result.push(Instruction::label(start_label, location).into());
                result.push(
                    Instruction::store_in_register(repetition.counting_variable, location).into(),
                );
                result.push(
                    Instruction::load_register(repetition.counting_variable, location).into(),
                );
                result.push(Instruction::load_immediate(0, location).into());
                result.push(Instruction::equals(location).into());
                let end_label = converter.generate_label();
                result.push(Instruction::jump_if_true(end_label as _, location).into());
                result.append(&mut node);
                result.push(
                    Instruction::load_register(repetition.counting_variable, location).into(),
                );
                result.push(Instruction::load_immediate(1, location).into());
                result.push(Instruction::subtraction(location).into());
                result.push(Instruction::jump(start_label as _, location).into());
                result.push(Instruction::label(end_label, location).into());
                variable_element_count.push(*repetition.repetition);
            }
        } else {
            const_element_count += 1;
            result.append(&mut convert_node(child, converter));
        }
    }
    let mut element_count = vec![];
    element_count.push(Instruction::load_immediate(const_element_count, location).into());
    for count in variable_element_count {
        let location = count.location;
        element_count.append(&mut convert_node(count, converter));
        element_count.push(Instruction::addition(location).into());
    }
    if element_count.len() == 1 {
        result.push(
            if const_element_count == 0 {
                Instruction::load_none_pointer(location)
            } else {
                Instruction::write_to_heap(const_element_count, location)
            }
            .into(),
        );
    } else {
        result.extend_from_slice(&element_count);
        result.push(Instruction::write_to_heap_runtime(location).into());
    }
    result.extend_from_slice(&element_count);
    let array_type = converter.project.types[type_]
        .as_struct_type()
        .unwrap()
        .generic_base_type
        .unwrap();
    match converter
        .project
        .types
        .name_of_generic_type_id(array_type)
        .as_ref()
    {
        "List" => {
            result.append(&mut element_count);
            result.push(Instruction::write_to_heap(3, location).into());
        }
        "Array" => {
            result.push(Instruction::write_to_heap(2, location).into());
        }
        err => unreachable!("Unknown type for array literal {err}!"),
    }
    result
}

fn convert_variable(
    span: TextLocation,
    variable: BoundVariableNodeKind,
) -> Vec<InstructionOrLabelReference> {
    vec![Instruction::load_register(variable.variable_index, span).into()]
}

fn convert_variable_for_assignment(
    span: TextLocation,
    variable: BoundVariableNodeKind,
) -> Vec<InstructionOrLabelReference> {
    vec![Instruction::store_in_register(variable.variable_index, span).into()]
}

fn convert_array_index_for_assignment(
    location: TextLocation,
    array_index: BoundArrayIndexNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let base_type_size = match &converter.project.types[array_index.base.type_] {
        Type::PointerOf(base_type) => converter.project.types[*base_type].size_in_bytes(),
        error => unreachable!("unexpected access type {:?}", error),
    };
    // array
    let mut result = convert_node(*array_index.base, converter);
    // index
    result.append(&mut convert_node(*array_index.index, converter));
    result.push(Instruction::load_immediate(base_type_size, location).into());
    result.push(Instruction::multiplication(location).into());
    result.push(Instruction::addition(location).into());

    match base_type_size {
        1 => {
            result.push(Instruction::store_byte_in_memory(location).into());
        }
        8 => {
            result.push(Instruction::store_word_in_memory(location).into());
        }
        err => {
            unimplemented!("Unexpected base type size {err}");
        }
    }
    result
}

fn convert_field_access_for_assignment(
    location: TextLocation,
    field_access: BoundFieldAccessNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = convert_node(*field_access.base, converter);
    result.push(Instruction::load_immediate(field_access.offset, location).into());
    result.push(Instruction::addition(location).into());
    result.push(Instruction::store_word_in_memory(location).into());
    result
}

fn convert_unary(
    span: TextLocation,
    unary: BoundUnaryNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let operand_type = unary.operand.type_;
    let mut result = convert_node(*unary.operand, converter);
    match unary.operator_token {
        BoundUnaryOperator::ArithmeticNegate => {
            result.push(Instruction::twos_complement(span).into())
        }
        BoundUnaryOperator::ArithmeticIdentity => {}
        BoundUnaryOperator::LogicalNegation
            if converter.project.types[operand_type].is_pointer() =>
        {
            if converter
                .project
                .types
                .noneable_base_type(operand_type)
                .is_some()
            {
                result.push(Instruction::read_word_with_offset(0, span).into());
            }
            result.push(Instruction::load_none_pointer(span).into());
            result.push(Instruction::equals(span).into());
        }
        BoundUnaryOperator::LogicalNegation => {
            if converter
                .project
                .types
                .noneable_base_type(operand_type)
                .is_some()
            {
                result.push(Instruction::read_word_with_offset(0, span).into());
            }
            result.push(Instruction::load_immediate(0, span).into());
            result.push(Instruction::equals(span).into());
        }
    }
    result
}

fn convert_binary(
    location: TextLocation,
    binary: BoundBinaryNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let is_unsigned = converter.project.types.is_unsigned(binary.lhs.type_);
    let operator_instruction = match binary.operator_token {
        BoundBinaryOperator::ArithmeticAddition => Instruction::addition(location),
        BoundBinaryOperator::ArithmeticSubtraction => Instruction::subtraction(location),
        BoundBinaryOperator::ArithmeticMultiplication => Instruction::multiplication(location),
        BoundBinaryOperator::ArithmeticDivision => Instruction::division(location),
        BoundBinaryOperator::Equals => {
            match (
                &converter.project.types[binary.lhs.type_],
                &converter.project.types[binary.rhs.type_],
            ) {
                (Type::String, Type::String) => Instruction::array_equals(location),
                _ => Instruction::equals(location),
            }
        }
        BoundBinaryOperator::NotEquals => {
            if binary.lhs.type_ == typeid!(Type::String)
                && binary.rhs.type_ == typeid!(Type::String)
            {
                Instruction::array_not_equals(location)
            } else {
                Instruction::not_equals(location)
            }
        }
        BoundBinaryOperator::LessThan => Instruction::less_than(location, is_unsigned),
        BoundBinaryOperator::GreaterThan => Instruction::greater_than(location, is_unsigned),
        BoundBinaryOperator::LessThanEquals => Instruction::less_than_equals(location, is_unsigned),
        BoundBinaryOperator::GreaterThanEquals => {
            Instruction::greater_than_equals(location, is_unsigned)
        }
        BoundBinaryOperator::StringConcat => Instruction::string_concat(location),
        BoundBinaryOperator::NoneableOrValue => {
            let needs_dereferencing = !converter.project.types[converter
                .project
                .types
                .noneable_base_type(binary.rhs.type_)
                .unwrap_or_else(|| {
                    panic!(
                        "Should have been converted to a noneable! Found {} instead!",
                        converter
                            .project
                            .types
                            .name_of_type_id_debug(binary.rhs.type_)
                    )
                })]
            .is_pointer();
            Instruction::noneable_or_value(needs_dereferencing, location)
        }
        operator @ (BoundBinaryOperator::LogicalAnd
        | BoundBinaryOperator::LogicalOr
        | BoundBinaryOperator::Range) => {
            unreachable!("{} is handled in the binder already!", operator)
        }
    };
    let mut result = vec![];
    result.append(&mut convert_node(*binary.lhs, converter));
    result.append(&mut convert_node(*binary.rhs, converter));
    result.push(operator_instruction.into());
    result
}

fn convert_function_call(
    location: TextLocation,
    function_call: BoundFunctionCallNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = vec![];
    let argument_count = function_call.arguments.len();
    for argument in function_call.arguments {
        result.append(&mut convert_node(argument, converter));
    }
    let closure_included_arguments = converter.project.types[function_call.base.type_]
        .as_closure_type()
        .map(|c| c.included_arguments.len());
    result.append(&mut convert_node(*function_call.base, converter));
    let argument_count = if let Some(extra_arguments) = closure_included_arguments {
        let argument_count = (extra_arguments + argument_count) as _;
        result.push(Instruction::decode_closure(argument_count, true, location).into());
        argument_count
    } else {
        let argument_count = (argument_count
            + if function_call.has_this_argument {
                1
            } else {
                0
            }) as _;
        result.push(Instruction::load_immediate(argument_count, location).into());
        argument_count
    };
    result.push(
        Instruction::function_call(location, argument_count, function_call.returns_value).into(),
    );
    result
}

fn convert_constructor_call(
    span: TextLocation,
    constructor_call: BoundConstructorCallNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = vec![];
    let word_count =
        bytes_to_word(converter.project.types[constructor_call.base_type].raw_size_in_bytes());
    match constructor_call.function {
        Some(function) => {
            let argument_count = constructor_call.arguments.len() as u64;
            result.push(Instruction::allocate(word_count * WORD_SIZE_IN_BYTES, span).into());
            for argument in constructor_call.arguments.into_iter() {
                result.append(&mut convert_node(argument, converter));
            }
            result.push(Instruction::duplicate_over(argument_count, span).into());
            result.push(
                LabelReference {
                    label_reference: function as _,
                    location: span,
                }
                .into(),
            );
            result.push(Instruction::load_immediate(argument_count + 1, span).into());
            // Technically the constructor does not return the value, it only
            // changes a value that already exists. So it has no return value.
            result.push(Instruction::function_call(span, argument_count + 1, false).into());
        }
        None => {
            for argument in constructor_call.arguments.into_iter().rev() {
                result.append(&mut convert_node(argument, converter));
            }
            result.push(Instruction::write_to_heap(word_count, span).into());
        }
    }
    result
}

fn convert_system_call(
    span: TextLocation,
    system_call: BoundSystemCallNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    match system_call.base {
        SystemCallKind::ArrayLength => {
            convert_array_length_system_call(span, system_call, converter)
        }
        SystemCallKind::Break => {
            vec![Instruction::breakpoint(span).into()]
        }
        _ => {
            let mut result = vec![];
            let argument_count = match system_call.base {
                SystemCallKind::GarbageCollect => 0,
                SystemCallKind::Print
                | SystemCallKind::ToString
                | SystemCallKind::ArrayLength
                | SystemCallKind::HeapDump
                | SystemCallKind::RuntimeError
                | SystemCallKind::AddressOf
                | SystemCallKind::Hash
                | SystemCallKind::ByteToChar
                | SystemCallKind::TypeOfValue => 1,
                SystemCallKind::Reallocate => 2,
                SystemCallKind::Break => unreachable!(),
            };

            for argument in system_call.arguments {
                result.append(&mut convert_node(argument, converter));
            }
            result.push(Instruction::load_immediate(argument_count, span).into());
            result.push(Instruction::system_call(system_call.base, span).into());
            result
        }
    }
}

fn convert_array_length_system_call(
    span: TextLocation,
    mut system_call: BoundSystemCallNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = vec![];
    let base = system_call.arguments.pop().unwrap();
    let is_closure = matches!(converter.project.types[base.type_], Type::Closure(_));
    result.append(&mut convert_node(base, converter));
    if is_closure {
        result.push(Instruction::decode_closure(1, false, span).into());
    } else {
        result.push(Instruction::load_immediate(1, span).into());
    }

    result.push(Instruction::system_call(system_call.base, span).into());
    result
}

fn convert_array_index(
    location: TextLocation,
    array_index: BoundArrayIndexNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let base_type_size = match &converter.project.types[array_index.base.type_] {
        Type::PointerOf(base_type) => converter.project.types[*base_type].size_in_bytes(),
        error => unreachable!("unexpected access type {:?}", error),
    };

    let mut result = convert_node(*array_index.base, converter);
    result.append(&mut convert_node(*array_index.index, converter));
    result.push(Instruction::load_immediate(base_type_size, location).into());
    result.push(Instruction::multiplication(location).into());

    result.push(Instruction::addition(location).into());
    match base_type_size {
        1 => {
            result.push(Instruction::read_byte_with_offset(0, location).into());
        }
        8 => {
            result.push(Instruction::read_word_with_offset(0, location).into());
        }
        err => {
            unreachable!("Unhandled type size {err}!");
        }
    }
    result
}

fn convert_field_access(
    location: TextLocation,
    field_access: BoundFieldAccessNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let base_type = field_access.base.type_;
    let mut result = convert_node(*field_access.base, converter);
    match &converter.project.types[field_access.type_] {
        Type::SystemCall(_) => {}
        Type::Function(_) => {
            result.push(Instruction::load_register(field_access.offset, location).into());
        }
        _ => {
            if converter.project.types.is_abstract_type(base_type) {
                result.push(
                    Instruction::read_word_with_offset(2 * WORD_SIZE_IN_BYTES, location).into(),
                );
            }
            result.push(Instruction::read_word_with_offset(field_access.offset, location).into());
        }
    }
    result
}

fn convert_closure(
    location: TextLocation,
    closure: BoundClosureNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = vec![];
    let arguments = closure.arguments;
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
            result.push(Instruction::load_register(id, location).into());
        }
        FunctionKind::SystemCall(_) => {}
        FunctionKind::LabelReference(label_reference) => {
            result.push(
                LabelReference {
                    label_reference,
                    location,
                }
                .into(),
            );
        }
        FunctionKind::VtableIndex(vtable_index) => {
            // Create copy for function pointer and actual pointer to struct.
            result.push(Instruction::duplicate(location).into());
            // Read function pointer
            result.push(Instruction::read_word_with_offset(WORD_SIZE_IN_BYTES, location).into());
            result.push(
                Instruction::read_word_with_offset(
                    vtable_index as u64 * WORD_SIZE_IN_BYTES,
                    location,
                )
                .into(),
            );
            // Flatten fat pointer
            result.push(Instruction::rotate(1, location).into());
            result
                .push(Instruction::read_word_with_offset(2 * WORD_SIZE_IN_BYTES, location).into());
            result.push(Instruction::rotate(1, location).into());
        }
    }
    result.push(Instruction::load_immediate(size_in_bytes, location).into());
    result.push(Instruction::write_to_heap(size_in_words + 1, location).into());
    result
}

fn convert_conversion(
    location: TextLocation,
    conversion: BoundConversionNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let conversion_kind = conversion
        .kind(&converter.project.types)
        .expect("Did not report an error during binding!");
    let base_type = conversion.base.type_.clone();
    let base_type = if base_type == typeid!(Type::IntegerLiteral) {
        typeid!(Type::Integer(IntegerType::Signed64))
    } else {
        base_type
    };
    let mut result = convert_node(*conversion.base, converter);
    match conversion_kind {
        ConversionKind::None => {}
        ConversionKind::TypeBoxing => {
            if converter.project.types.is_abstract_type(base_type) {
                result.push(Instruction::duplicate(location).into());
                result.push(Instruction::read_word_with_offset(0, location).into());
            } else {
                convert_type_identifier(base_type, location, &mut result);
            }
            result.push(
                // The type id is 1 word big and the pointer to the inner value
                // is also 1 word big.
                Instruction::write_to_heap(1 + 1, location).into(),
            );
        }
        ConversionKind::TypeUnboxing => {
            result.push(Instruction::duplicate(location).into());
            result.push(Instruction::read_word_with_offset(0, location).into());
            convert_type_identifier(
                converter
                    .project
                    .types
                    .noneable_base_type(conversion.type_)
                    .unwrap(),
                location,
                &mut result,
            );
            result.push(Instruction::equals(location).into());
            let label = converter.generate_label();
            result.push(Instruction::jump_to_label_conditionally(label, false, location).into());
            // If types are equal, the value of the type needs to be converted into a noneable
            // result.push(Instruction::write_to_heap(1, location).into());
            result.push(Instruction::read_word_with_offset(WORD_SIZE_IN_BYTES, location).into());
            result.push(Instruction::write_to_heap(1, location).into());
            if !converter.project.types[base_type].is_pointer() {
                result.push(Instruction::write_to_heap(1, location).into());
            }
            let end_label = converter.generate_label();
            result.push(Instruction::jump(end_label as _, location).into());
            result.push(Instruction::label(label, location).into());
            result.push(Instruction::pop(location).into());
            result.push(Instruction::load_none_pointer(location).into());
            result.push(Instruction::write_to_heap(1, location).into());
            result.push(Instruction::label(end_label, location).into());
        }
        ConversionKind::Boxing => {
            if base_type != typeid!(Type::None)
                && base_type != typeid!(Type::Pointer)
                && &converter.project.types[base_type]
                    != &Type::PointerOf(
                        converter
                            .project
                            .types
                            .noneable_base_type(conversion.type_)
                            .unwrap(),
                    )
            {
                result.push(Instruction::write_to_heap(1, location).into());
            }
            result.push(Instruction::write_to_heap(1, location).into());
        }
        ConversionKind::Unboxing => {
            result.push(Instruction::read_word_with_offset(0, location).into());
            result.push(Instruction::read_word_with_offset(0, location).into());
        }
        ConversionKind::NoneableToPointer => {
            result.push(Instruction::read_word_with_offset(0, location).into());
        }
        ConversionKind::IntToUint => {
            let label_if_is_not_uint = converter.generate_label();
            let label_end_if = converter.generate_label();
            result.push(Instruction::load_immediate(0, location).into());
            result.push(Instruction::duplicate_over(1, location).into());
            result.push(Instruction::greater_than(location, false).into());
            result.push(
                Instruction::jump_to_label_conditionally(label_if_is_not_uint, true, location)
                    .into(),
            );
            // If the int is >= 0, it needs to be converted into a noneable
            result.push(Instruction::write_to_heap(1, location).into());
            result.push(Instruction::write_to_heap(1, location).into());
            result.push(Instruction::jump_to_label(label_end_if, location).into());
            result.push(Instruction::label(label_if_is_not_uint, location).into());
            result.push(Instruction::pop(location).into());
            result.push(Instruction::load_none_pointer(location).into());
            result.push(Instruction::write_to_heap(1, location).into());
            result.push(Instruction::label(label_end_if, location).into());
        }
        ConversionKind::AbstractTypeBoxing => {
            // Create the vtable
            let vtable_functions = converter.project.types[base_type]
                .as_struct_type()
                .unwrap()
                .vtable_functions(&converter.project.types);
            for &f in &vtable_functions {
                result.push(
                    LabelReference {
                        label_reference: f as usize,
                        location,
                    }
                    .into(),
                );
            }
            result.push(
                if vtable_functions.is_empty() {
                    Instruction::load_none_pointer(location)
                } else {
                    Instruction::write_to_heap(vtable_functions.len() as u64, location)
                }
                .into(),
            );
            result.push(Instruction::load_immediate(base_type.as_raw(), location).into());
            // Create the fat pointer.
            result.push(Instruction::write_to_heap(3, location).into());
        }
        ConversionKind::AbstractTypeUnboxing => {
            result.push(Instruction::duplicate(location).into());
            result.push(Instruction::read_word_with_offset(0, location).into());
            result.push(
                Instruction::load_immediate(
                    converter
                        .project
                        .types
                        .noneable_base_type(conversion.type_)
                        .expect("Expected noneable type here!")
                        .as_raw(),
                    location,
                )
                .into(),
            );
            result.push(Instruction::equals(location).into());

            let label = converter.generate_label();
            result.push(Instruction::jump_to_label_conditionally(label, false, location).into());
            // If types are equal, the value of the type needs to be converted into a noneable
            // result.push(Instruction::write_to_heap(1, location).into());
            result.push(Instruction::read_word_with_offset(16, location).into());
            result.push(Instruction::write_to_heap(1, location).into());
            result.push(Instruction::write_to_heap(1, location).into());
            let end_label = converter.generate_label();
            result.push(Instruction::jump(end_label as _, location).into());
            result.push(Instruction::label(label, location).into());
            result.push(Instruction::pop(location).into());
            result.push(Instruction::load_none_pointer(location).into());
            result.push(Instruction::write_to_heap(1, location).into());
            result.push(Instruction::label(end_label, location).into());
        }
    }
    result
}

fn convert_type_identifier(
    base_type: TypeId,
    location: TextLocation,
    result: &mut Vec<InstructionOrLabelReference>,
) {
    result.push(Instruction::load_immediate(base_type.as_raw(), location).into());
}

fn convert_variable_declaration(
    span: TextLocation,
    variable_declaration: BoundVariableDeclarationNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = convert_node(*variable_declaration.initializer, converter);
    result.push(Instruction::store_in_register(variable_declaration.variable_index, span).into());
    result
}

fn convert_assignment(
    _span: TextLocation,
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
    _span: TextLocation,
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
    span: TextLocation,
    expression_statement: BoundExpressionStatementNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let pushes_on_stack = expression_statement.expression.type_ != typeid!(Type::Void);
    let mut result = convert_node(*expression_statement.expression, converter);
    if pushes_on_stack {
        result.push(Instruction::pop(span).into());
    }
    result
}

fn convert_return_statement(
    span: TextLocation,
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
        Instruction::return_from_function(
            pushes_on_stack,
            return_statement.restores_variables,
            span,
        )
        .into(),
    );
    result
}

fn convert_label(span: TextLocation, index: usize) -> Vec<InstructionOrLabelReference> {
    vec![Instruction::label(index, span).into()]
}

fn convert_label_reference(
    location: TextLocation,
    label_reference: usize,
) -> Vec<InstructionOrLabelReference> {
    vec![LabelReference {
        label_reference,
        location,
    }
    .into()]
}

fn convert_jump(
    location: TextLocation,
    jump: BoundJumpNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut label = convert_node(*jump.target, converter);
    if label.len() == 1 {
        let label = label.pop().unwrap();
        let label = label
            .as_label()
            .unwrap_or_else(|| panic!("{:?}", label))
            .label_reference;
        match jump.condition {
            Some(condition) => {
                let mut result = convert_node(*condition, converter);
                result.push(
                    Instruction::jump_to_label_conditionally(label, jump.jump_if_true, location)
                        .into(),
                );
                result
            }
            None => vec![Instruction::jump_to_label(label, location).into()],
        }
    } else {
        label.push(Instruction::jump_dynamically(location).into());
        label
    }
}
