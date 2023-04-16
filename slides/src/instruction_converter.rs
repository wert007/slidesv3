pub mod instruction;
// #[cfg(test)]
// mod tests;

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

    pub fn span(&self) -> Option<TextLocation> {
        match self {
            InstructionOrLabelReference::Instruction(instruction) => instruction.location,
            InstructionOrLabelReference::LabelReference(label) => Some(label.location),
        }
    }

    pub fn span_mut(&mut self) -> Option<&mut TextLocation> {
        match self {
            InstructionOrLabelReference::Instruction(instruction) => instruction.location.as_mut(),
            InstructionOrLabelReference::LabelReference(label) => Some(&mut label.location),
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
        memory::static_memory::print_static_memory_as_string(&converter.static_memory);
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
        | BoundNodeKind::ErrorExpression => unreachable!(),
        BoundNodeKind::FunctionDeclaration(function_declaration) => {
            convert_function_declaration(node.location, function_declaration, converter)
        }
        BoundNodeKind::LiteralExpression(literal) => {
            convert_literal(node.location, literal, converter)
        }
        BoundNodeKind::ArrayLiteralExpression(array_literal) => {
            convert_array_literal(node.location, array_literal, converter)
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
    span: TextLocation,
    function_declaration: BoundFunctionDeclarationNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = vec![Instruction::label(function_declaration.label)
        .location(span)
        .into()];
    for parameter in function_declaration.parameters.into_iter().rev() {
        result.push(
            Instruction::store_in_register(parameter)
                .location(span)
                .into(),
        );
    }
    if let Some(register) = function_declaration.base_register {
        result.push(Instruction::load_register(0).location(span).into());
        result.push(
            Instruction::store_in_register(register)
                .location(span)
                .into(),
        );
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
        Value::None => return vec![Instruction::load_none_pointer().location(location).into()],
        Value::LabelPointer(label_reference, _) => {
            return vec![LabelReference {
                label_reference,
                location,
            }
            .into()]
        }
        Value::EnumType(_, _) => {
            todo!("This is probably unreachable? This should be meaningless at least.")
        }
    };
    vec![Instruction::load_immediate(value).location(location).into()]
}

fn convert_string_literal(
    span: TextLocation,
    value: String,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let pointer = converter.static_memory.allocate_string(value);
    vec![Instruction::load_pointer(pointer).location(span).into()]
}

fn convert_array_literal(
    span: TextLocation,
    array_literal: BoundArrayLiteralNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = vec![];
    let element_count = array_literal.children.len() as _;
    for child in array_literal.children.into_iter().rev() {
        result.append(&mut convert_node(child, converter));
    }
    result.push(
        Instruction::write_to_heap(element_count)
            .location(span)
            .into(),
    );
    result.push(
        Instruction::load_immediate(element_count)
            .location(span)
            .into(),
    );
    result.push(Instruction::write_to_heap(2).location(span).into());
    result
}

fn convert_variable(
    span: TextLocation,
    variable: BoundVariableNodeKind,
) -> Vec<InstructionOrLabelReference> {
    vec![Instruction::load_register(variable.variable_index)
        .location(span)
        .into()]
}

fn convert_variable_for_assignment(
    span: TextLocation,
    variable: BoundVariableNodeKind,
) -> Vec<InstructionOrLabelReference> {
    vec![Instruction::store_in_register(variable.variable_index)
        .location(span)
        .into()]
}

fn convert_array_index_for_assignment(
    span: TextLocation,
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
    result.push(
        Instruction::load_immediate(base_type_size)
            .location(span)
            .into(),
    );
    result.push(Instruction::multiplication().location(span).into());

    result.push(Instruction::store_in_memory().location(span).into());
    result
}

fn convert_field_access_for_assignment(
    span: TextLocation,
    field_access: BoundFieldAccessNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = convert_node(*field_access.base, converter);
    result.push(
        Instruction::load_immediate(field_access.offset)
            .location(span)
            .into(),
    );
    result.push(Instruction::store_in_memory().location(span).into());
    result
}

fn convert_unary(
    span: TextLocation,
    unary: BoundUnaryNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let is_pointer = converter.project.types[unary.operand.type_].is_pointer();
    let mut result = convert_node(*unary.operand, converter);
    match unary.operator_token {
        BoundUnaryOperator::ArithmeticNegate => {
            result.push(Instruction::twos_complement().location(span).into())
        }
        BoundUnaryOperator::ArithmeticIdentity => {}
        BoundUnaryOperator::LogicalNegation if is_pointer => {
            result.push(Instruction::load_none_pointer().location(span).into());
            result.push(Instruction::noneable_equals(1).location(span).into());
        }
        BoundUnaryOperator::LogicalNegation => {
            result.push(Instruction::load_immediate(0).location(span).into());
            result.push(Instruction::equals().location(span).into());
        }
    }
    result
}

fn convert_binary(
    span: TextLocation,
    binary: BoundBinaryNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let operator_instruction = match binary.operator_token {
        BoundBinaryOperator::ArithmeticAddition => Instruction::addition(),
        BoundBinaryOperator::ArithmeticSubtraction => Instruction::subtraction(),
        BoundBinaryOperator::ArithmeticMultiplication => Instruction::multiplication(),
        BoundBinaryOperator::ArithmeticDivision => Instruction::division(),
        BoundBinaryOperator::Equals => {
            match (
                &converter.project.types[binary.lhs.type_],
                &converter.project.types[binary.rhs.type_],
            ) {
                (Type::String, Type::String) => Instruction::array_equals(),
                (Type::Noneable(base_type), Type::Noneable(_))
                    if !converter.project.types[*base_type].is_pointer() =>
                {
                    Instruction::noneable_equals(
                        converter.project.types[*base_type].size_in_bytes(),
                    )
                }
                (Type::Noneable(base_type), Type::Noneable(_))
                    if converter.project.types[*base_type].is_pointer() =>
                {
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
            if binary.lhs.type_ == typeid!(Type::String)
                && binary.rhs.type_ == typeid!(Type::String)
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
            Instruction::noneable_or_value(!converter.project.types[binary.rhs.type_].is_pointer())
        }
        operator @ (BoundBinaryOperator::LogicalAnd
        | BoundBinaryOperator::LogicalOr
        | BoundBinaryOperator::Range) => {
            unreachable!("{} is handled in the binder already!", operator)
        }
    }
    .location(span);
    let mut result = vec![];
    result.append(&mut convert_node(*binary.lhs, converter));
    result.append(&mut convert_node(*binary.rhs, converter));
    result.push(operator_instruction.into());
    result
}

fn convert_function_call(
    span: TextLocation,
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
    let is_closure = matches!(
        converter.project.types[function_call.base.type_],
        Type::Closure(_)
    );
    result.append(&mut convert_node(*function_call.base, converter));
    if is_closure {
        result.push(
            Instruction::decode_closure(argument_count as _, true)
                .location(span)
                .into(),
        );
    } else {
        result.push(
            Instruction::load_immediate(argument_count as _)
                .location(span)
                .into(),
        );
    }
    result.push(Instruction::function_call().location(span).into());
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
            result.push(
                Instruction::allocate(word_count * WORD_SIZE_IN_BYTES)
                    .location(span)
                    .into(),
            );
            for argument in constructor_call.arguments.into_iter() {
                result.append(&mut convert_node(argument, converter));
            }
            result.push(
                Instruction::duplicate_over(argument_count)
                    .location(span)
                    .into(),
            );
            result.push(
                LabelReference {
                    label_reference: function as _,
                    location: span,
                }
                .into(),
            );
            result.push(
                Instruction::load_immediate(argument_count + 1)
                    .location(span)
                    .into(),
            );
            result.push(Instruction::function_call().location(span).into());
        }
        None => {
            for argument in constructor_call.arguments.into_iter().rev() {
                result.append(&mut convert_node(argument, converter));
            }
            result.push(Instruction::write_to_heap(word_count).location(span).into());
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
        SystemCallKind::Print
        | SystemCallKind::ToString
        | SystemCallKind::HeapDump
        | SystemCallKind::RuntimeError
        | SystemCallKind::Reallocate
        | SystemCallKind::AddressOf
        | SystemCallKind::GarbageCollect => {
            let mut result = vec![];
            let argument_count = match system_call.base {
                SystemCallKind::GarbageCollect => 0,
                SystemCallKind::Print
                | SystemCallKind::ToString
                | SystemCallKind::ArrayLength
                | SystemCallKind::HeapDump
                | SystemCallKind::RuntimeError
                | SystemCallKind::AddressOf => 1,
                SystemCallKind::Reallocate => 2,
                SystemCallKind::Break => unreachable!(),
            };

            for argument in system_call.arguments {
                result.append(&mut convert_node(argument, converter));
            }
            result.push(
                Instruction::load_immediate(argument_count)
                    .location(span)
                    .into(),
            );
            result.push(
                Instruction::system_call(system_call.base)
                    .location(span)
                    .into(),
            );
            result
        }
        SystemCallKind::ArrayLength => {
            convert_array_length_system_call(span, system_call, converter)
        }
        SystemCallKind::Break => {
            vec![Instruction::breakpoint().location(span).into()]
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
        result.push(Instruction::decode_closure(1, false).location(span).into());
    } else {
        result.push(Instruction::load_immediate(1).location(span).into());
    }

    result.push(
        Instruction::system_call(system_call.base)
            .location(span)
            .into(),
    );
    result
}

fn convert_array_index(
    span: TextLocation,
    array_index: BoundArrayIndexNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let base_type_size = match &converter.project.types[array_index.base.type_] {
        Type::PointerOf(base_type) => converter.project.types[*base_type].size_in_bytes(),
        error => unreachable!("unexpected access type {:?}", error),
    };

    let mut result = convert_node(*array_index.base, converter);
    result.append(&mut convert_node(*array_index.index, converter));
    result.push(
        Instruction::load_immediate(base_type_size)
            .location(span)
            .into(),
    );
    result.push(Instruction::multiplication().location(span).into());

    result.push(Instruction::addition().location(span).into());
    result.push(Instruction::read_word_with_offset(0).location(span).into());
    result
}

fn convert_field_access(
    span: TextLocation,
    field_access: BoundFieldAccessNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = convert_node(*field_access.base, converter);
    match &converter.project.types[field_access.type_] {
        Type::SystemCall(_) => {}
        Type::Function(_) => {
            result.push(
                Instruction::load_register(field_access.offset)
                    .location(span)
                    .into(),
            );
        }
        _ => {
            result.push(
                Instruction::read_word_with_offset(field_access.offset)
                    .location(span)
                    .into(),
            );
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
            result.push(Instruction::load_register(id).location(location).into());
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
            // Read function pointer
            result.push(
                Instruction::read_word_with_offset(0)
                    .location(location)
                    .into(),
            );
            result.push(
                Instruction::read_word_with_offset(vtable_index as u64 * WORD_SIZE_IN_BYTES)
                    .location(location)
                    .into(),
            );
            // Flatten fat pointer
            result.push(Instruction::rotate(1, location).into());
            result.push(
                Instruction::read_word_with_offset(WORD_SIZE_IN_BYTES)
                    .location(location)
                    .into(),
            );
            result.push(Instruction::rotate(1, location).into());
        }
    }
    result.push(
        Instruction::load_immediate(size_in_bytes)
            .location(location)
            .into(),
    );
    result.push(
        Instruction::write_to_heap(size_in_words + 1)
            .location(location)
            .into(),
    );
    result
}

fn convert_conversion(
    location: TextLocation,
    conversion: BoundConversionNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let conversion_kind = conversion.kind(&converter.project.types);
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
            convert_type_identifier(base_type, location, &mut result);
            result.push(
                // The type id is 1 word big and the pointer to the inner value
                // is also 1 word big.
                Instruction::write_to_heap(1 + 1).location(location).into(),
            );
        }
        ConversionKind::TypeUnboxing => {
            convert_type_identifier(
                converter.project.types[conversion.type_]
                    .noneable_base_type()
                    .unwrap()
                    .clone(),
                location,
                &mut result,
            );
            // Write to heap to keep code simpler. This can be optimized later!
            result.push(
                // TypeIdentifier are now always just an u64 number as an index
                // in a big type array. So we only have 1 word big type ids.
                Instruction::write_to_heap(1).location(location).into(),
            );
            result.push(
                Instruction::type_identifier_equals()
                    .location(location)
                    .into(),
            );
            let label = converter.generate_label();
            result.push(
                Instruction::jump_to_label_conditionally(label, false)
                    .location(location)
                    .into(),
            );
            // If types are equal, the value of the type needs to be converted into a noneable
            if !converter.project.types[base_type].is_pointer() {
                result.push(Instruction::write_to_heap(1).location(location).into());
            }
            result.push(Instruction::label(label).location(location).into());
        }
        ConversionKind::Boxing => {
            result.push(Instruction::write_to_heap(1).location(location).into());
        }
        ConversionKind::Unboxing => {
            result.push(
                Instruction::read_word_with_offset(0)
                    .location(location)
                    .into(),
            );
        }
        ConversionKind::IntToUint => {
            let label_if_is_not_uint = converter.generate_label();
            let label_end_if = converter.generate_label();
            result.push(Instruction::load_immediate(0).location(location).into());
            result.push(Instruction::duplicate_over(1).location(location).into());
            result.push(Instruction::greater_than().location(location).into());
            result.push(
                Instruction::jump_to_label_conditionally(label_if_is_not_uint, true)
                    .location(location)
                    .into(),
            );
            // If the int is >= 0, it needs to be converted into a noneable
            result.push(Instruction::write_to_heap(1).location(location).into());
            result.push(
                Instruction::jump_to_label(label_end_if)
                    .location(location)
                    .into(),
            );
            result.push(
                Instruction::label(label_if_is_not_uint)
                    .location(location)
                    .into(),
            );
            result.push(Instruction::pop().location(location).into());
            result.push(Instruction::load_none_pointer().location(location).into());
            result.push(Instruction::label(label_end_if).location(location).into());
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
                    Instruction::load_none_pointer()
                } else {
                    Instruction::write_to_heap(vtable_functions.len() as u64)
                }
                .location(location)
                .into(),
            );
            // Create the fat pointer.
            result.push(Instruction::write_to_heap(2).location(location).into());
        }
    }
    result
}

fn convert_type_identifier(
    base_type: TypeId,
    span: TextLocation,
    result: &mut Vec<InstructionOrLabelReference>,
) {
    // let size_in_words = base_type.type_identifier_size_in_words();
    // let size_in_bytes = size_in_words * WORD_SIZE_IN_BYTES;
    // let type_identifier_kind = base_type.type_identifier_kind();
    // let type_word_count = match base_type {
    //     Type::Noneable(base_type) => convert_type_identifier(*base_type, span, result) + 2,
    //     Type::PointerOf(base_type) => convert_type_identifier(*base_type, span, result) + 2,
    //     Type::Function(function_type) => {
    //         let mut type_word_count = 3;
    //         let parameter_count = function_type.parameter_types.len() as u64;
    //         // ParameterTypes
    //         for parameter in function_type.parameter_types.into_iter().rev() {
    //             type_word_count += convert_type_identifier(parameter, span, result);
    //         }
    //         // ReturnType
    //         type_word_count += convert_type_identifier(function_type.return_type, span, result);
    //         // ThisType or Void
    //         type_word_count += convert_type_identifier(
    //             function_type.this_type.unwrap_or(Type::Void),
    //             span,
    //             result,
    //         );
    //         // ParameterCount + 2
    //         result.push(
    //             Instruction::load_immediate(parameter_count + 2)
    //                 .span(span)
    //                 .into(),
    //         );
    //         type_word_count
    //     }
    //     Type::Closure(closure_type) => {
    //         let mut type_word_count = 3;
    //         let parameter_count = closure_type.base_function_type.parameter_types.len() as u64;
    //         // ParameterTypes
    //         for parameter in closure_type
    //             .base_function_type
    //             .parameter_types
    //             .into_iter()
    //             .rev()
    //         {
    //             type_word_count += convert_type_identifier(parameter, span, result);
    //         }
    //         // ReturnType
    //         type_word_count +=
    //             convert_type_identifier(closure_type.base_function_type.return_type, span, result);
    //         // ThisType or Void
    //         type_word_count += convert_type_identifier(
    //             closure_type
    //                 .base_function_type
    //                 .this_type
    //                 .unwrap_or(Type::Void),
    //             span,
    //             result,
    //         );
    //         // ParameterCount + 2
    //         result.push(
    //             Instruction::load_immediate(parameter_count + 2)
    //                 .span(span)
    //                 .into(),
    //         );
    //         type_word_count
    //     }
    //     Type::Struct(struct_type) => {
    //         result.push(match struct_type.function_table.to_string_function {
    //             Some(function) => LabelReference {
    //                 label_reference: function.function_label as _,
    //                 span,
    //             }
    //             .into(),
    //             None => Instruction::load_none_pointer().span(span).into(),
    //         });
    //         result.push(
    //             Instruction::load_immediate(struct_type.id)
    //                 .span(span)
    //                 .into(),
    //         );
    //         4
    //     }
    //     Type::StructReference(id) => {
    //         result.push(match id.simple_function_table.to_string_function {
    //             Some(function) => LabelReference {
    //                 label_reference: function as _,
    //                 span,
    //             }
    //             .into(),
    //             None => Instruction::load_none_pointer().span(span).into(),
    //         });
    //         result.push(Instruction::load_immediate(id.id).span(span).into());
    //         4
    //     }
    //     _ => 2,
    // };
    // result.push(
    //     Instruction::load_immediate(type_identifier_kind)
    //         .span(span)
    //         .into(),
    // );
    // result.push(Instruction::load_immediate(size_in_bytes).span(span).into());
    result.push(
        Instruction::load_immediate(base_type.as_raw())
            .location(span)
            .into(),
    );
}

fn convert_variable_declaration(
    span: TextLocation,
    variable_declaration: BoundVariableDeclarationNodeKind,
    converter: &mut InstructionConverter,
) -> Vec<InstructionOrLabelReference> {
    let mut result = convert_node(*variable_declaration.initializer, converter);
    result.push(
        Instruction::store_in_register(variable_declaration.variable_index)
            .location(span)
            .into(),
    );
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
        result.push(Instruction::pop().location(span).into());
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
        Instruction::return_from_function(pushes_on_stack, return_statement.restores_variables)
            .location(span)
            .into(),
    );
    result
}

fn convert_label(span: TextLocation, index: usize) -> Vec<InstructionOrLabelReference> {
    vec![Instruction::label(index).location(span).into()]
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
    span: TextLocation,
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
                    .location(span)
                    .into(),
            );
            result
        }
        None => vec![Instruction::jump_to_label(label).location(span).into()],
    }
}
