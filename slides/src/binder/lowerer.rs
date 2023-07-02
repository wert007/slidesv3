use crate::{
    binder::{bound_nodes::BoundNodeKind, typing::SystemCallKind},
    text::TextLocation,
    value::Value,
    Project,
};

use super::{bound_nodes::*, typing::TypeId};

struct Flattener<'a> {
    pub label_count: usize,
    pub project: &'a mut Project,
}

impl<'a> Flattener<'a> {
    pub fn new(label_count: usize, project: &'a mut Project) -> Self {
        Self {
            label_count,
            project,
        }
    }
    pub fn next_label_index(&mut self) -> usize {
        let label = self.label_count;
        self.label_count += 1;
        label
    }
}

pub fn flatten(node: BoundNode, label_count: &mut usize, project: &mut Project) -> Vec<BoundNode> {
    let mut flattener = Flattener::new(*label_count, project);
    let result = flatten_node(node, &mut flattener);
    *label_count = flattener.label_count;
    result
}

fn flatten_node(node: BoundNode, flattener: &mut Flattener) -> Vec<BoundNode> {
    match node.kind {
        BoundNodeKind::VariableDeclaration(variable_declaration) => {
            vec![flatten_variable_declaration(
                node.location,
                node.type_,
                variable_declaration,
                flattener,
            )]
        }
        BoundNodeKind::Assignment(assignment) => vec![flatten_assignment(
            node.location,
            node.type_,
            assignment,
            flattener,
        )],
        BoundNodeKind::ReturnStatement(return_statement) => vec![flatten_return_statement(
            node.location,
            node.type_,
            return_statement,
            flattener,
        )],
        BoundNodeKind::ExpressionStatement(expression_statement) => {
            vec![flatten_expression_statement(
                node.location,
                node.type_,
                expression_statement,
                flattener,
            )]
        }
        BoundNodeKind::FieldAccess(field_access) => vec![flatten_field_access(
            node.location,
            node.type_,
            field_access,
            flattener,
        )],
        BoundNodeKind::Closure(closure) => {
            vec![flatten_closure(
                node.location,
                node.type_,
                closure,
                flattener,
            )]
        }
        BoundNodeKind::Conversion(conversion) => vec![flatten_conversion(
            node.location,
            node.type_,
            conversion,
            flattener,
        )],
        BoundNodeKind::ArrayIndex(array_index) => vec![flatten_array_index(
            node.location,
            node.type_,
            array_index,
            flattener,
        )],
        BoundNodeKind::SystemCall(system_call) => vec![flatten_system_call(
            node.location,
            node.type_,
            system_call,
            flattener,
        )],
        BoundNodeKind::FunctionCall(function_call) => vec![flatten_function_call(
            node.location,
            node.type_,
            function_call,
            flattener,
        )],
        BoundNodeKind::BinaryExpression(binary_expression) => vec![flatten_binary_expression(
            node.location,
            node.type_,
            binary_expression,
            flattener,
        )],
        BoundNodeKind::UnaryExpression(unary_expression) => vec![flatten_unary_expression(
            node.location,
            node.type_,
            unary_expression,
            flattener,
        )],
        BoundNodeKind::ConstructorCall(constructor_call) => vec![flatten_constructor_call(
            node.location,
            node.type_,
            *constructor_call,
            flattener,
        )],
        BoundNodeKind::ArrayLiteralExpression(array_literal_expression) => {
            vec![flatten_array_literal_expression(
                node.location,
                node.type_,
                array_literal_expression,
                flattener,
            )]
        }
        BoundNodeKind::RepetitionNode(repetition_node) => vec![flatten_repetition_node(
            node.location,
            node.type_,
            repetition_node,
            flattener,
        )],
        BoundNodeKind::VariableExpression(_)
        | BoundNodeKind::LiteralExpression(_)
        | BoundNodeKind::Label(_)
        | BoundNodeKind::LabelReference(_)
        | BoundNodeKind::ErrorExpression => vec![node],
        BoundNodeKind::FunctionDeclaration(function_declaration) => {
            flatten_function_declaration(function_declaration, flattener)
        }
        BoundNodeKind::BlockStatement(block_statement) => {
            flatten_block_statement(block_statement, flattener)
        }
        BoundNodeKind::IfStatement(if_statement) => flatten_if_statement(if_statement, flattener),
        BoundNodeKind::WhileStatement(while_statement) => {
            flatten_while_statement(while_statement, flattener)
        }
        BoundNodeKind::MatchStatement(match_statement) => {
            flatten_match_statement(node.location, match_statement, flattener)
        }
        BoundNodeKind::Jump(jump) => flatten_jump(node.location, jump, flattener),
    }
}

fn flatten_expression(node: BoundNode, flattener: &mut Flattener) -> BoundNode {
    match node.kind {
        BoundNodeKind::VariableDeclaration(variable_declaration) => {
            flatten_variable_declaration(node.location, node.type_, variable_declaration, flattener)
        }
        BoundNodeKind::Assignment(assignment) => {
            flatten_assignment(node.location, node.type_, assignment, flattener)
        }
        BoundNodeKind::ReturnStatement(return_statement) => {
            flatten_return_statement(node.location, node.type_, return_statement, flattener)
        }
        BoundNodeKind::ExpressionStatement(expression_statement) => {
            flatten_expression_statement(node.location, node.type_, expression_statement, flattener)
        }
        BoundNodeKind::FieldAccess(field_access) => {
            flatten_field_access(node.location, node.type_, field_access, flattener)
        }
        BoundNodeKind::Closure(closure) => {
            flatten_closure(node.location, node.type_, closure, flattener)
        }
        BoundNodeKind::Conversion(conversion) => {
            flatten_conversion(node.location, node.type_, conversion, flattener)
        }
        BoundNodeKind::ArrayIndex(array_index) => {
            flatten_array_index(node.location, node.type_, array_index, flattener)
        }
        BoundNodeKind::SystemCall(system_call) => {
            flatten_system_call(node.location, node.type_, system_call, flattener)
        }
        BoundNodeKind::FunctionCall(function_call) => {
            flatten_function_call(node.location, node.type_, function_call, flattener)
        }
        BoundNodeKind::BinaryExpression(binary_expression) => {
            flatten_binary_expression(node.location, node.type_, binary_expression, flattener)
        }
        BoundNodeKind::UnaryExpression(unary_expression) => {
            flatten_unary_expression(node.location, node.type_, unary_expression, flattener)
        }
        BoundNodeKind::ConstructorCall(constructor_call) => {
            flatten_constructor_call(node.location, node.type_, *constructor_call, flattener)
        }
        BoundNodeKind::ArrayLiteralExpression(array_literal_expression) => {
            flatten_array_literal_expression(
                node.location,
                node.type_,
                array_literal_expression,
                flattener,
            )
        }
        BoundNodeKind::VariableExpression(_)
        | BoundNodeKind::LiteralExpression(_)
        | BoundNodeKind::Label(_)
        | BoundNodeKind::LabelReference(_)
        | BoundNodeKind::ErrorExpression => node,
        BoundNodeKind::WhileStatement(_) => unreachable!("WhileStatements are no expressions!"),
        BoundNodeKind::MatchStatement(_) => unreachable!("MatchStatements are no expressions!"),
        BoundNodeKind::FunctionDeclaration(_) => {
            unreachable!("FunctionDeclarations are no expressions!")
        }
        BoundNodeKind::Jump(_) => unreachable!("Only the lowerer itself can emit Jump bound nodes"),
        BoundNodeKind::BlockStatement(block_statement) => BoundNode::block_expression(
            node.location,
            flatten_block_statement(block_statement, flattener),
            node.type_,
        ),
        BoundNodeKind::IfStatement(if_statement) => BoundNode::block_expression(
            node.location,
            flatten_if_statement(if_statement, flattener),
            node.type_,
        ),
        BoundNodeKind::RepetitionNode(repetition_node) => {
            flatten_repetition_node(node.location, node.type_, repetition_node, flattener)
        }
    }
}

fn flatten_variable_declaration(
    span: TextLocation,
    _type_: TypeId,
    variable_declaration: BoundVariableDeclarationNodeKind,
    flattener: &mut Flattener,
) -> BoundNode {
    let initializer = flatten_expression(*variable_declaration.initializer, flattener);
    BoundNode::variable_declaration(
        span,
        variable_declaration.variable_index,
        initializer,
        Some(variable_declaration.variable_type),
    )
}

fn flatten_assignment(
    span: TextLocation,
    _type_: TypeId,
    assignment: BoundAssignmentNodeKind,
    flattener: &mut Flattener,
) -> BoundNode {
    let lhs = flatten_expression(*assignment.variable, flattener);
    let expression = flatten_expression(*assignment.expression, flattener);
    BoundNode::assignment(span, lhs, expression)
}

fn flatten_return_statement(
    span: TextLocation,
    _type_: TypeId,
    return_statement: BoundReturnStatementNodeKind,
    flattener: &mut Flattener,
) -> BoundNode {
    let expression = return_statement
        .expression
        .map(|r| flatten_expression(*r, flattener));
    BoundNode::return_statement(span, expression, return_statement.restores_variables)
}

fn flatten_expression_statement(
    span: TextLocation,
    _type_: TypeId,
    expression_statement: BoundExpressionStatementNodeKind,
    flattener: &mut Flattener,
) -> BoundNode {
    let expression = flatten_expression(*expression_statement.expression, flattener);
    BoundNode::expression_statement(span, expression)
}

fn flatten_field_access(
    span: TextLocation,
    _type_: TypeId,
    field_access: BoundFieldAccessNodeKind,
    flattener: &mut Flattener,
) -> BoundNode {
    let base = flatten_expression(*field_access.base, flattener);
    BoundNode::field_access(span, base, field_access.offset, field_access.type_)
}

fn flatten_closure(
    span: TextLocation,
    type_: TypeId,
    closure: BoundClosureNodeKind,
    flattener: &mut Flattener,
) -> BoundNode {
    let mut arguments = Vec::with_capacity(closure.arguments.len());
    for argument in closure.arguments {
        arguments.push(flatten_expression(argument, flattener));
    }
    match closure.function {
        super::typing::FunctionKind::FunctionId(id) => {
            BoundNode::closure(span, arguments, id, type_)
        }
        super::typing::FunctionKind::SystemCall(system_call_kind) => {
            BoundNode::system_call_closure(span, arguments, system_call_kind, type_)
        }
        super::typing::FunctionKind::LabelReference(label_index) => {
            BoundNode::closure_label(span, arguments, label_index, type_)
        }
        super::typing::FunctionKind::VtableIndex(vtable_index) => {
            BoundNode::closure_abstract(span, arguments, vtable_index, type_)
        }
    }
}

fn flatten_conversion(
    span: TextLocation,
    _type_: TypeId,
    conversion: BoundConversionNodeKind,
    flattener: &mut Flattener,
) -> BoundNode {
    let base = flatten_expression(*conversion.base, flattener);
    BoundNode::conversion(span, base, conversion.type_)
}

fn flatten_array_index(
    span: TextLocation,
    type_: TypeId,
    array_index: BoundArrayIndexNodeKind,
    flattener: &mut Flattener,
) -> BoundNode {
    let base = flatten_expression(*array_index.base, flattener);
    let index = flatten_expression(*array_index.index, flattener);
    BoundNode::array_index(span, base, index, type_)
}

fn flatten_system_call(
    span: TextLocation,
    type_: TypeId,
    system_call: BoundSystemCallNodeKind,
    flattener: &mut Flattener,
) -> BoundNode {
    let mut arguments = Vec::with_capacity(system_call.arguments.len());
    for argument in system_call.arguments {
        arguments.push(flatten_expression(argument, flattener));
    }
    BoundNode::system_call(span, system_call.base, arguments, type_)
}

fn flatten_function_call(
    span: TextLocation,
    type_: TypeId,
    function_call: BoundFunctionCallNodeKind,
    flattener: &mut Flattener,
) -> BoundNode {
    let base = flatten_expression(*function_call.base, flattener);
    let mut arguments = Vec::with_capacity(function_call.arguments.len());
    for argument in function_call.arguments {
        arguments.push(flatten_expression(argument, flattener));
    }
    BoundNode::function_call(
        span,
        base,
        arguments,
        function_call.has_this_argument,
        type_,
    )
}

fn flatten_binary_expression(
    span: TextLocation,
    type_: TypeId,
    binary_expression: BoundBinaryNodeKind,
    flattener: &mut Flattener,
) -> BoundNode {
    let lhs = flatten_expression(*binary_expression.lhs, flattener);
    let rhs = flatten_expression(*binary_expression.rhs, flattener);
    BoundNode::binary(span, lhs, binary_expression.operator_token, rhs, type_)
}

fn flatten_unary_expression(
    span: TextLocation,
    type_: TypeId,
    unary_expression: BoundUnaryNodeKind,
    flattener: &mut Flattener,
) -> BoundNode {
    let operand = flatten_expression(*unary_expression.operand, flattener);
    BoundNode::unary(span, unary_expression.operator_token, operand, type_)
}

fn flatten_constructor_call(
    span: TextLocation,
    _type_: TypeId,
    constructor_call: BoundConstructorCallNodeKind,
    flattener: &mut Flattener,
) -> BoundNode {
    let mut arguments = Vec::with_capacity(constructor_call.arguments.len());
    for argument in constructor_call.arguments {
        arguments.push(flatten_expression(argument, flattener));
    }
    BoundNode::constructor_call(
        span,
        arguments,
        constructor_call.base_type,
        constructor_call.function,
    )
}

fn flatten_array_literal_expression(
    span: TextLocation,
    type_: TypeId,
    array_literal_expression: BoundArrayLiteralNodeKind,
    flattener: &mut Flattener,
) -> BoundNode {
    let mut children = Vec::with_capacity(array_literal_expression.children.len());
    for child in array_literal_expression.children {
        children.push(flatten_expression(child, flattener));
    }
    BoundNode::array_literal(span, children, type_)
}

fn flatten_repetition_node(
    location: TextLocation,
    _type_: TypeId,
    repetition_node: BoundRepetitionNodeKind,
    flattener: &mut Flattener,
) -> BoundNode {
    let expression = flatten_expression(*repetition_node.expression, flattener);
    let repetition = flatten_expression(*repetition_node.repetition, flattener);
    BoundNode::repetition_node(
        location,
        repetition_node.counting_variable,
        expression,
        repetition,
    )
}

fn flatten_function_declaration(
    _function_declaration: BoundFunctionDeclarationNodeKind,
    _flattener: &mut Flattener,
) -> Vec<BoundNode> {
    // let mut _result = vec![BoundNode::label(todo!("Location"), function_declaration.label)];
    // TODO: assign parameters here!
    unimplemented!();
    // result.append(&mut flatten_node(*function_declaration.body, flattener));
    // result
}

fn flatten_block_statement(
    block_statement: BoundBlockStatementNodeKind,
    flattener: &mut Flattener,
) -> Vec<BoundNode> {
    let mut result = Vec::with_capacity(block_statement.statements.len());
    for statement in block_statement.statements {
        result.append(&mut flatten_node(statement, flattener));
    }
    result
}

fn flatten_if_statement(
    if_statement: BoundIfStatementNodeKind,
    flattener: &mut Flattener,
) -> Vec<BoundNode> {
    let mut result = vec![];
    let (end_label, end_label_ref) = create_label(if_statement.condition.location, flattener);
    let (else_label, else_label_ref) = if if_statement.else_body.is_some() {
        create_label(if_statement.else_body.as_ref().unwrap().location, flattener)
    } else {
        (end_label.clone(), end_label_ref.clone())
    };
    let condition_span = if_statement.condition.location;
    let condition_type = if_statement.condition.type_.clone();
    result.push(BoundNode::jump_if_false(
        condition_span,
        BoundNode::block_expression(
            condition_span,
            flatten_node(*if_statement.condition, flattener),
            condition_type,
        ),
        else_label_ref,
    ));
    result.append(&mut flatten_node(*if_statement.body, flattener));
    if if_statement.else_body.is_some() {
        result.push(BoundNode::jump(
            if_statement.else_body.as_ref().unwrap().location,
            end_label_ref,
        ));
        result.push(else_label);
    }
    if let Some(else_body) = if_statement.else_body {
        result.append(&mut flatten_node(*else_body, flattener));
    }
    result.push(end_label);
    result
}

fn flatten_while_statement(
    while_statement: BoundWhileStatementNodeKind,
    flattener: &mut Flattener,
) -> Vec<BoundNode> {
    let mut result = vec![];
    let (label_repeat, label_repeat_ref) = create_label(while_statement.body.location, flattener);
    let (label_skip, label_skip_ref) = create_label(while_statement.body.location, flattener);
    result.push(BoundNode::jump_if_false(
        while_statement.condition.location,
        *while_statement.condition.clone(),
        label_skip_ref,
    ));
    result.push(label_repeat);
    result.append(&mut flatten_node(*while_statement.body, flattener));
    result.push(BoundNode::jump_if_true(
        while_statement.condition.location,
        *while_statement.condition,
        label_repeat_ref,
    ));
    result.push(label_skip);
    result
}

#[allow(unused_variables)]
fn flatten_match_statement(
    location: TextLocation,
    match_statement: BoundMatchStatementNodeKind,
    flattener: &mut Flattener,
) -> Vec<BoundNode> {
    let (end_label, end_label_reference) = create_label(location, flattener);
    let mut is_type_match = false;
    let (values, mut bodies): (Vec<(BoundNode, BoundNode)>, Vec<BoundNode>) = match_statement
        .cases
        .into_iter()
        .map(|c| {
            let (case_label, case_label_reference) = create_label(location, flattener);
            let expression = match *c.expression {
                BoundMatchCaseExpression::Expression(e) => e,
                BoundMatchCaseExpression::Type(_, t, location) => {
                    is_type_match = true;
                    BoundNode::literal(location, Value::TypeId(t))
                }
            };
            (
                (expression, case_label_reference),
                BoundNode::block_statement(
                    location,
                    vec![
                        case_label,
                        *c.body,
                        BoundNode::jump(location, end_label_reference.clone()),
                    ],
                ),
            )
        })
        .unzip();
    let dictionary_type = flattener
        .project
        .types
        .look_up_type_by_name(&format!(
            "Dict<{}, ptr, >",
            if is_type_match {
                "type".into()
            } else {
                flattener
                    .project
                    .types
                    .name_of_type_id(match_statement.expression.type_)
            }
        ))
        .expect("There needs to exists a Dict<Range, pointer, > for match statements to work...")
        .unwrap_type_id();
    let dictionary = BoundNode::dictionary_literal(
        location,
        match_statement.temporary_variable,
        values,
        dictionary_type,
        &flattener.project.types,
    );
    let dictionary_get_function = flattener.project.types[dictionary_type]
        .as_struct_type()
        .unwrap()
        .member_functions_first("get", &flattener.project.types)
        .unwrap()
        .offset_or_address
        .unwrap_address();
    let (default_case_label, mut default_case_label_reference) = create_label(location, flattener);
    default_case_label_reference.type_ = typeid!(Type::Pointer);
    let expression = if is_type_match {
        BoundNode::system_call(
            location,
            SystemCallKind::TypeOfValue,
            vec![BoundNode::conversion(
                location,
                *match_statement.expression,
                typeid!(Type::Any),
            )],
            typeid!(Type::TypeId),
        )
    } else {
        *match_statement.expression
    };
    let mut statements = vec![BoundNode::jump(
        location,
        BoundNode::binary(
            location,
            BoundNode::function_call(
                location,
                BoundNode::label_reference(location, dictionary_get_function, typeid!(Type::Error)),
                vec![expression, dictionary],
                false,
                flattener
                    .project
                    .types
                    .look_up_type_by_name("Noneable<ptr, >")
                    .unwrap()
                    .unwrap_type_id(),
            ),
            super::operators::BoundBinaryOperator::NoneableOrValue,
            BoundNode::conversion(
                location,
                default_case_label_reference,
                flattener
                    .project
                    .types
                    .look_up_type_by_name("Noneable<ptr, >")
                    .unwrap()
                    .unwrap_type_id(),
            ),
            flattener
                .project
                .types
                .look_up_type_by_name("ptr")
                .unwrap()
                .unwrap_type_id(),
        ),
    )];
    statements.append(&mut bodies);
    statements.push(default_case_label);
    if let Some(default_case) = match_statement.default_case {
        statements.push(*default_case);
    }
    statements.push(end_label);
    flatten_node(BoundNode::block_statement(location, statements), flattener)
}

fn flatten_jump(
    location: TextLocation,
    jump: BoundJumpNodeKind,
    flattener: &mut Flattener<'_>,
) -> Vec<BoundNode> {
    let condition = jump.condition.map(|c| flatten_expression(*c, flattener));
    let target = flatten_expression(*jump.target, flattener);
    vec![if let Some(condition) = condition {
        if jump.jump_if_true {
            BoundNode::jump_if_true(location, condition, target)
        } else {
            BoundNode::jump_if_false(location, condition, target)
        }
    } else {
        BoundNode::jump(location, target)
    }]
}

fn create_label(location: TextLocation, flattener: &mut Flattener) -> (BoundNode, BoundNode) {
    let index = flattener.next_label_index();
    (
        BoundNode::label(location, index),
        BoundNode::label_reference(location, index, typeid!(Type::Error)),
    )
}
