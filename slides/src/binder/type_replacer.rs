use super::{
    bound_nodes::{
        BoundArrayIndexNodeKind, BoundArrayLiteralNodeKind, BoundAssignmentNodeKind,
        BoundBinaryNodeKind, BoundBlockStatementNodeKind, BoundClosureNodeKind,
        BoundConstructorCallNodeKind, BoundConversionNodeKind, BoundExpressionStatementNodeKind,
        BoundFieldAccessNodeKind, BoundFunctionCallNodeKind, BoundFunctionDeclarationNodeKind,
        BoundIfStatementNodeKind, BoundJumpNodeKind, BoundNode, BoundNodeKind,
        BoundReturnStatementNodeKind, BoundSystemCallNodeKind, BoundUnaryNodeKind,
        BoundVariableDeclarationNodeKind, BoundWhileStatementNodeKind,
    },
    typing::Type,
};

pub fn replace_type_with_other_type(
    input_type: &mut Type,
    compares_to: &Type,
    replacement_type: &Type,
) {
    if input_type == compares_to {
        *input_type = replacement_type.clone();
    }
    if let Type::PointerOf(input_type) = input_type {
        replace_type_with_other_type(input_type, compares_to, replacement_type);
    }
    if let Type::Function(function_type) = input_type {
        for it in function_type.parameter_types.iter_mut() {
            replace_type_with_other_type(it, compares_to, replacement_type);
        }
        replace_type_with_other_type(
            &mut function_type.return_type,
            compares_to,
            replacement_type,
        );
        if let Some(it) = &mut function_type.this_type {
            replace_type_with_other_type(it, compares_to, replacement_type);
        }
    }
    if let Type::Struct(struct_type) = input_type {
        for it in struct_type.fields.iter_mut() {
            replace_type_with_other_type(it, compares_to, replacement_type);
        }
        for it in struct_type.functions.iter_mut() {
            replace_type_with_other_type(it, compares_to, replacement_type);
        }
    }
}

pub fn replace_generic_type_with(node: BoundNode, replacement_type: &Type) -> BoundNode {
    replace_type_with_other_type_on_node(node, &Type::GenericType, replacement_type)
}

fn replace_type_with_other_type_on_node(
    mut node: BoundNode,
    type_: &Type,
    replacement_type: &Type,
) -> BoundNode {
    replace_type_with_other_type(&mut node.type_, type_, replacement_type);
    match &mut node.kind {
        BoundNodeKind::FunctionDeclaration(function_declaration) => {
            replace_type_with_other_type_in_function_declaration(
                function_declaration,
                type_,
                replacement_type,
            )
        }
        BoundNodeKind::ErrorExpression => {}
        BoundNodeKind::Label(_) => {}
        BoundNodeKind::LabelReference(_) => {}
        BoundNodeKind::Jump(jump) => {
            replace_type_with_other_type_in_jump(jump, type_, replacement_type)
        }
        BoundNodeKind::LiteralExpression(_) => {}
        BoundNodeKind::ArrayLiteralExpression(array_literal) => {
            replace_type_with_other_type_in_array_literal(array_literal, type_, replacement_type)
        }
        BoundNodeKind::ConstructorCall(constructor_call) => {
            replace_type_with_other_type_in_constructor_call(
                constructor_call,
                type_,
                replacement_type,
            )
        }
        BoundNodeKind::VariableExpression(_) => {}
        BoundNodeKind::UnaryExpression(unary_expression) => {
            replace_type_with_other_type_in_unary_expression(
                unary_expression,
                type_,
                replacement_type,
            )
        }
        BoundNodeKind::BinaryExpression(binary_expression) => {
            replace_type_with_other_type_in_binary_expression(
                binary_expression,
                type_,
                replacement_type,
            )
        }
        BoundNodeKind::FunctionCall(function_call) => {
            replace_type_with_other_type_in_function_call(function_call, type_, replacement_type)
        }
        BoundNodeKind::SystemCall(system_call) => {
            replace_type_with_other_type_in_system_call(system_call, type_, replacement_type)
        }
        BoundNodeKind::ArrayIndex(array_index) => {
            replace_type_with_other_type_in_array_index(array_index, type_, replacement_type)
        }
        BoundNodeKind::FieldAccess(field_access) => {
            replace_type_with_other_type_in_field_access(field_access, type_, replacement_type)
        }
        BoundNodeKind::Closure(closure) => {
            replace_type_with_other_type_in_closure(closure, type_, replacement_type)
        }
        BoundNodeKind::Conversion(conversion) => {
            replace_type_with_other_type_in_conversion(conversion, type_, replacement_type)
        }
        BoundNodeKind::BlockStatement(block_statement) => {
            replace_type_with_other_type_in_block_statement(
                block_statement,
                type_,
                replacement_type,
            )
        }
        BoundNodeKind::IfStatement(if_statement) => {
            replace_type_with_other_type_in_if_statement(if_statement, type_, replacement_type)
        }
        BoundNodeKind::VariableDeclaration(variable_declaration) => {
            replace_type_with_other_type_in_variable_declaration(
                variable_declaration,
                type_,
                replacement_type,
            )
        }
        BoundNodeKind::WhileStatement(while_statement) => {
            replace_type_with_other_type_in_while_statement(
                while_statement,
                type_,
                replacement_type,
            )
        }
        BoundNodeKind::Assignment(assignment) => {
            replace_type_with_other_type_in_assignment(assignment, type_, replacement_type)
        }
        BoundNodeKind::ExpressionStatement(expression_statement) => {
            replace_type_with_other_type_in_expression_statement(
                expression_statement,
                type_,
                replacement_type,
            )
        }
        BoundNodeKind::ReturnStatement(return_statement) => {
            replace_type_with_other_type_in_return_statement(
                return_statement,
                type_,
                replacement_type,
            )
        }
    }
    node
}

fn replace_type_with_other_type_in_return_statement(
    return_statement: &mut BoundReturnStatementNodeKind,
    type_: &Type,
    replacement_type: &Type,
) {
    if let Some(it) = return_statement.expression.as_mut() {
        *it = Box::new(replace_type_with_other_type_on_node(
            *it.clone(),
            type_,
            replacement_type,
        ));
    }
}

fn replace_type_with_other_type_in_expression_statement(
    expression_statement: &mut BoundExpressionStatementNodeKind,
    type_: &Type,
    replacement_type: &Type,
) {
    expression_statement.expression = Box::new(replace_type_with_other_type_on_node(
        *expression_statement.expression.clone(),
        type_,
        replacement_type,
    ));
}

fn replace_type_with_other_type_in_assignment(
    assignment: &mut BoundAssignmentNodeKind,
    type_: &Type,
    replacement_type: &Type,
) {
    assignment.variable = Box::new(replace_type_with_other_type_on_node(
        *assignment.variable.clone(),
        type_,
        replacement_type,
    ));
    assignment.expression = Box::new(replace_type_with_other_type_on_node(
        *assignment.expression.clone(),
        type_,
        replacement_type,
    ));
}

fn replace_type_with_other_type_in_while_statement(
    while_statement: &mut BoundWhileStatementNodeKind,
    type_: &Type,
    replacement_type: &Type,
) {
    while_statement.condition = Box::new(replace_type_with_other_type_on_node(
        *while_statement.condition.clone(),
        type_,
        replacement_type,
    ));
    while_statement.body = Box::new(replace_type_with_other_type_on_node(
        *while_statement.body.clone(),
        type_,
        replacement_type,
    ));
}

fn replace_type_with_other_type_in_variable_declaration(
    variable_declaration: &mut BoundVariableDeclarationNodeKind,
    type_: &Type,
    replacement_type: &Type,
) {
    variable_declaration.initializer = Box::new(replace_type_with_other_type_on_node(
        *variable_declaration.initializer.clone(),
        type_,
        replacement_type,
    ));
}

fn replace_type_with_other_type_in_if_statement(
    if_statement: &mut BoundIfStatementNodeKind,
    type_: &Type,
    replacement_type: &Type,
) {
    if_statement.condition = Box::new(replace_type_with_other_type_on_node(
        *if_statement.condition.clone(),
        type_,
        replacement_type,
    ));
    if_statement.body = Box::new(replace_type_with_other_type_on_node(
        *if_statement.body.clone(),
        type_,
        replacement_type,
    ));
    if let Some(it) = if_statement.else_body.as_mut() {
        *it = Box::new(replace_type_with_other_type_on_node(
            *it.clone(),
            type_,
            replacement_type,
        ));
    }
}

fn replace_type_with_other_type_in_block_statement(
    block_statement: &mut BoundBlockStatementNodeKind,
    type_: &Type,
    replacement_type: &Type,
) {
    for it in block_statement.statements.iter_mut() {
        *it = replace_type_with_other_type_on_node(it.clone(), type_, replacement_type);
    }
}

fn replace_type_with_other_type_in_conversion(
    conversion: &mut BoundConversionNodeKind,
    type_: &Type,
    replacement_type: &Type,
) {
    conversion.base = Box::new(replace_type_with_other_type_on_node(
        *conversion.base.clone(),
        type_,
        replacement_type,
    ));
    replace_type_with_other_type(&mut conversion.type_, type_, replacement_type);
}

fn replace_type_with_other_type_in_closure(
    closure: &mut BoundClosureNodeKind,
    type_: &Type,
    replacement_type: &Type,
) {
    for it in closure.arguments.iter_mut() {
        *it = replace_type_with_other_type_on_node(it.clone(), type_, replacement_type);
    }
}

fn replace_type_with_other_type_in_field_access(
    field_access: &mut BoundFieldAccessNodeKind,
    type_: &Type,
    replacement_type: &Type,
) {
    field_access.base = Box::new(replace_type_with_other_type_on_node(
        *field_access.base.clone(),
        type_,
        replacement_type,
    ));
    replace_type_with_other_type(&mut field_access.type_, type_, replacement_type);
}

fn replace_type_with_other_type_in_array_index(
    array_index: &mut BoundArrayIndexNodeKind,
    type_: &Type,
    replacement_type: &Type,
) {
    array_index.base = Box::new(replace_type_with_other_type_on_node(
        *array_index.base.clone(),
        type_,
        replacement_type,
    ));
    array_index.index = Box::new(replace_type_with_other_type_on_node(
        *array_index.index.clone(),
        type_,
        replacement_type,
    ));
}

fn replace_type_with_other_type_in_system_call(
    system_call: &mut BoundSystemCallNodeKind,
    type_: &Type,
    replacement_type: &Type,
) {
    for it in system_call.arguments.iter_mut() {
        *it = replace_type_with_other_type_on_node(it.clone(), type_, replacement_type);
    }
}

fn replace_type_with_other_type_in_function_call(
    function_call: &mut BoundFunctionCallNodeKind,
    type_: &Type,
    replacement_type: &Type,
) {
    function_call.base = Box::new(replace_type_with_other_type_on_node(
        *function_call.base.clone(),
        type_,
        replacement_type,
    ));
    for it in function_call.arguments.iter_mut() {
        *it = replace_type_with_other_type_on_node(it.clone(), type_, replacement_type);
    }
}

fn replace_type_with_other_type_in_binary_expression(
    binary_expression: &mut BoundBinaryNodeKind,
    type_: &Type,
    replacement_type: &Type,
) {
    binary_expression.lhs = Box::new(replace_type_with_other_type_on_node(
        *binary_expression.lhs.clone(),
        type_,
        replacement_type,
    ));
    binary_expression.rhs = Box::new(replace_type_with_other_type_on_node(
        *binary_expression.rhs.clone(),
        type_,
        replacement_type,
    ));
}

fn replace_type_with_other_type_in_unary_expression(
    unary_expression: &mut BoundUnaryNodeKind,
    type_: &Type,
    replacement_type: &Type,
) {
    unary_expression.operand = Box::new(replace_type_with_other_type_on_node(
        *unary_expression.operand.clone(),
        type_,
        replacement_type,
    ));
}

fn replace_type_with_other_type_in_constructor_call(
    constructor_call: &mut BoundConstructorCallNodeKind,
    type_: &Type,
    replacement_type: &Type,
) {
    for it in constructor_call.arguments.iter_mut() {
        *it = replace_type_with_other_type_on_node(it.clone(), type_, replacement_type);
    }
}

fn replace_type_with_other_type_in_array_literal(
    array_literal: &mut BoundArrayLiteralNodeKind,
    type_: &Type,
    replacement_type: &Type,
) {
    for it in array_literal.children.iter_mut() {
        *it = replace_type_with_other_type_on_node(it.clone(), type_, replacement_type);
    }
}

fn replace_type_with_other_type_in_jump(
    jump: &mut BoundJumpNodeKind,
    type_: &Type,
    replacement_type: &Type,
) {
    if let Some(condition) = jump.condition.as_mut() {
        *condition = Box::new(replace_type_with_other_type_on_node(
            *condition.clone(),
            type_,
            replacement_type,
        ));
    }
}

fn replace_type_with_other_type_in_function_declaration(
    function_declaration: &mut BoundFunctionDeclarationNodeKind,
    type_: &Type,
    replacement_type: &Type,
) {
    function_declaration.body = Box::new(replace_type_with_other_type_on_node(
        *function_declaration.body.clone(),
        type_,
        replacement_type,
    ));
}
