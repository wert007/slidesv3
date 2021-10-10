use crate::binder::bound_nodes::BoundNodeKind;

use super::bound_nodes::{
    BoundBlockStatementNodeKind, BoundFunctionDeclarationNodeKind, BoundIfStatementNodeKind,
    BoundNode, BoundWhileStatementNodeKind,
};

struct Flattener {
    pub label_count: usize,
}

impl Flattener {
    pub fn new(label_count: usize) -> Self {
        Self { label_count }
    }
    pub fn next_label_index(&mut self) -> usize {
        let label = self.label_count;
        self.label_count += 1;
        label
    }
}

pub fn flatten<'a>(node: BoundNode<'a>, label_count: &mut usize) -> Vec<BoundNode<'a>> {
    let mut flattener = Flattener::new(*label_count);
    let result = flatten_node(node, &mut flattener);
    *label_count = flattener.label_count;
    result
}

fn flatten_node<'a>(node: BoundNode<'a>, flattener: &mut Flattener) -> Vec<BoundNode<'a>> {
    match node.kind {
        BoundNodeKind::VariableDeclaration(_)
        | BoundNodeKind::Assignment(_)
        | BoundNodeKind::ReturnStatement(_)
        | BoundNodeKind::ExpressionStatement(_)
        | BoundNodeKind::FieldAccess(_)
        | BoundNodeKind::Closure(_)
        | BoundNodeKind::Conversion(_)
        | BoundNodeKind::ArrayIndex(_)
        | BoundNodeKind::SystemCall(_)
        | BoundNodeKind::FunctionCall(_)
        | BoundNodeKind::BinaryExpression(_)
        | BoundNodeKind::UnaryExpression(_)
        | BoundNodeKind::ConstructorCall(_)
        | BoundNodeKind::VariableExpression(_)
        | BoundNodeKind::ArrayLiteralExpression(_)
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
        BoundNodeKind::Jump(_) => unreachable!(),
    }
}

fn flatten_function_declaration<'a>(
    function_declaration: BoundFunctionDeclarationNodeKind<'a>,
    _flattener: &mut Flattener,
) -> Vec<BoundNode<'a>> {
    let mut _result = vec![BoundNode::label(function_declaration.index)];
    // TODO: assign parameters here!
    unimplemented!();
    // result.append(&mut flatten_node(*function_declaration.body, flattener));
    // result
}

fn flatten_block_statement<'a>(
    block_statement: BoundBlockStatementNodeKind<'a>,
    flattener: &mut Flattener,
) -> Vec<BoundNode<'a>> {
    let mut result = Vec::with_capacity(block_statement.statements.len());
    for statement in block_statement.statements {
        result.append(&mut flatten_node(statement, flattener));
    }
    result
}

fn flatten_if_statement<'a>(
    if_statement: BoundIfStatementNodeKind<'a>,
    flattener: &mut Flattener,
) -> Vec<BoundNode<'a>> {
    let mut result = vec![];
    let (end_label, end_label_ref) = create_label(flattener);
    let (else_label, else_label_ref) = if if_statement.else_body.is_some() {
        create_label(flattener)
    } else {
        (end_label.clone(), end_label_ref.clone())
    };
    result.push(BoundNode::jump_if_false(
        *if_statement.condition,
        else_label_ref,
    ));
    result.append(&mut flatten_node(*if_statement.body, flattener));
    if if_statement.else_body.is_some() {
        result.push(BoundNode::jump(end_label_ref));
        result.push(else_label);
    }
    if let Some(else_body) = if_statement.else_body {
        result.append(&mut flatten_node(*else_body, flattener));
    }
    result.push(end_label);
    result
}

fn flatten_while_statement<'a>(
    while_statement: BoundWhileStatementNodeKind<'a>,
    flattener: &mut Flattener,
) -> Vec<BoundNode<'a>> {
    let mut result = vec![];
    let (label, label_ref) = create_label(flattener);
    result.push(label);
    result.append(&mut flatten_node(*while_statement.body, flattener));
    result.push(BoundNode::jump_if_true(
        *while_statement.condition,
        label_ref,
    ));
    result
}

fn create_label<'a>(flattener: &mut Flattener) -> (BoundNode<'a>, BoundNode<'a>) {
    let index = flattener.next_label_index();
    (BoundNode::label(index), BoundNode::label_reference(index))
}
