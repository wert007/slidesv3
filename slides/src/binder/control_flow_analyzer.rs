use std::collections::HashMap;

use crate::{binder::bound_nodes::BoundNodeKind, DebugFlags};

use super::bound_nodes::BoundNode;

#[derive(Debug)]
pub struct BasicBlock<'a> {
    pub index: usize,
    pub label_indices: Vec<usize>,
    pub kind: BasicBlockKind,
    pub incoming_connections: IncomingConnections,
    pub outgoing_connections: OutgoingConnections<'a>,
}

impl<'a> BasicBlock<'a> {
    pub fn start() -> Self {
        Self {
            index: 0,
            label_indices: vec![],
            kind: BasicBlockKind::Start,
            incoming_connections: IncomingConnections::None,
            outgoing_connections: OutgoingConnections::None,
        }
    }

    pub fn end(index: usize, label_indices: Vec<usize>) -> Self {
        Self {
            index,
            label_indices,
            kind: BasicBlockKind::End,
            incoming_connections: IncomingConnections::None,
            outgoing_connections: OutgoingConnections::None,
        }
    }

    pub fn code_block(index: usize, code_block: BasicCodeBlock, label_indices: Vec<usize>) -> Self {
        Self {
            index,
            label_indices,
            kind: BasicBlockKind::CodeBlock(code_block),
            incoming_connections: IncomingConnections::None,
            outgoing_connections: OutgoingConnections::None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BasicBlockKind {
    Start,
    End,
    CodeBlock(BasicCodeBlock),
}

#[derive(Debug)]
pub enum IncomingConnections {
    None,
    Single(usize),
    Multiple(Vec<usize>),
}

impl IncomingConnections {
    pub fn add_connection_from(&mut self, from: usize) {
        match self {
            IncomingConnections::None => *self = Self::Single(from),
            IncomingConnections::Single(value) => *self = Self::Multiple(vec![*value, from]),
            IncomingConnections::Multiple(values) => values.push(from),
        }
    }
}

#[derive(Debug)]
pub enum OutgoingConnections<'a> {
    None,
    Single(usize),
    IfTrue(&'a BoundNode, usize, usize),
    IfFalse(&'a BoundNode, usize, usize),
}

impl OutgoingConnections<'_> {
    pub fn to_vec(&self) -> Vec<usize> {
        match self {
            OutgoingConnections::None => vec![],
            OutgoingConnections::Single(value) => vec![*value],
            OutgoingConnections::IfTrue(_, then_value, else_value)
            | OutgoingConnections::IfFalse(_, then_value, else_value) => {
                vec![*then_value, *else_value]
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BasicCodeBlock {
    pub index: usize,
    pub length: usize,
}

pub fn check_if_all_paths_return(
    function_name: &str,
    statements: &[BoundNode],
    debug_flags: DebugFlags,
) -> bool {
    let mut basic_blocks = collect_basic_blocks(statements);
    connect_basic_blocks(&mut basic_blocks, statements);
    if debug_flags.output_basic_blocks_to_dot {
        crate::debug::output_basic_blocks_to_dot(function_name, &basic_blocks, statements);
        println!(
            "Outputted BasicBlocks for func {}() to debug-out/{}.svg",
            function_name, function_name
        );
    }
    let accessible_incoming_to_end_block = collect_paths_to_end(&basic_blocks);
    if accessible_incoming_to_end_block.is_empty() {
        return false;
    }
    for incoming in accessible_incoming_to_end_block {
        let basic_block = &basic_blocks[incoming];
        if let BasicBlockKind::CodeBlock(code_block) = basic_block.kind {
            let last_statement = &statements[code_block.index + code_block.length - 1];
            if !matches!(last_statement.kind, BoundNodeKind::ReturnStatement(_)) {
                return false;
            }
        } else {
            return false;
        }
    }
    true
}

fn collect_paths_to_end(basic_blocks: &[BasicBlock]) -> Vec<usize> {
    let mut result = vec![];
    let mut gray_list = basic_blocks[0].outgoing_connections.to_vec();
    let mut already_seen = vec![];
    let end_index = basic_blocks.len() - 1;
    while let Some(next) = gray_list.pop() {
        if already_seen.contains(&next) {
            continue;
        } else {
            already_seen.push(next);
        }
        let mut next_connections = basic_blocks[next].outgoing_connections.to_vec();
        if next_connections.contains(&end_index) {
            result.push(next);
        }
        gray_list.append(&mut next_connections);
        gray_list.dedup();
    }
    result
}

fn collect_basic_blocks<'a>(statements: &[BoundNode]) -> Vec<BasicBlock<'a>> {
    let mut result = vec![BasicBlock::start()];
    let mut current_block = BasicCodeBlock {
        index: 0,
        length: 0,
    };
    let mut label_indices = vec![];
    for (index, statement) in statements.iter().enumerate() {
        current_block.length += 1;
        match &statement.kind {
            BoundNodeKind::Label(label_index) => {
                current_block.length -= 1;
                label_indices.push(*label_index);
                // Start new Block
                if current_block.length > 0 {
                    result.push(BasicBlock::code_block(
                        result.len(),
                        current_block,
                        label_indices,
                    ));
                    label_indices = vec![];
                }
                current_block = BasicCodeBlock {
                    index: index + 1,
                    length: 0,
                };
            }
            BoundNodeKind::ReturnStatement(_) | BoundNodeKind::Jump(_) => {
                // Start new Block
                if current_block.length > 0 {
                    result.push(BasicBlock::code_block(
                        result.len(),
                        current_block,
                        label_indices,
                    ));
                    label_indices = vec![];
                }
                current_block = BasicCodeBlock {
                    index: index + 1,
                    length: 0,
                };
            }
            BoundNodeKind::VariableDeclaration(_)
            | BoundNodeKind::Assignment(_)
            | BoundNodeKind::ExpressionStatement(_) => {}
            unexpected => unreachable!(
                "Unexpected BoundNodeKind {:#?}, only lowered statements are expected.",
                unexpected
            ),
        }
    }
    // End old Block
    if current_block.length > 0 {
        result.push(BasicBlock::code_block(
            result.len(),
            current_block,
            label_indices,
        ));
        label_indices = vec![];
    }
    result.push(BasicBlock::end(result.len(), label_indices));
    result
}

fn connect_basic_blocks<'a>(basic_blocks: &mut Vec<BasicBlock<'a>>, statements: &'a [BoundNode]) {
    let end_index = basic_blocks.last().unwrap().index;
    let mut incoming_connections = vec![];
    let label_index_to_basic_block_index: HashMap<usize, usize> = basic_blocks
        .iter()
        .enumerate()
        .flat_map(|(i, b)| b.label_indices.iter().map(move |v| (*v, i)))
        .collect();
    for basic_block in basic_blocks.iter_mut() {
        match basic_block.kind {
            BasicBlockKind::Start => {
                basic_block.outgoing_connections =
                    OutgoingConnections::Single(basic_block.index + 1);
                incoming_connections.push((basic_block.index, basic_block.index + 1));
            }
            BasicBlockKind::End => {}
            BasicBlockKind::CodeBlock(code_block) => {
                match &statements[code_block.index + code_block.length - 1].kind {
                    BoundNodeKind::Jump(jump) => {
                        let then_address =
                            if let BoundNodeKind::LabelReference(it) = &jump.target.kind {
                                label_index_to_basic_block_index[it]
                            } else {
                                panic!()
                            };
                        incoming_connections.push((basic_block.index, then_address));
                        basic_block.outgoing_connections =
                            match (jump.jump_if_true, &jump.condition) {
                                (true, Some(condition)) => {
                                    incoming_connections
                                        .push((basic_block.index, basic_block.index + 1));
                                    OutgoingConnections::IfTrue(
                                        condition,
                                        then_address,
                                        basic_block.index + 1,
                                    )
                                }
                                (true, None) | (false, None) => {
                                    OutgoingConnections::Single(then_address)
                                }
                                (false, Some(condition)) => {
                                    incoming_connections
                                        .push((basic_block.index, basic_block.index + 1));
                                    OutgoingConnections::IfFalse(
                                        condition,
                                        then_address,
                                        basic_block.index + 1,
                                    )
                                }
                            };
                    }
                    BoundNodeKind::ReturnStatement(_) => {
                        basic_block.outgoing_connections = OutgoingConnections::Single(end_index);
                        incoming_connections.push((basic_block.index, end_index));
                    }
                    _ => {
                        basic_block.outgoing_connections =
                            OutgoingConnections::Single(basic_block.index + 1);
                        incoming_connections.push((basic_block.index, basic_block.index + 1));
                    }
                }
            }
        }
    }

    for (from, to) in incoming_connections {
        basic_blocks[to]
            .incoming_connections
            .add_connection_from(from);
    }
}
