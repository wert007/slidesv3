use crate::binder::bound_nodes::BoundNodeKind;

use super::bound_nodes::BoundNode;

#[derive(Debug)]
struct BasicBlock<'a> {
    index: usize,
    kind: BasicBlockKind,
    incoming_connections: IncomingConnections,
    outgoing_connections: OutgoingConnections<'a>,
}

impl<'a> BasicBlock<'a> {
    pub fn start() -> Self {
        Self {
            index: 0,
            kind: BasicBlockKind::Start,
            incoming_connections: IncomingConnections::None,
            outgoing_connections: OutgoingConnections::None,
        }
    }

    pub fn end(index: usize) -> Self {
        Self {
            index,
            kind: BasicBlockKind::End,
            incoming_connections: IncomingConnections::None,
            outgoing_connections: OutgoingConnections::None,
        }
    }

    pub fn code_block(index: usize, code_block: BasicCodeBlock) -> Self {
        Self {
            index,
            kind: BasicBlockKind::CodeBlock(code_block),
            incoming_connections: IncomingConnections::None,
            outgoing_connections: OutgoingConnections::None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum BasicBlockKind {
    Start,
    End,
    CodeBlock(BasicCodeBlock),
}

#[derive(Debug)]
enum IncomingConnections {
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

    pub fn to_vec(&self) -> Vec<usize> {
        match self {
            IncomingConnections::None => vec![],
            IncomingConnections::Single(value) => vec![*value],
            IncomingConnections::Multiple(values) => values.clone(),
        }
    }
}

#[derive(Debug)]
enum OutgoingConnections<'a> {
    None,
    Single(usize),
    IfTrue(BoundNode<'a>, usize, usize),
    IfFalse(BoundNode<'a>, usize, usize),
}

#[derive(Debug, Clone, Copy)]
struct BasicCodeBlock {
    index: usize,
    length: usize,
}

pub fn check_if_all_paths_return(body: &BoundNode) -> bool {
    if let BoundNodeKind::BlockStatement(block_statement) = &body.kind {
        let mut basic_blocks = collect_basic_blocks(&block_statement.statements);
        connect_basic_blocks(&mut basic_blocks, &block_statement.statements);
        let incoming = basic_blocks.last().unwrap().incoming_connections.to_vec();
        for incoming in incoming {
            let basic_block = &basic_blocks[incoming];
            if let BasicBlockKind::CodeBlock(code_block) = basic_block.kind {
                let last_statement =
                    &block_statement.statements[code_block.index + code_block.length - 1];
                if !matches!(last_statement.kind, BoundNodeKind::ReturnStatement(_)) {
                    return false;
                }
            } else {
                return false;
            }
        }
        true
    } else {
        panic!("Unexpected BoundNodeKind {:#?}!", body);
    }
}

fn collect_basic_blocks<'a>(statements: &[BoundNode<'a>]) -> Vec<BasicBlock<'a>> {
    let mut result = vec![BasicBlock::start()];
    let mut current_block = BasicCodeBlock {
        index: 0,
        length: 0,
    };
    for (index, statement) in statements.iter().enumerate() {
        current_block.length += 1;
        match &statement.kind {
            BoundNodeKind::Label(_) => {
                current_block.length -= 1;
                // Start new Block
                if current_block.length > 0 {
                    result.push(BasicBlock::code_block(result.len(), current_block));
                }
                current_block = BasicCodeBlock { index, length: 1 };
            }
            BoundNodeKind::ReturnStatement(_) | BoundNodeKind::Jump(_) => {
                // Start new Block
                if current_block.length > 0 {
                    result.push(BasicBlock::code_block(result.len(), current_block));
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
        result.push(BasicBlock::code_block(result.len(), current_block));
    }
    result.push(BasicBlock::end(result.len()));
    result
}

fn connect_basic_blocks(basic_blocks: &mut Vec<BasicBlock>, statements: &[BoundNode]) {
    let end_index = basic_blocks.last().unwrap().index;
    let mut incoming_connections = vec![];
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
                    BoundNodeKind::Jump(_) => todo!(),
                    BoundNodeKind::ReturnStatement(_) => {
                        basic_block.outgoing_connections = OutgoingConnections::Single(end_index);
                        incoming_connections.push((basic_block.index, end_index));
                    }
                    _ => unreachable!(),
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
