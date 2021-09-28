use std::{collections::HashMap, process::Command};

use crate::binder::control_flow_analyzer::BasicBlock;
use crate::binder::{bound_nodes::BoundNode, control_flow_analyzer::BasicBlockKind};

pub fn output_basic_blocks_to_dot(basic_blocks: &[BasicBlock], statements: &[BoundNode]) {
    let mut result = String::new();
    result.push_str("digraph Hello {\n");
    let mut generated_node_count = 0;
    let mut block_index_to_node_id = HashMap::new();
    for (index, basic_block) in basic_blocks.iter().enumerate() {
        let node_id = match basic_block.kind {
            BasicBlockKind::Start => "\"<Start>\"".into(),
            BasicBlockKind::End => "\"<End>\"".into(),
            BasicBlockKind::CodeBlock(_) => {
                if basic_block.label_indices.is_empty() {
                    generated_node_count += 1;
                    format!("N{}", generated_node_count - 1)
                } else {
                    let result = basic_block
                        .label_indices
                        .iter()
                        .map(|l| format!("L{}", l))
                        .collect::<Vec<String>>()
                        .join("-");
                    format!("\"{}\"", result)
                }
            }
        };
        block_index_to_node_id.insert(index, node_id.clone());
        result.push_str("    ");
        result.push_str(&node_id);

        if let BasicBlockKind::CodeBlock(code_block) = basic_block.kind {
            let statements = &statements[code_block.index..][..code_block.length];
            let mut buffer = String::new();
            for statement in statements {
                super::bound_nodes::bound_node_as_code_to_string(statement, &mut buffer);
            }
            result.push_str(" [label = \"\\N\n");
            result.push_str(&buffer.replace('\n', "\\l"));
            result.push_str("\"]");
        }
        // TODO: Add code as label here!
        result.push_str(";\n");
    }
    for (block_index, basic_block) in basic_blocks.iter().enumerate() {
        for outgoing in basic_block.outgoing_connections.to_vec() {
            result.push_str("    ");
            result.push_str(&block_index_to_node_id[&block_index]);
            result.push_str(" -> ");
            result.push_str(&block_index_to_node_id[&outgoing]);
            result.push_str(";\n");
        }
    }
    result.push_str("}\n");

    std::fs::write("../out.dot", result).unwrap();
    let output = Command::new("dot")
        .args(&["../out.dot", "-Tsvg", "-o../out.svg"])
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "stdout = {} \n\n stderr = {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}
