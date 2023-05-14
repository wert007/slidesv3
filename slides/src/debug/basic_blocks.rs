use std::fmt::{Display, Write};
use std::{collections::HashMap, process::Command};

use crate::binder::control_flow_analyzer::OutgoingConnections;
use crate::binder::typing::TypeCollection;
use crate::binder::{bound_nodes::BoundNode, control_flow_analyzer::BasicBlockKind};
use crate::instruction_converter::InstructionOrLabelReference;

pub trait DebugBasicBlock<T, C> {
    fn kind(&self) -> BasicBlockKind;
    fn label_indices(&self) -> &[usize];
    fn outgoing_connections(&self) -> OutgoingConnections<T>;
    fn comment(&self) -> Option<C>;
}

pub trait BasicBlockConditionDisplay {
    fn display(&self, types: &TypeCollection, buffer: &mut String);
}

impl BasicBlockConditionDisplay for &BoundNode {
    fn display(&self, types: &TypeCollection, buffer: &mut String) {
        super::bound_nodes::bound_node_as_code_to_string(self, types, buffer);
    }
}

impl BasicBlockConditionDisplay for BoundNode {
    fn display(&self, types: &TypeCollection, buffer: &mut String) {
        super::bound_nodes::bound_node_as_code_to_string(self, types, buffer);
    }
}

impl BasicBlockConditionDisplay for () {
    fn display(&self, _: &TypeCollection, _: &mut String) {}
}

impl BasicBlockConditionDisplay for InstructionOrLabelReference {
    fn display(&self, _: &TypeCollection, buffer: &mut String) {
        writeln!(buffer, "{}", crate::debug::instruction_or_label_to_string(*self)).unwrap();
    }
}

pub(crate) fn output_basic_instruction_blocks_to_dot<
    DBB: DebugBasicBlock<C, BC>,
    C: BasicBlockConditionDisplay,
    BC: Display,
    S: BasicBlockConditionDisplay,
>(
    function_name: &str,
    basic_blocks: &[DBB],
    instructions: &[S],
) {
    output_basic_blocks_to_dot(
        function_name,
        basic_blocks,
        instructions,
        &TypeCollection::new(),
    );
}

pub fn output_basic_blocks_to_dot<
    DBB: DebugBasicBlock<C, BC>,
    C: BasicBlockConditionDisplay,
    S: BasicBlockConditionDisplay,
    BC: Display,
>(
    function_name: &str,
    basic_blocks: &[DBB],
    statements: &[S],
    types: &TypeCollection,
) {
    let mut result = String::new();
    result.push_str("digraph \"");
    result.push_str(function_name);
    result.push_str("\" {\n");
    result.push_str("    splines = ortho;\n");
    let mut generated_node_count = 0;
    let mut block_index_to_node_id = HashMap::new();
    for (index, basic_block) in basic_blocks.iter().enumerate() {
        let node_id = match basic_block.kind() {
            BasicBlockKind::Start => "\"<Start>\"".into(),
            BasicBlockKind::End => "\"<End>\"".into(),
            BasicBlockKind::CodeBlock(_) => {
                if basic_block.label_indices().is_empty() {
                    generated_node_count += 1;
                    format!("N{}", generated_node_count - 1)
                } else {
                    let result = basic_block
                        .label_indices()
                        .iter()
                        .map(|l| format!("L{l:X}"))
                        .collect::<Vec<String>>()
                        .join(",");
                    format!("\"{}\"", result)
                }
            }
        };
        block_index_to_node_id.insert(index, node_id.clone());
        result.push_str("    ");
        result.push_str(&node_id);

        if let BasicBlockKind::CodeBlock(code_block) = basic_block.kind() {
            let statements = &statements[code_block.index..][..code_block.length];
            let mut buffer = String::new();
            for statement in statements {
                statement.display(types, &mut buffer);
            }
            result.push_str(" [label = \"\\N\\n");
            result.push_str(&buffer.replace('\n', "\\l"));
            if let Some(comment) = basic_block.comment() {
                write!(result, "\\lCOMMENT: {comment}").unwrap();
            }
            result.push_str("\"]");
        }
        result.push_str(";\n");
    }
    let mut add_connection = |block_index, target, label: &str| {
        result.push_str("    ");
        result.push_str(&block_index_to_node_id[&block_index]);
        result.push_str(" -> ");
        result.push_str(&block_index_to_node_id[&target]);
        if !label.is_empty() {
            result.push_str(" [ label = \"");
            result.push_str(label);
            result.push_str("\" ]");
        }
        result.push_str(";\n");
    };
    for (block_index, basic_block) in basic_blocks.iter().enumerate() {
        match basic_block.outgoing_connections() {
            crate::binder::control_flow_analyzer::OutgoingConnections::None => {}
            crate::binder::control_flow_analyzer::OutgoingConnections::Single(target) => {
                add_connection(block_index, target, "");
            }
            crate::binder::control_flow_analyzer::OutgoingConnections::IfTrue(
                condition,
                then_target,
                else_target,
            ) => {
                let mut buffer = String::new();
                condition.display(types, &mut buffer);
                add_connection(block_index, then_target, &format!("{} is true", buffer));
                add_connection(block_index, else_target, "else");
            }
            crate::binder::control_flow_analyzer::OutgoingConnections::IfFalse(
                condition,
                then_target,
                else_target,
            ) => {
                let mut buffer = String::new();
                condition.display(types, &mut buffer);
                add_connection(block_index, then_target, &format!("{} is false", buffer));
                add_connection(block_index, else_target, "else");
            }
        }
    }
    result.push_str("}\n");

    let out_dot_path = mangle(format!("./debug-out/{}.dot", function_name));
    let out_svg_path = mangle(format!("./debug-out/{}.svg", function_name));
    std::fs::write(&out_dot_path, result).unwrap();
    let output = Command::new("dot")
        .args(&[&out_dot_path, "-Tsvg", &format!("-o{}", out_svg_path)])
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "stdout = {} \n\n stderr = {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn mangle(function_name: String) -> String {
    function_name.replace("::", "-DOT-DOT-").replace('<', "-LT-").replace('>', "-GT-")
}
