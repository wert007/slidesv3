use std::collections::HashMap;

use super::bound_nodes::{BoundNode, BoundNodeKind};



pub fn replace_function_calls(mut node: BoundNode, function_label_map: &HashMap<u64, usize>) -> BoundNode {
    node.for_each_child_mut(&mut |n| {
        match &mut n.kind {
            BoundNodeKind::LabelReference(it) if function_label_map.contains_key(&(*it as u64)) => {
                dbg!();
                *it = function_label_map[&(*it as u64)];
            },
            _ => {}
        }
    });
    node
}