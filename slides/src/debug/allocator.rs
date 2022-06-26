use std::{fmt::Write, process::Command};

use crate::evaluator::memory::{
    self,
    allocator::{Allocator, BucketEntry},
};

pub fn output_allocator_to_dot(file_name: &str, heap: &Allocator) {
    let color_names = [
        "7a8296", "88b3a4", "7eb2f2", "aed6d6", "88fcc2", "0e86c2", "12bddb", "73ffe5", "c1e8e5",
        "62c49d",
    ];
    let pale_color_names = [
        "697185", "77a293", "6da1e1", "9dc5c5", "77ebb1", "0d75b1", "01acca", "62eed4", "b0d7d4",
        "51b38e",
    ];
    let mut result = String::new();
    let mut node_ids = vec![];
    result.push_str("graph Heap {\n");
    result.push_str("    splines = ortho;\n");
    for (index, bucket) in heap.buckets.iter().enumerate() {
        let node_id = match bucket {
            BucketEntry::Bucket(_) => format!("B{}", index),
            BucketEntry::Parent(_) => format!("P{}", index),
            BucketEntry::Tombstone => format!("T{}", index),
        };
        node_ids.push(node_id.clone());
        result.push_str("    ");
        result.push_str(&node_id);
        result.push_str(" [shape=box style=filled label = \"\\N\\n");
        match bucket {
            BucketEntry::Bucket(bucket) => {
                let address = (bucket.address * memory::WORD_SIZE_IN_BYTES) | memory::HEAP_POINTER;
                let address = address
                    .to_be_bytes()
                    .iter()
                    .map(|e| format!("{e:02X}"))
                    .collect::<Vec<_>>()
                    .join(" ".into());
                let potential_str: Option<String> = {
                    let mut data = vec![];
                    for word in
                        &heap.data[bucket.address as usize..][..bucket.size_in_words as usize]
                    {
                        data.extend_from_slice(&word.to_be_bytes());
                    }
                    match std::str::from_utf8(&data) {
                        Ok(s) => Some(
                            s.chars()
                                .filter(|c| !c.is_control())
                                .map(|c| if c == '"' { '`' } else { c })
                                .collect(),
                        ),
                        Err(_) => None,
                    }
                };
                let mut bucket_data = String::new();
                for word in 0..bucket.size_in_words {
                    for byte in heap.data[(bucket.address + word) as usize].to_be_bytes() {
                        write!(bucket_data, "{:02X} ", byte).unwrap();
                    }
                    writeln!(bucket_data).unwrap();
                }

                result.push_str(
                    &format!(
                        "Address:{}\\l{:#?}\\lData:\\l{}\\l",
                        address, bucket, bucket_data
                    )
                    .replace('\n', "\\l"),
                );
                if let Some(str) = potential_str {
                    write!(result, "String:\\l'{}'\\l", str).unwrap();
                }
            }
            BucketEntry::Parent(parent) => {
                result.push_str(&format!("{:#?}\\l", parent).replace('\n', "\\l"))
            }
            BucketEntry::Tombstone => result.push_str("Tombstone"),
        }
        result.push_str("\" ");
        if !matches!(bucket, BucketEntry::Tombstone) {
            if let Some(buddy_index) = bucket.buddy_index() {
                result.push_str("fillcolor=\"#");
                let color_name_index = (index + buddy_index) % color_names.len();
                if bucket.is_used() {
                    result.push_str(color_names[color_name_index]);
                } else {
                    result.push_str(pale_color_names[color_name_index]);
                }
                result.push_str("\" ");
            }
        }
        result.push_str("];\n");
    }
    let mut add_connection = |block_index: usize, target: usize, is_dashed: bool| {
        result.push_str("    ");
        result.push_str(&node_ids[block_index]);
        result.push_str(" -- ");
        result.push_str(&node_ids[target]);
        if is_dashed {
            result.push_str(" [ style=dashed ]");
        }
        result.push_str(";\n");
    };
    for bucket in &heap.buckets {
        match bucket {
            BucketEntry::Bucket(_bucket) => {}
            BucketEntry::Parent(parent) => {
                add_connection(parent.index, parent.child_indices[0], false);
                add_connection(parent.index, parent.child_indices[1], false);
            }
            BucketEntry::Tombstone => {}
        }
    }
    result.push_str("}\n");

    let out_dot_path = format!("../debug-out/{}.dot", file_name);
    let out_svg_path = format!("../debug-out/{}.svg", file_name);
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
