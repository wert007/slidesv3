mod allocator;
pub mod basic_blocks;
mod bound_nodes;
mod instructions;
mod syntax_nodes;

pub use allocator::output_allocator_to_dot;
pub use basic_blocks::output_basic_blocks_to_dot;
pub use bound_nodes::print_bound_node_as_code;
pub use instructions::commented_instruction_to_string;
pub use instructions::instruction_or_label_to_string;
pub use instructions::instruction_to_string;
pub use instructions::output_instructions_or_labels_with_source_code_to_sldasm;
pub use instructions::output_instructions_with_source_code_to_sldasm;
pub use instructions::print_instructions_or_labels_with_source_code;
pub use instructions::print_instructions_with_source_code;
pub use syntax_nodes::print_syntax_node_as_code;

// TODO: Move to own module.
pub(crate) fn print_location_as_source_text(
    location: crate::text::TextLocation,
    source_text_collection: &crate::text::SourceTextCollection,
) {
    let text = &source_text_collection[location];
    let file_name = &source_text_collection[location.source_text].file_name;
    println!("// Source text snippet from {file_name}");
    println!("{text}");
    println!();
}

#[derive(Debug, Clone, Copy, Default)]
pub struct DebugFlags {
    pub print_instructions: bool,
    pub print_instructions_and_labels: bool,
    pub print_runtime_instruction: bool,
    pub print_variable_table: bool,
    pub print_constant_table: bool,
    pub print_struct_table: bool,
    pub print_tokens: bool,
    pub print_heap_as_string: bool,
    pub print_static_memory_as_string: bool,
    pub print_bound_program: bool,
    pub print_syntax_program: bool,
    pub print_stack: bool,
    pub print_labels: bool,
    pub print_type_table: bool,
    pub output_basic_blocks_to_dot: bool,
    pub check_stack_corruption: bool,
    pub output_instructions_to_sldasm: bool,
    pub output_instructions_and_labels_to_sldasm: bool,
    pub run_program: bool,
    pub slow_mode: bool,
    pub use_debugger: bool,
    pub print_lines: bool,
}

#[derive(Clone, Copy, Debug, Default)]
struct DebugPrinter {
    indent: usize,
}

impl DebugPrinter {
    fn print_indentation(&self) {
        for _ in 0..self.indent {
            print!(" ");
        }
    }

    fn output_indentation(&self, buffer: &mut String) {
        for _ in 0..self.indent {
            buffer.push(' ');
        }
    }
}
