mod allocator;
mod basic_blocks;
mod bound_nodes;
pub mod instructions;

pub use allocator::output_allocator_to_dot;
pub use basic_blocks::output_basic_blocks_to_dot;
pub use bound_nodes::print_bound_node_as_code;
pub use instructions::instruction_to_string;
pub use instructions::output_instructions_or_labels_with_source_code_to_sldasm;
pub use instructions::output_instructions_with_source_code_to_sldasm;
pub use instructions::print_instructions_or_labels_with_source_code;
pub use instructions::print_instructions_with_source_code;

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
    pub print_static_memory_as_hex: bool,
    pub print_bound_program: bool,
    pub print_stack: bool,
    pub print_labels: bool,
    pub print_library_loading_order: bool,
    pub output_basic_blocks_to_dot: bool,
    pub output_instructions_to_sldasm: bool,
    pub output_instructions_and_labels_to_sldasm: bool,
    pub run_program: bool,
    pub slow_mode: bool,
    pub use_debugger: bool,
    pub record_output: bool,
    pub test_runner: bool,
}

impl DebugFlags {
    /// Get a reference to the debug flags's print instructions.
    pub fn print_instructions(&self) -> bool {
        self.print_instructions
    }
    /// Get a reference to the debug flags's print current instruction.
    pub fn print_current_instruction(&self) -> bool {
        self.print_instructions
    }
    /// Get a reference to the debug flags's print variable table.
    pub fn print_variable_table(&self) -> bool {
        self.print_variable_table
    }

    /// Get a reference to the debug flags's print tokens.
    pub fn print_tokens(&self) -> bool {
        self.print_tokens
    }
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
