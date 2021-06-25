#[derive(Debug, Clone, Copy)]
pub struct DebugFlags {
    pub print_instructions: bool,
    pub print_current_instruction: bool,
    pub print_variable_table: bool,
}

impl Default for DebugFlags {
    fn default() -> Self {
        Self { print_instructions: false, print_variable_table: false, print_current_instruction: false }
    }
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
}

