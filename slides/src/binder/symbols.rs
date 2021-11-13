use crate::instruction_converter::Program;

pub struct Library {
    pub program: Program,
}
impl Library {
    pub fn error() -> Self {
        Self {
            program: Program::error(),
        }
    }
}