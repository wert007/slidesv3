mod binder;
#[allow(dead_code)]
mod debug;
mod diagnostics;
mod evaluator;
mod instruction_converter;
mod lexer;
mod parser;
mod text;
mod value;

use crate::{diagnostics::DiagnosticBag, text::SourceText};

pub use debug::DebugFlags;

pub fn evaluate(input: &str, file_name: &str, debug_flags: DebugFlags) {
    let source_text = SourceText::new(input, file_name);
    let mut diagnostic_bag = DiagnosticBag::new(&source_text);
    let result = instruction_converter::convert(&source_text, &mut diagnostic_bag, debug_flags);
    // let result = binder::bind(input, &mut diagnostic_bag);
    if diagnostic_bag.has_errors() {
        diagnostic_bag.flush_to_console();
        return;
    }
    let result = evaluator::evaluate(result, debug_flags);
    println!("= {}", result);
}
