mod binder;
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
    let source_text = SourceText::new(input, "this_is_a_file.txt");
    dbg!(source_text);
    let mut diagnostic_bag = DiagnosticBag::new(SourceText::new(input, file_name));
    let result = instruction_converter::convert(input, &mut diagnostic_bag, debug_flags);
    // let result = binder::bind(input, &mut diagnostic_bag);
    if diagnostic_bag.has_errors() {
        diagnostic_bag.flush_to_console();
        return;
    }
    let result = evaluator::evaluate(result, debug_flags);
    println!("= {}", result);
}
