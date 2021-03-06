mod binder;
#[cfg(test)]
mod compilation_tests;
#[allow(dead_code)]
mod debug;
mod diagnostics;
mod evaluator;
mod instruction_converter;
mod lexer;
mod parser;
mod text;
mod value;

use std::path::Path;

use crate::{diagnostics::DiagnosticBag, text::SourceText};

use binder::symbols::Library;
pub use debug::DebugFlags;

pub fn load_library_from_path<P>(
    path: P,
    debug_flags: DebugFlags,
    import_std_libs: bool,
) -> std::io::Result<Library>
where
    P: AsRef<Path>,
{
    let path = path.as_ref().to_owned();
    let source_code = std::fs::read_to_string(&path)?;
    let file_name = path.to_string_lossy();
    Ok(load_library(
        &source_code,
        &file_name,
        debug_flags,
        import_std_libs,
    ))
}

pub fn load_library(
    input: &str,
    file_name: &str,
    debug_flags: DebugFlags,
    import_std_libs: bool,
) -> Library {
    if debug_flags.print_library_loading_order {
        println!(
            "Loading library {}. import std libs = {}",
            file_name, import_std_libs
        );
    }
    let source_text = SourceText::new(input, file_name);
    let mut diagnostic_bag = DiagnosticBag::new(&source_text, debug_flags.record_output || debug_flags.test_runner);
    let mut result = instruction_converter::convert_library(
        &source_text,
        &mut diagnostic_bag,
        debug_flags,
        import_std_libs,
    );
    if diagnostic_bag.has_errors() {
        result.has_errors = true;
        assert!(
            !debug_flags.record_output,
            "Cannot record libraries at the moment!"
        );
        diagnostic_bag
            .flush_to_console(std::io::stderr())
            .expect("Could not write to stdout.");
    }
    result
}

pub fn evaluate(input: &str, file_name: &str, debug_flags: DebugFlags) {
    let source_text = SourceText::new(input, file_name);
    let mut diagnostic_bag = DiagnosticBag::new(&source_text, debug_flags.record_output || debug_flags.test_runner);
    let result = instruction_converter::convert(&source_text, &mut diagnostic_bag, debug_flags);
    // let result = binder::bind(input, &mut diagnostic_bag);
    if diagnostic_bag.has_errors() || !debug_flags.run_program {
        match debug_flags.record_output {
            true => {
                let mut path = std::path::PathBuf::from(file_name);
                path.set_extension("diagnostics");
                let file = std::fs::File::options().write(true).truncate(true).create(true).open(path).expect("Could not create output file.");
                diagnostic_bag
                    .flush_to_console(file)
                    .expect("Could not write to output file.")
            }
            false => diagnostic_bag
                .flush_to_console(std::io::stderr())
                .expect("Could not write to stdout."),
        }
        return;
    }
    evaluator::evaluate(result, &source_text, debug_flags);
}
