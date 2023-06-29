#[macro_use]
mod binder;
#[allow(dead_code)]
mod debug;
mod diagnostics;
mod evaluator;
mod instruction_converter;
pub mod lexer;
mod parser;
mod text;
mod value;

use std::path::Path;

use crate::{diagnostics::DiagnosticBag, text::SourceText};

use binder::{symbols::Library, typing::TypeCollection};
pub use debug::DebugFlags;
use text::{SourceTextCollection, SourceTextId};

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Project {
    types: TypeCollection,
    debug_flags: DebugFlags,
    source_text_collection: SourceTextCollection,
}

impl Project {
    pub fn new(debug_flags: DebugFlags) -> Self {
        Self {
            types: TypeCollection::new(),
            debug_flags,
            source_text_collection: SourceTextCollection::default(),
        }
    }

    pub fn evaluate(&mut self, source_text: &str, file_name: &str) {
        let source_text = SourceText::new(source_text, file_name);
        let source_text = self.source_text_collection.add(source_text);
        let mut diagnostic_bag = DiagnosticBag::new();
        let result = instruction_converter::convert_with_project_parameter(
            source_text,
            &mut diagnostic_bag,
            self,
        );
        // let result = binder::bind(input, &mut diagnostic_bag);
        if diagnostic_bag.has_errors() || !self.debug_flags.run_program {
            diagnostic_bag.flush_to_console(&self.source_text_collection);
            return;
        }
        let result = evaluator::evaluate(result, source_text, self.clone());
        println!("= {}", result);
    }

    pub fn load_library(&mut self, source_text: SourceTextId, import_std_libs: bool) -> Library {
        let mut diagnostic_bag = DiagnosticBag::new();
        let mut result = instruction_converter::convert_library_with_project_parameter(
            source_text,
            &mut diagnostic_bag,
            self,
            import_std_libs,
        );
        if diagnostic_bag.has_errors() {
            result.has_errors = true;
            diagnostic_bag.flush_to_console(&self.source_text_collection);
        }
        result
    }

    pub fn load_library_from_path<P>(&mut self, path: P, import_std_libs: bool) -> Library
    where
        P: AsRef<Path>,
    {
        let path = path.as_ref().to_owned();
        let source_code = std::fs::read_to_string(&path);
        if source_code.is_err() {
            return Library::error();
        }
        let source_code = source_code.unwrap();
        let file_name = path.to_string_lossy();
        let source_text = self
            .source_text_collection
            .add(SourceText::new(source_code, file_name));
        self.load_library(source_text, import_std_libs)
    }

    pub(crate) fn maybe_print_type_table(&self) {
        self.types.maybe_print_type_table(self.debug_flags);
    }
}

// pub fn load_library_from_source(
//     file_name: &str,
//     source_code: &'static str,
//     debug_flags: DebugFlags,
//     import_std_libs: bool,
// ) -> Library {
//     load_library(&source_code, &file_name, debug_flags, import_std_libs)
// }

// pub fn load_library(
//     input: &str,
//     file_name: &str,
//     debug_flags: DebugFlags,
//     import_std_libs: bool,
// ) -> Library {
//     let source_text = SourceText::new(input, file_name);
//     let mut diagnostic_bag = DiagnosticBag::new(&source_text);
//     let mut result = instruction_converter::convert_library(
//         &source_text,
//         &mut diagnostic_bag,
//         debug_flags,
//         import_std_libs,
//     );
//     if diagnostic_bag.has_errors() {
//         result.has_errors = true;
//         diagnostic_bag.flush_to_console();
//     }
//     result
// }

// pub fn evaluate(input: &str, file_name: &str, debug_flags: DebugFlags) {
//     let source_text = SourceText::new(input, file_name);
//     let mut diagnostic_bag = DiagnosticBag::new(&source_text);
//     let result = instruction_converter::convert(&source_text, &mut diagnostic_bag, debug_flags);
//     // let result = binder::bind(input, &mut diagnostic_bag);
//     if diagnostic_bag.has_errors() || !debug_flags.run_program {
//         diagnostic_bag.flush_to_console();
//         return;
//     }
//     let result = evaluator::evaluate(result, &source_text, debug_flags);
//     println!("= {}", result);
// }
