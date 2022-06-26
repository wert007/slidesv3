use std::io::Write;

use clap::{load_yaml, App};
use slides::DebugFlags;

fn main() -> Result<(), std::io::Error> {
    let yaml = load_yaml!("cli_arguments.yaml");
    let matches = App::from_yaml(yaml).get_matches();

    let input_file = matches.value_of("FILE");
    let debug_flags = DebugFlags {
        print_instructions: matches.is_present("print-instructions"),
        print_runtime_instruction: matches.is_present("print-runtime-instructions"),
        print_instructions_and_labels: matches.is_present("print-instructions-and-labels"),
        print_tokens: matches.is_present("print-tokens"),
        print_variable_table: matches.is_present("print-bound-variable-table"),
        print_constant_table: matches.is_present("print-bound-constant-table"),
        print_struct_table: matches.is_present("print-bound-struct-table"),
        print_bound_program: matches.is_present("print-bound-program"),
        print_labels: matches.is_present("print-labels"),
        print_heap_as_string: matches.is_present("print-heap-memory-as-string"),
        print_static_memory_as_string: matches.is_present("print-static-memory-as-string"),
        print_static_memory_as_hex: matches.is_present("print-static-memory-as-hex"),
        print_stack: matches.is_present("print-stack"),
        print_library_loading_order: matches.is_present("print-library-loading-order"),
        output_basic_blocks_to_dot: matches.is_present("output-basic-blocks-to-dot"),
        output_instructions_to_sldasm: matches.is_present("output-instructions-to-sldasm"),
        output_instructions_and_labels_to_sldasm: matches
            .is_present("output-instructions-and-labels-to-sldasm"),
        run_program: !matches.is_present("no-run"),
        slow_mode: matches.is_present("slow"),
        use_debugger: matches.is_present("debugger"),
    };

    match input_file {
        Some(file_path) => compile(file_path, debug_flags),
        None => repl(debug_flags),
    }
}

fn repl(debug_flags: DebugFlags) -> Result<(), std::io::Error> {
    let mut buf = String::new();
    loop {
        buf.clear();
        print!(">> ");
        std::io::stdout().flush()?;
        std::io::stdin().read_line(&mut buf)?;
        let buf = buf.trim().to_owned();
        if buf.is_empty() {
            break;
        }
        slides::evaluate(&buf, "", debug_flags);
    }
    Ok(())
}

fn compile(file_path: &str, debug_flags: DebugFlags) -> Result<(), std::io::Error> {
    let file = std::fs::read_to_string(file_path)?;
    slides::evaluate(&file, file_path, debug_flags);
    Ok(())
}
