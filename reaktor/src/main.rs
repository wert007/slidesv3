use std::io::Write;

use slides::DebugFlags;

fn main() -> Result<(), std::io::Error> {
    println!("Hello, world!");
    let args: Vec<String> = std::env::args().collect();
    let debug_flags = DebugFlags {
        print_instructions: args.contains(&String::from("-di")),
        print_instructions_and_labels: args.contains(&String::from("-dil")),
        print_tokens: args.contains(&String::from("-dt")),
        print_current_instruction: args.contains(&String::from("-dci")),
        print_variable_table: args.contains(&String::from("-dbv")),
        print_struct_table: args.contains(&String::from("-dbst")),
        print_heap_as_string: args.contains(&String::from("-dheap")),
        print_bound_program: args.contains(&String::from("-dbp")),
        print_stack: args.contains(&String::from("-dstack")),
        print_labels: args.contains(&String::from("-dlabels")),
        output_basic_blocks_to_dot: args.contains(&String::from("-dbb")),
        run_program: !args.contains(&String::from("-no-run")),
    };
    if args.len() == 1 || args[1].starts_with('-') {
        repl(debug_flags)
    } else {
        compile(&args[1], debug_flags)
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
