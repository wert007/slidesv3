use crate::evaluator::memory;

use super::EvaluatorState;

pub fn create_session(state: EvaluatorState) -> bool {
    println!("This is the debugger. Type :q to exit the debugger.");
    let mut line = String::new();
    loop {
        std::io::stdin().read_line(&mut line).unwrap();
        match parse_command(line.trim()) {
            Some(command) => match command {
                Command::Quit => return false,
                Command::NextInstruction => return true,
                Command::Stack => print_stack(&state.stack),
                Command::Pointer(address) => read_pointer(&state, address),
            }
            None => {
                println!("Unknown command {}", line);
            }
        }
        line.clear();
    }
}

fn read_pointer(state: &EvaluatorState, address: usize) {
    let value = state.read_pointer(address as _);
    if value.is_pointer() {
        println!("#{:x}", value.unwrap_pointer());
    } else {
        println!("{}", value.unwrap_value());
    }
}

fn _print_heap_at(heap: &memory::allocator::Allocator, address: usize) {
    dbg!(address);
    let address = memory::HEAP_POINTER | address as u64;
    let entry = heap.read_flagged_word(address);
    if entry.is_pointer() {
        println!("#{:x}", entry.unwrap_pointer());
    } else {
        println!("{}", entry.unwrap_value());
    }
}

fn print_stack(stack: &super::memory::stack::Stack) {
    println!("{} Entries in stack:", stack.len());
    for i in 0..stack.len() {
        let entry = stack.read_flagged_word(i as u64 * memory::WORD_SIZE_IN_BYTES);
        if entry.is_pointer() {
            println!("#{:x}", entry.unwrap_pointer());
        } else {
            println!("{}", entry.unwrap_value());
        }
    }
}

enum Command {
    Quit,
    NextInstruction,
    Stack,
    Pointer(usize),
}

fn parse_command(input: &str) -> Option<Command> {
    match &input.split(' ').collect::<Vec<_>>()[..] {
        &["ptr", arg] => {
            if arg.starts_with("0x") {
                Some(Command::Pointer(usize::from_str_radix(&arg[2..], 16).ok()?))
            } else {
                Some(Command::Pointer(arg.parse().ok()?))
            }
        }
        &["stack"] => Some(Command::Stack),
        &["q"|"quit"] => Some(Command::Quit),
        &["n"|"next"] => Some(Command::NextInstruction),
        _ => None,
    }
}