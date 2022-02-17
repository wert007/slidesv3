use crate::evaluator::memory;

use super::{EvaluatorState, memory::FlaggedWord};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SessionState {
    Continue,
    Quit,
    SkipFunction,
}

impl Default for SessionState {
    fn default() -> Self {
        Self::Quit
    }
}

#[derive(Default, Debug)]
pub struct DebuggerState {
    register_names: Vec<Option<String>>,
    pub session_state: SessionState,
}

impl DebuggerState {
    pub fn skip_function(&self) -> bool {
        self.session_state == SessionState::SkipFunction
    }
}

pub fn create_session(state: &mut EvaluatorState) {
    println!("This is the debugger. Type :q to exit the debugger.");
    let mut line = String::new();
    loop {
        std::io::stdin().read_line(&mut line).unwrap();
        match parse_command(line.trim()) {
            Some(command) => match command {
                Command::Quit => {
                    state.debugger_state.session_state = SessionState::Quit;
                    return;
                }
                Command::NextInstruction => {
                    state.debugger_state.session_state = SessionState::Continue;
                    return;
                }
                Command::Skip => {
                    state.debugger_state.session_state = SessionState::SkipFunction;
                    return;
                }
                Command::Stack => print_stack(&state.stack, state.static_memory_size_in_words),
                Command::Replace(new_value) => {
                    let flags = state.stack.pop().flags;
                    state.stack.push_flagged_word(FlaggedWord::value(new_value).flags(flags));
                }
                Command::Registers => print_registers(&state.registers),
                Command::Pointer(address) => read_pointer(state, address),
            },
            None => {
                println!("Unknown command {}", line);
            }
        }
        line.clear();
    }
}

fn print_registers(registers: &[FlaggedWord]) {
    for (index, value) in registers.iter().enumerate() {
        print!("r#{}: ", index);
        if value.is_pointer() {
            println!("#{:x}", value.unwrap_pointer());
        } else {
            println!("{}", value.unwrap_value());
        }
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

fn print_stack(stack: &super::memory::stack::Stack, skip: usize) {
    println!("{} Entries in stack:", stack.len() - skip);
    for i in skip..stack.len() {
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
    Skip,
    Stack,
    Registers,
    Pointer(usize),
    Replace(u64),
}

fn parse_command(input: &str) -> Option<Command> {
    match input.split(' ').collect::<Vec<_>>()[..] {
        ["ptr", arg] => {
            if let Some(arg) = arg.strip_prefix("0x") {
                Some(Command::Pointer(usize::from_str_radix(arg, 16).ok()?))
            } else {
                Some(Command::Pointer(arg.parse().ok()?))
            }
        }
        ["replace", arg] => {
            if let Some(arg) = arg.strip_prefix("0x") {
                Some(Command::Replace(u64::from_str_radix(arg, 16).ok()?))
            } else {
                Some(Command::Replace(arg.parse().ok()?))
            }
        }
        ["stack"] => Some(Command::Stack),
        ["registers"] => Some(Command::Registers),
        ["q" | "quit"] => Some(Command::Quit),
        ["n" | "next"] => Some(Command::NextInstruction),
        ["s" | "skip"] => Some(Command::Skip),
        _ => None,
    }
}
