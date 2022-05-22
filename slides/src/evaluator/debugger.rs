use crate::evaluator::memory;

use super::{memory::FlaggedWord, EvaluatorState};

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
    last_command: Command,
}

impl DebuggerState {
    pub fn skip_function(&self) -> bool {
        self.session_state == SessionState::SkipFunction
    }
}

pub fn create_session(state: &mut EvaluatorState) {
    if state.pc >= state.instructions.len() {
        return;
    }
    let current_instruction = state.instructions[state.pc];
    let reg_name = if current_instruction.arg < state.debugger_state.register_names.len() as u64 {
        state.debugger_state.register_names[current_instruction.arg as usize].as_deref()
    } else {
        None
    };
    println!(
        "{:5X}: {}",
        state.pc,
        crate::debug::instruction_to_string(current_instruction, reg_name)
    );
    let mut line = String::new();
    loop {
        std::io::stdin().read_line(&mut line).unwrap();
        match parse_command(line.trim()) {
            Some(command) => {
                let command = if command == Command::Repeat {
                    &state.debugger_state.last_command
                } else {
                    &command
                };
                match command {
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
                        state
                            .stack
                            .push_flagged_word(FlaggedWord::value(*new_value).flags(flags));
                    }
                    Command::Registers => {
                        print_registers(&state.registers, &state.debugger_state.register_names)
                    }
                    Command::Pointer(address) => read_pointer(state, *address),
                    Command::IndirectPointer(address) => read_indirect_pointer(state, *address),
                    Command::Register(register, offset) => {
                        read_pointer(state, state.registers[*register].value + *offset)
                    }
                    Command::IndirectRegister(register, offset) => {
                        read_indirect_pointer(state, state.registers[*register].value + *offset)
                    }
                    Command::Repeat => unreachable!(),
                    Command::RenameRegister(register, name) => {
                        state
                            .debugger_state
                            .register_names
                            .resize_with(*register + 1, Default::default);
                        state.debugger_state.register_names[*register] = Some(name.clone());
                    }
                    Command::Heapdump(file_name) => {
                        crate::debug::output_allocator_to_dot(file_name, &state.heap);
                    }
                }
            }
            None => {
                println!("Unknown command {}", line);
            }
        }
        line.clear();
    }
}

fn print_registers(registers: &[FlaggedWord], register_names: &[Option<String>]) {
    for (index, value) in registers.iter().enumerate() {
        if let Some(Some(name)) = register_names.get(index) {
            print!("{}: ", name);
        } else {
            print!("r#{}: ", index);
        }
        if value.is_pointer() {
            println!("#{:x}", value.unwrap_pointer());
        } else {
            println!("{}", value.unwrap_value());
        }
    }
}

fn read_indirect_pointer(state: &EvaluatorState, address: u64) {
    let value = state.read_word(address);
    let ptr = if value.is_pointer() {
        value.unwrap_pointer()
    } else {
        println!("{} is no pointer", value.unwrap_value());
        return;
    };
    print!("{:x}: ", ptr);
    let value = state.read_word(ptr);
    if value.is_pointer() {
        println!("#{:x}", value.unwrap_pointer());
    } else {
        println!("{}", value.unwrap_value());
    }
}

fn read_pointer(state: &EvaluatorState, address: u64) {
    let value = state.read_word(address);
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

#[derive(Debug, PartialEq, Eq, Clone)]
enum Command {
    Repeat,
    Quit,
    NextInstruction,
    Skip,
    Stack,
    Registers,
    Pointer(u64),
    IndirectPointer(u64),
    Register(usize, u64),
    IndirectRegister(usize, u64),
    Replace(u64),
    RenameRegister(usize, String),
    Heapdump(String),
}

impl Default for Command {
    fn default() -> Self {
        Self::NextInstruction
    }
}

fn parse_command(input: &str) -> Option<Command> {
    if input.starts_with("ptr") {
        parse_ptr_command(input)
    } else {
        match input.split(' ').collect::<Vec<_>>()[..] {
            ["heap", arg] => {
                let ptr = if let Some(arg) = arg.strip_prefix("0x") {
                    u64::from_str_radix(arg, 16).ok()?
                } else {
                    arg.parse().ok()?
                };
                Some(Command::Pointer(ptr | memory::HEAP_POINTER))
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
            ["rename", register, name] => {
                if let Some(register) = register.strip_prefix("r#") {
                    let register = register.parse().ok()?;
                    Some(Command::RenameRegister(register, name.into()))
                } else {
                    None
                }
            }
            ["q" | "quit"] => Some(Command::Quit),
            ["n" | "next"] => Some(Command::NextInstruction),
            ["s" | "skip"] => Some(Command::Skip),
            [""] => Some(Command::Repeat),
            ["heapdump", arg] => {
                Some(Command::Heapdump(arg.into()))
            }
            _ => None,
        }
    }
}

fn parse_ptr_command(input: &str) -> Option<Command> {
    match input.split(' ').collect::<Vec<_>>()[..] {
        ["ptr", arg] => {
            if let Some(reg) = arg.strip_prefix("r#") {
                Some(Command::Register(reg.parse().ok()?, 0))
            } else if let Some(arg) = arg.strip_prefix("0x") {
                Some(Command::Pointer(u64::from_str_radix(arg, 16).ok()?))
            } else {
                Some(Command::Pointer(arg.parse().ok()?))
            }
        }
        ["ptr", reg, "+", offset] => {
            if let Some(reg) = reg.strip_prefix("r#") {
                Some(Command::Register(reg.parse().ok()?, offset.parse().ok()?))
            } else {
                None
            }
        }
        ["ptrptr", arg] => {
            if let Some(reg) = arg.strip_prefix("r#") {
                Some(Command::IndirectRegister(reg.parse().ok()?, 0))
            } else if let Some(arg) = arg.strip_prefix("0x") {
                Some(Command::IndirectPointer(u64::from_str_radix(arg, 16).ok()?))
            } else {
                Some(Command::IndirectPointer(arg.parse().ok()?))
            }
        }
        ["ptrptr", reg, "+", offset] => {
            if let Some(reg) = reg.strip_prefix("r#") {
                Some(Command::IndirectRegister(
                    reg.parse().ok()?,
                    offset.parse().ok()?,
                ))
            } else {
                None
            }
        }
        _ => None,
    }
}
