use crate::{
    evaluator::{memory::static_memory::StaticMemoryWord, WORD_SIZE_IN_BYTES},
    DebugFlags,
};

use super::{static_memory::StaticMemory, FlaggedWord, Flags};

pub struct Stack {
    pub data: Vec<u64>,
    pub flags: Vec<Flags>,
    print_stack: bool,
    static_memory: StaticMemory,
}

impl Stack {
    pub fn new(debug_flags: DebugFlags) -> Self {
        Self {
            data: vec![],
            flags: vec![],
            print_stack: debug_flags.print_stack,
            static_memory: StaticMemory::new(debug_flags),
        }
    }

    pub fn len(&self) -> usize {
        debug_assert_eq!(self.data.len(), self.flags.len());
        self.data.len()
    }

    pub fn pop(&mut self) -> FlaggedWord {
        debug_assert!(!self.data.is_empty());
        let value = self.data.pop().unwrap();
        let flags = self.flags.pop().unwrap();
        self.print_maybe_stack();
        FlaggedWord { value, flags }
    }

    pub fn push(&mut self, value: u64) {
        self.data.push(value);
        self.flags.push(Flags::default());
        self.print_maybe_stack();
    }

    pub fn push_pointer(&mut self, value: u64) {
        self.data.push(value);
        self.flags.push(Flags::default().pointer());
        self.print_maybe_stack();
    }

    pub fn push_flagged_word(&mut self, value: FlaggedWord) {
        self.data.push(value.value);
        self.flags.push(value.flags);
        self.print_maybe_stack();
    }

    pub fn push_static_memory(&mut self, static_memory: StaticMemory) {
        assert_eq!(self.static_memory.size_in_bytes(), 0);
        let mut address = 0;
        for &word in &static_memory.data {
            match word {
                StaticMemoryWord::Word(word) => {
                    self.data.push(word);
                    self.flags.push(Flags::default());
                }
                StaticMemoryWord::RelativePointer(relative_pointer) => {
                    let word = if relative_pointer > 0 {
                        address + relative_pointer as u64
                    } else {
                        address - relative_pointer.abs() as u64
                    };
                    self.data.push(word);
                    self.flags.push(Flags::default().pointer());
                }
            }
            address += WORD_SIZE_IN_BYTES;
        }
        self.print_maybe_stack();
        self.static_memory = static_memory;
    }

    pub fn static_memory(&self) -> &StaticMemory {
        &self.static_memory
    }

    pub fn read_flagged_word(&self, address: u64) -> FlaggedWord {
        if address % WORD_SIZE_IN_BYTES == 0 {
            self.read_flagged_word_aligned(address / WORD_SIZE_IN_BYTES)
        } else {
            unimplemented!("address = 0x{:x}", address)
        }
    }

    pub fn read_flagged_byte(&self, address: u64) -> FlaggedWord {
        let value = self.read_flagged_word_aligned(address / WORD_SIZE_IN_BYTES);
        let byte = value.value.to_be_bytes()[(address % WORD_SIZE_IN_BYTES) as usize];
        FlaggedWord {
            value: byte as _,
            flags: value.flags,
        }
    }

    fn read_flagged_word_aligned(&self, address: u64) -> FlaggedWord {
        FlaggedWord::value(self.data[address as usize]).flags(self.flags[address as usize])
    }

    pub fn write_flagged_word(&mut self, address: u64, value: FlaggedWord) {
        if address % WORD_SIZE_IN_BYTES == 0 {
            self.write_flagged_word_aligned(address / WORD_SIZE_IN_BYTES, value);
        } else {
            unimplemented!("address = 0x{:x}", address);
        }
    }

    pub fn write_flagged_byte(&mut self, address: u64, value: FlaggedWord) {
        let old_value = self
            .read_flagged_word_aligned(address / WORD_SIZE_IN_BYTES)
            .value;
        let mut bytes = old_value.to_be_bytes();
        bytes[(address % WORD_SIZE_IN_BYTES) as usize] = value.value as u8;
        let word = u64::from_be_bytes(bytes);
        self.write_flagged_word_aligned(
            address / WORD_SIZE_IN_BYTES,
            FlaggedWord {
                value: word,
                flags: value.flags,
            },
        );
    }

    fn write_flagged_word_aligned(&mut self, address: u64, value: FlaggedWord) {
        self.data[address as usize] = value.value;
        self.flags[address as usize] = value.flags;
        self.print_maybe_stack();
    }

    fn print_maybe_stack(&self) {
        if !self.print_stack {
            return;
        }
        print!("stack = [");
        let mut is_first = true;
        for (value, flags) in self
            .data
            .iter()
            .zip(&self.flags)
            .skip(self.static_memory.data.len())
        {
            if !is_first {
                print!(", ");
            }
            is_first = false;
            if flags.is_pointer {
                print!("#");
            }
            print!("{:x}", value);
        }
        println!("]");
    }

    pub(crate) fn pop_print_stack(&mut self) -> bool {
        let result = self.print_stack;
        self.print_stack = false;
        result
    }

    pub(crate) fn push_print_stack(&mut self, print_stack_value: bool) {
        self.print_stack = print_stack_value;
        self.print_maybe_stack();
    }
}

impl std::fmt::Debug for Stack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "stack = [")?;
        let mut is_first = true;
        for (value, flags) in self.data.iter().zip(&self.flags) {
            if !is_first {
                write!(f, ", ")?;
            }
            is_first = false;
            if flags.is_pointer {
                write!(f, "#")?;
            }
            write!(f, "{:x}", value)?;
        }
        write!(f, "]")
    }
}
