use crate::{DebugFlags, evaluator::WORD_SIZE_IN_BYTES};

use super::{FlaggedWord, Flags};

pub struct Stack {
    pub data: Vec<u64>,
    pub flags: Vec<Flags>,
    print_stack: bool,
}

impl Stack {
    pub fn new(debug_flags: DebugFlags) -> Self {
        Self {
            data: vec![],
            flags: vec![],
            print_stack: debug_flags.print_stack,
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
        FlaggedWord {
            value,
            flags,
        }
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

    pub fn read_flagged_word(&self, address: u64) -> FlaggedWord {
        if address % WORD_SIZE_IN_BYTES == 0 {
            self.read_flagged_word_aligned(address / WORD_SIZE_IN_BYTES)
        } else {
            unimplemented!("address = 0x{:x}", address)
        }
    }

    fn read_flagged_word_aligned(&self, address: u64) -> FlaggedWord {
        FlaggedWord::value(self.data[address as usize]).flags(self.flags[address as usize])
    }

    pub fn write_word(&mut self, address: u64, value: u64) {
        if address % WORD_SIZE_IN_BYTES == 0 {
            self.write_word_aligned(address / WORD_SIZE_IN_BYTES, value);
        } else {
            unimplemented!("address = 0x{:x}", address);
        }
    }

    pub fn write_flagged_word(&mut self, address: u64, value: FlaggedWord) {
        if address % WORD_SIZE_IN_BYTES == 0 {
            self.write_flagged_word_aligned(address / WORD_SIZE_IN_BYTES, value);
        } else {
            unimplemented!("address = 0x{:x}", address);
        }
    }

    fn write_word_aligned(&mut self, address: u64, value: u64) {
        self.data[address as usize] = value;
        self.print_maybe_stack();
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
        println!("stack = {:x?}", self.data);
    }
}