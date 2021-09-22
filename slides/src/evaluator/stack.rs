use crate::DebugFlags;

use super::{TypedU64, WORD_SIZE_IN_BYTES};

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

    pub fn pop(&mut self) -> TypedU64 {
        debug_assert!(!self.data.is_empty());
        let value = self.data.pop().unwrap();
        let flags = self.flags.pop().unwrap();
        self.print_maybe_stack();
        TypedU64 {
            value,
            is_pointer: flags.is_pointer,
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

    pub fn is_pointer(&self, address: usize) -> bool {
        assert_eq!(address % WORD_SIZE_IN_BYTES as usize, 0);
        let address = address / WORD_SIZE_IN_BYTES as usize;
        self.flags[address].is_pointer
    }

    pub fn set_pointer(&mut self, address: usize) {
        assert_eq!(address % WORD_SIZE_IN_BYTES as usize, 0);
        let address = address / WORD_SIZE_IN_BYTES as usize;
        self.flags[address].is_pointer = true;
    }

    pub fn read_word(&self, address: usize) -> u64 {
        if address % WORD_SIZE_IN_BYTES as usize == 0 {
            self.read_word_aligned(address / WORD_SIZE_IN_BYTES as usize)
        } else {
            unimplemented!("address = 0x{:x}", address)
        }
    }

    fn read_word_aligned(&self, address: usize) -> u64 {
        self.data[address]
    }

    pub fn write_word(&mut self, address: usize, value: u64) {
        if address % WORD_SIZE_IN_BYTES as usize == 0 {
            self.write_word_aligned(address / WORD_SIZE_IN_BYTES as usize, value);
        } else {
            unimplemented!("address = 0x{:x}", address);
        }
    }

    fn write_word_aligned(&mut self, address: usize, value: u64) {
        self.data[address] = value;
        self.print_maybe_stack();
    }

    fn print_maybe_stack(&self) {
        if !self.print_stack {
            return;
        }
        println!("stack = {:x?}", self.data);
    }
}

#[derive(Debug, Default)]
pub struct Flags {
    pub is_pointer: bool,
}

impl Flags {
    pub fn pointer(mut self) -> Self{
        self.is_pointer = true;
        self
    }
}
