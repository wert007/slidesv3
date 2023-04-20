use crate::DebugFlags;

use super::{static_memory::StaticMemory, FlaggedWord, Memory};

pub struct Stack {
    pub words: Vec<FlaggedWord>,
    // pub data: Vec<u64>,
    // pub flags: Vec<Flags>,
    print_stack: bool,
    static_memory_size: usize,
}

impl Stack {
    pub fn new(debug_flags: DebugFlags) -> Self {
        Self {
            words: Vec::new(),
            print_stack: debug_flags.print_stack,
            static_memory_size: 0,
        }
    }

    pub fn len(&self) -> usize {
        self.words.len()
    }

    pub fn pop(&mut self) -> FlaggedWord {
        debug_assert!(!self.words.is_empty());
        let word = self.words.pop().unwrap();
        self.print_maybe_stack();
        word
    }

    pub fn push(&mut self, value: u64) {
        self.words.push(FlaggedWord::value(value));
        self.print_maybe_stack();
    }

    pub fn push_with_comment(&mut self, value: u64, comment: &'static str) {
        self.words
            .push(FlaggedWord::value(value).with_comment(comment));
        self.print_maybe_stack();
    }

    pub fn push_pointer(&mut self, value: u64) {
        self.words.push(FlaggedWord::pointer(value));
        self.print_maybe_stack();
    }

    pub fn push_pointer_with_comment(&mut self, value: u64, comment: &'static str) {
        self.words
            .push(FlaggedWord::pointer(value).with_comment(comment));
        self.print_maybe_stack();
    }

    pub fn push_flagged_word(&mut self, value: FlaggedWord) {
        self.words.push(value);
        self.print_maybe_stack();
    }

    pub fn push_static_memory(&mut self, static_memory: StaticMemory) {
        assert_eq!(self.static_memory_size, 0);
        let static_memory_size = static_memory.data.len();
        for word in static_memory.data {
            self.push(word);
        }
        self.print_maybe_stack();
        self.static_memory_size = static_memory_size;
    }

    fn print_maybe_stack(&self) {
        if !self.print_stack {
            return;
        }
        print!("stack = [");
        let mut is_first = true;
        for word in self.words.iter().skip(self.static_memory_size) {
            if !is_first {
                print!(", ");
            }
            is_first = false;
            if word.flags.is_pointer {
                print!("#");
            }
            print!("{:x}", word.value);
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

    pub fn remove_at_offset(&mut self, offset: u64) -> FlaggedWord {
        let index = self.words.len() - offset as usize - 1;
        let word = self.words.remove(index);
        self.print_maybe_stack();
        word
    }
}

impl Memory for Stack {
    fn read_flagged_word_aligned(&self, address: u64) -> &FlaggedWord {
        &self.words[address as usize]
    }

    fn write_flagged_word_aligned(&mut self, address: u64, value: FlaggedWord) {
        self.words[address as usize] = value;
        self.print_maybe_stack();
    }

    fn len(&self) -> usize {
        self.words.len()
    }
}

impl std::fmt::Debug for Stack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "stack = [")?;
        let mut is_first = true;
        for word in &self.words {
            if !is_first {
                write!(f, ", ")?;
            }
            is_first = false;
            if word.flags.is_pointer {
                write!(f, "#")?;
            }
            write!(f, "{:x}", word.value)?;
        }
        write!(f, "]")
    }
}
