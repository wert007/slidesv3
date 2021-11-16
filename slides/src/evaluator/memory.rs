pub mod allocator;
pub mod stack;
pub mod static_memory;

pub const HEAP_POINTER: u64 = 0x80_00_00_00_00_00_00_00;
pub const WORD_SIZE_IN_BYTES: u64 = 8;

pub const fn bytes_to_word(bytes: u64) -> u64 {
    (bytes + WORD_SIZE_IN_BYTES - 1) / WORD_SIZE_IN_BYTES
}

pub const fn is_heap_pointer(address: u64) -> bool {
    address & HEAP_POINTER > 0
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Flags {
    pub is_pointer: bool,
}

impl Flags {
    pub fn pointer(mut self) -> Self {
        self.is_pointer = true;
        self
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct FlaggedWord {
    pub value: u64,
    pub flags: Flags,
}

impl FlaggedWord {
    pub fn unwrap_value(self) -> u64 {
        #[cfg(debug_assertions)]
        assert!(!self.flags.is_pointer, "{:#?}", self);
        self.value
    }

    pub fn unwrap_pointer(self) -> u64 {
        #[cfg(debug_assertions)]
        assert!(self.flags.is_pointer, "{:#?}", self);
        self.value
    }

    pub fn value(value: u64) -> Self {
        Self {
            value,
            flags: Flags::default(),
        }
    }

    pub fn pointer(value: u64) -> Self {
        Self {
            value,
            flags: Flags::default().pointer(),
        }
    }

    pub fn flags(mut self, flags: Flags) -> Self {
        self.flags = flags;
        self
    }

    pub fn is_pointer(&self) -> bool {
        self.flags.is_pointer
    }
}
