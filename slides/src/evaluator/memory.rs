use std::{borrow::Cow, fmt::Display};

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

pub trait Memory {
    fn read_flagged_byte(&self, address: u64) -> FlaggedByte {
        let word = self.read_flagged_word_aligned(address / WORD_SIZE_IN_BYTES);
        let byte = word.value.to_le_bytes()[(address % WORD_SIZE_IN_BYTES) as usize];
        FlaggedByte {
            value: byte,
            flags: word.flags,
            comment: word.comment.clone(),
        }
    }

    fn read_flagged_word_aligned(&self, address: u64) -> &FlaggedWord;
    fn write_flagged_word_aligned(&mut self, address: u64, value: FlaggedWord);
    fn len(&self) -> usize;
    fn transform_address(&self, address: u64) -> u64 {
        address
    }

    fn read_flagged_word<'a>(&'a self, address: u64) -> Cow<'a, FlaggedWord> {
        let address = self.transform_address(address);
        if address % WORD_SIZE_IN_BYTES == 0 {
            Cow::Borrowed(self.read_flagged_word_aligned(address / WORD_SIZE_IN_BYTES))
        } else {
            let a = self.read_flagged_word_aligned(address / WORD_SIZE_IN_BYTES);
            let b = self.read_flagged_word_aligned(address / WORD_SIZE_IN_BYTES + 1);
            assert_eq!(a.flags, b.flags);
            let offset = address & (WORD_SIZE_IN_BYTES - 1);
            let a_mask = (1 << offset) - 1;
            let b_mask = u64::MAX - (1 << offset) - 1;
            let value = (a.value & a_mask) | (b.value & b_mask);
            Cow::Owned(FlaggedWord::value(value).flags(a.flags))
        }
    }

    fn read_flagged_word_safe(&self, address: u64) -> Option<&FlaggedWord> {
        let address = self.transform_address(address);
        if self.len() <= (address / WORD_SIZE_IN_BYTES) as usize {
            return None;
        }
        if address % WORD_SIZE_IN_BYTES == 0 {
            Some(self.read_flagged_word_aligned(address / WORD_SIZE_IN_BYTES))
        } else {
            // unimplemented!("address = 0x{:x}", address)
            return None;
        }
    }

    fn read_flagged_byte_safe(&self, address: u64) -> Option<FlaggedByte> {
        let address = self.transform_address(address);
        if self.len() <= (address / WORD_SIZE_IN_BYTES) as usize {
            return None;
        }
        Some(self.read_flagged_byte(address))
    }

    fn write_flagged_word(&mut self, address: u64, value: FlaggedWord) {
        let address = self.transform_address(address);
        if address % WORD_SIZE_IN_BYTES == 0 {
            self.write_flagged_word_aligned(address / WORD_SIZE_IN_BYTES, value);
        } else {
            unimplemented!("address = 0x{:x}", address);
        }
    }

    fn write_flagged_byte(&mut self, address: u64, value: FlaggedByte) {
        let address = self.transform_address(address);
        let word = self.read_flagged_word_aligned(address / WORD_SIZE_IN_BYTES);
        let offset = address % WORD_SIZE_IN_BYTES;
        let mask = 0xff << (offset * 8);
        assert_eq!(word.flags, value.flags);
        let flags = value.flags;
        let old = word.value & !mask;
        let new = ((value.value as u64) << (offset * 8)) & mask;
        let value = old | new;
        self.write_flagged_word_aligned(
            address / WORD_SIZE_IN_BYTES,
            FlaggedWord::value(value).flags(flags),
        );
    }
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

#[derive(Default, Clone, PartialEq, Eq)]
pub struct FlaggedByte {
    pub value: u8,
    pub flags: Flags,
    pub comment: String,
}

impl Display for FlaggedByte {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.flags.is_pointer {
            write!(f, "#{:x}", self.value)
        } else {
            write!(f, "{}", self.value)
        }
    }
}

#[derive(Default, Clone, PartialEq, Eq)]
pub struct FlaggedWord {
    pub value: u64,
    pub flags: Flags,
    pub comment: String,
}

impl FlaggedWord {
    pub fn with_comment(mut self, comment: impl Into<String>) -> Self {
        self.comment = comment.into();
        self
    }

    pub fn replace_comment(mut self, cb: impl FnOnce(String) -> String) -> Self {
        self.comment = cb(self.comment);
        self
    }

    pub fn unwrap_value(&self) -> u64 {
        #[cfg(debug_assertions)]
        assert!(!self.flags.is_pointer, "{:#?}", self);
        self.value
    }

    pub fn unwrap_pointer(&self) -> u64 {
        #[cfg(debug_assertions)]
        assert!(self.flags.is_pointer, "{:#?}", self);
        self.value
    }

    pub fn value(value: u64) -> Self {
        Self {
            value,
            flags: Flags::default(),
            comment: String::new(),
        }
    }

    pub fn pointer(value: u64) -> Self {
        Self {
            value,
            flags: Flags::default().pointer(),
            comment: String::new(),
        }
    }

    pub fn flags(mut self, flags: Flags) -> Self {
        self.flags = flags;
        self
    }

    pub fn is_pointer(&self) -> bool {
        self.flags.is_pointer
    }

    pub(crate) fn as_value(&self) -> Result<u64, u64> {
        (!self.flags.is_pointer)
            .then(|| self.value)
            .ok_or(self.value)
    }

    pub(crate) fn as_pointer(&self) -> Result<u64, u64> {
        self.flags.is_pointer.then(|| self.value).ok_or(self.value)
    }

    pub(crate) fn into_flagged_byte(self) -> Option<FlaggedByte> {
        if self.value <= u8::MAX as _ {
            Some(FlaggedByte {
                value: self.value as _,
                flags: self.flags,
                comment: self.comment,
            })
        } else {
            None
        }
    }
}

impl std::fmt::Debug for FlaggedWord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = if self.flags.is_pointer {
            format!("#{:x}", self.value)
        } else {
            format!("{}", self.value)
        };
        f.debug_struct("FlaggedWord")
            .field("value", &value)
            .field("comment", &self.comment)
            .finish()
    }
}

impl Display for FlaggedWord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.flags.is_pointer {
            write!(f, "#{:x}", self.value)
        } else {
            write!(f, "{}", self.value as i64)
        }
    }
}

impl From<FlaggedByte> for FlaggedWord {
    fn from(value: FlaggedByte) -> Self {
        Self {
            value: value.value as _,
            flags: value.flags,
            comment: value.comment,
        }
    }
}
