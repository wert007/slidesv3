use std::fmt::Write;

use crate::DebugFlags;

use super::WORD_SIZE_IN_BYTES;

#[derive(Debug, Clone, Copy)]
pub enum StaticMemoryWord {
    Word(u64),
    RelativePointer(i64),
}

impl StaticMemoryWord {
    pub fn as_word(&self) -> Option<u64> {
        if let Self::Word(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    pub fn as_relative_pointer(&self) -> Option<i64> {
        if let Self::RelativePointer(v) = self {
            Some(*v)
        } else {
            None
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct StaticMemory {
    pub data: Vec<StaticMemoryWord>,
}

impl StaticMemory {
    pub fn new(_: DebugFlags) -> Self {
        Self::default()
    }

    pub fn allocate_string(&mut self, string: String) -> u64 {
        let result = self.data.len() as u64;
        let result = result * WORD_SIZE_IN_BYTES;
        let length = string.len() as _;
        self.push_immediate(length);
        self.push_pointer(WORD_SIZE_IN_BYTES as i64);
        for word in string.as_bytes().chunks(8) {
            let word = [
                word.get(0).copied().unwrap_or_default(),
                word.get(1).copied().unwrap_or_default(),
                word.get(2).copied().unwrap_or_default(),
                word.get(3).copied().unwrap_or_default(),
                word.get(4).copied().unwrap_or_default(),
                word.get(5).copied().unwrap_or_default(),
                word.get(6).copied().unwrap_or_default(),
                word.get(7).copied().unwrap_or_default(),
            ];
            let word = u64::from_be_bytes(word);
            self.push_immediate(word);
        }
        result
    }

    pub fn allocate_null(&mut self) -> u64 {
        let result = self.data.len() as u64 * WORD_SIZE_IN_BYTES;
        self.push_immediate(0);
        result
    }

    pub fn insert(&mut self, other: &mut Self) {
        self.data.append(&mut other.data);
    }

    pub fn size_in_bytes(&self) -> u64 {
        WORD_SIZE_IN_BYTES * self.data.len() as u64
    }

    fn push_immediate(&mut self, value: u64) {
        self.data.push(StaticMemoryWord::Word(value));
    }

    fn push_pointer(&mut self, relative_pointer: i64) {
        self.data
            .push(StaticMemoryWord::RelativePointer(relative_pointer));
    }

    pub fn try_read_string_from(&self, address: u64) -> Option<String> {
        if address == 0 {
            return None;
        }
        if address % WORD_SIZE_IN_BYTES != 0 {
            return None;
        }
        let index = (address / WORD_SIZE_IN_BYTES) as usize;
        let length_in_bytes = self.data[index].as_word()?;
        let length = super::bytes_to_word(length_in_bytes);
        let pointer = self.data[index + 1].as_relative_pointer()?;
        if pointer % WORD_SIZE_IN_BYTES as i64 != 0 {
            return None;
        }
        let pointer = (pointer / WORD_SIZE_IN_BYTES as i64) as isize;
        let pointer = (index as isize + pointer + 1) as usize;
        let mut data = Vec::with_capacity(length as usize);
        for index in pointer..pointer + length as usize {
            let word = self.data[index].as_word()?;
            data.extend_from_slice(&word.to_be_bytes());
        }
        Some(String::from_utf8_lossy(&data[..length_in_bytes as usize]).to_string())
    }
}

#[must_use]
pub fn print_static_memory_as_string(static_memory: &StaticMemory) -> String {
    let mut result = Vec::with_capacity(static_memory.size_in_bytes() as _);
    for word in &static_memory.data {
        match word {
            StaticMemoryWord::Word(word) => result.extend_from_slice(&word.to_be_bytes()),
            StaticMemoryWord::RelativePointer(_) => {}
        }
    }
    String::from_utf8_lossy(&result).into_owned()
}

#[must_use]
pub fn print_static_memory_as_hex(static_memory: &StaticMemory) -> String {
    let mut result = String::with_capacity(static_memory.size_in_bytes() as usize / 4 * 6);
    for (index, word) in static_memory.data.iter().enumerate() {
        let addr = index as u64 * WORD_SIZE_IN_BYTES;
        match index % 2 {
            1 => write!(result, "  "),
            0 => write!(result, "\n{addr:16x}: "),
            _ => unreachable!(),
        }
        .unwrap();
        match word {
            StaticMemoryWord::Word(word) => write!(result, " {word:016x}"),
            StaticMemoryWord::RelativePointer(pointer) => {
                write!(result, "#{:016x}", (addr as i64 + pointer) as usize)
            }
        }
        .unwrap();
    }
    result
}
