use crate::DebugFlags;

use super::WORD_SIZE_IN_BYTES;

#[derive(Debug, Clone)]
pub enum StaticMemoryWord {
    Word(u64),
    RelativePointer(i64),
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
        self.data.push(StaticMemoryWord::RelativePointer(relative_pointer));
    }
}

pub fn print_static_memory_as_string(static_memory: &StaticMemory) -> String {
    let mut result = Vec::with_capacity(static_memory.size_in_bytes() as _);
    for word in &static_memory.data {
        match word {
            StaticMemoryWord::Word(word) => result.extend_from_slice(&word.to_be_bytes()),
            StaticMemoryWord::RelativePointer(_) => {},
        }
    }
    String::from_utf8_lossy(&result).into_owned()
}
