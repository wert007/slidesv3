use crate::DebugFlags;

use super::WORD_SIZE_IN_BYTES;

#[derive(Default, Debug, Clone)]
pub struct StaticMemory {
    pub data: Vec<u64>,
    pub flags: Vec<super::Flags>,
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
        self.push_pointer(result + 2 * WORD_SIZE_IN_BYTES);
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
        self.flags.append(&mut other.flags);
    }

    pub fn size_in_bytes(&self) -> u64 {
        WORD_SIZE_IN_BYTES * self.data.len() as u64
    }

    fn push_immediate(&mut self, value: u64) {
        self.data.push(value);
        self.flags.push(super::Flags::default());
    }

    fn push_pointer(&mut self, value: u64) {
        self.data.push(value);
        self.flags.push(super::Flags::default().pointer());
    }
}

pub fn print_static_memory_as_string(static_memory: &StaticMemory) -> String {
    let mut result = Vec::with_capacity(static_memory.size_in_bytes() as _);
    for word in &static_memory.data {
        result.extend_from_slice(&word.to_be_bytes());
    }
    String::from_utf8_lossy(&result).into_owned()
}
