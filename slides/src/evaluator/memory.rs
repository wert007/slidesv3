pub mod allocator;
pub mod stack;

pub const HEAP_POINTER: u64 = 0x80_00_00_00_00_00_00_00;
pub const WORD_SIZE_IN_BYTES: u64 = 8;

pub const fn bytes_to_word(bytes: u64) -> u64 {
    (bytes + WORD_SIZE_IN_BYTES - 1) / WORD_SIZE_IN_BYTES
}

pub const fn is_heap_pointer(address: u64) -> bool {
    address & HEAP_POINTER > 0
}
