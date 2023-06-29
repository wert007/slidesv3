use crate::evaluator::memory::{
    allocator::{self, Bucket},
    is_heap_pointer, Memory, WORD_SIZE_IN_BYTES,
};

use super::Allocator;

pub struct GarbageCollectStats {
    pub freed_buckets: usize,
    pub folded_buckets: usize,
}

pub fn garbage_collect(
    mut unchecked_pointers: Vec<u64>,
    heap: &mut Allocator,
) -> GarbageCollectStats {
    let mut checked_pointers = vec![];

    while let Some(pointer) = unchecked_pointers.pop() {
        if !is_heap_pointer(pointer) {
            continue;
        }
        let pointer = allocator::clear_address(pointer);
        let &Bucket {
            address,
            size_in_words,
            is_used,
            ..
        } = heap
            .find_bucket_from_address(pointer)
            .unwrap_or_else(|| panic!("Found very stale pointer! pointer = {pointer:X}"));
        if !is_used {
            panic!("Found stale pointer for some reason!");
        }
        let address = address * WORD_SIZE_IN_BYTES;
        checked_pointers.push(address);
        let mut address = address;
        let end_address = address + size_in_words * WORD_SIZE_IN_BYTES;
        while address + WORD_SIZE_IN_BYTES < end_address {
            let value = heap.read_flagged_word(address);
            if value.is_pointer() {
                unchecked_pointers.push(value.unwrap_pointer());
            }
            address += WORD_SIZE_IN_BYTES;
        }
    }
    let mut freed_buckets = 0;
    for bucket in heap.used_buckets_mut() {
        let mut is_used = false;
        for pointer in &checked_pointers {
            let address = pointer / WORD_SIZE_IN_BYTES;
            if bucket.address == address {
                is_used = true;
                break;
            }
        }
        bucket.is_used = is_used;
        if !is_used {
            freed_buckets += 1;
        }
    }
    let folded_buckets = heap.fold_free_buckets();
    GarbageCollectStats {
        freed_buckets,
        folded_buckets,
    }
}
