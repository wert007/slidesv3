use crate::evaluator::memory::{
    allocator::{self, Bucket},
    is_heap_pointer, WORD_SIZE_IN_BYTES,
};

use super::Allocator;

pub fn garbage_collect(mut unchecked_pointers: Vec<u64>, heap: &mut Allocator) {
    let mut checked_pointers = vec![];

    while let Some(pointer) = unchecked_pointers.pop() {
        if !is_heap_pointer(pointer) {
            continue;
        }
        let pointer = allocator::clear_address(pointer);
        let &Bucket {
            address,
            size_in_words,
            ..
        } = heap.find_bucket_from_address(pointer).unwrap();
        let address = address * WORD_SIZE_IN_BYTES;
        checked_pointers.push(address);
        let mut address = address;
        let end_address = address + size_in_words * WORD_SIZE_IN_BYTES;
        while address < end_address {
            let value = heap.read_flagged_word_unchecked(address);
            if value.is_pointer() {
                unchecked_pointers.push(value.unwrap_pointer());
            }
            address += WORD_SIZE_IN_BYTES;
        }
    }
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
    }
    heap.fold_free_buckets();
}
