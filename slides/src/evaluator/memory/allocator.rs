use crate::{evaluator::memory::Flags, DebugFlags};

use super::{bytes_to_word, FlaggedWord, HEAP_POINTER, WORD_SIZE_IN_BYTES};

pub mod garbage_collector;

#[derive(Debug)]
pub struct Allocator {
    pub data: Vec<u64>,
    pub flags: Vec<Flags>,
    pub buckets: Vec<BucketEntry>,
    debug_heap_as_string: bool,
}

impl Allocator {
    pub fn new(size_in_bytes: u64, debug_flags: DebugFlags) -> Self {
        assert_eq!(size_in_bytes % WORD_SIZE_IN_BYTES, 0);
        let size_in_words = bytes_to_word(size_in_bytes);
        assert!(size_in_words.is_power_of_two());
        Self {
            data: vec![0; size_in_words as usize],
            flags: vec![Flags::default(); size_in_words as usize],
            buckets: vec![BucketEntry::root(size_in_words)],
            debug_heap_as_string: debug_flags.print_heap_as_string,
        }
    }

    // #[cfg(debug_assertions)]
    fn find_bucket_from_address(&self, address: u64) -> &Bucket {
        for bucket in &self.buckets {
            if let BucketEntry::Bucket(bucket) = bucket {
                if bucket.address * WORD_SIZE_IN_BYTES <= address
                    && (bucket.address + bucket.size_in_words) * WORD_SIZE_IN_BYTES > address
                {
                    return bucket;
                }
            }
        }
        panic!(
            "Address 0x{:x} could not be assigned to a bucket! All Buckets ({}):\n\n{:#?}",
            address,
            self.buckets.len(),
            &self
                .buckets
                .iter()
                .filter_map(|b| b.as_bucket())
                .collect::<Vec<_>>()
        );
    }

    fn find_bucket_from_address_mut(&mut self, address: u64) -> Option<&mut Bucket> {
        self.buckets.iter_mut().filter_map(|b| b.as_bucket_mut()).find(|b| b.address * WORD_SIZE_IN_BYTES == address)
    }

    fn free_buckets(&mut self, min_size: u64) -> Vec<&mut Bucket> {
        let mut result: Vec<_> = self
            .buckets
            .iter_mut()
            .filter_map(|b| match b {
                BucketEntry::Bucket(b) if !b.is_used && b.size_in_words >= min_size => Some(b),
                _ => None,
            })
            .collect();
        result.sort_by_key(|b| u64::MAX - b.size_in_words);
        result
    }

    fn used_buckets_mut(&mut self) -> impl Iterator<Item = &mut Bucket> {
        self.buckets.iter_mut().filter_map(|b| match b {
            BucketEntry::Bucket(b) if b.is_used => Some(b),
            _ => None,
        })
    }

    fn fold_free_buckets(&mut self) {
        for index in 0..self.buckets.len() {
            if self.buckets[index].as_bucket().is_none() {
                continue;
            }
            if self.buckets[index].as_bucket().unwrap().is_used {
                continue;
            }
            if self.buckets[index].buddy_index().is_none() {
                continue;
            }
            let buddy_index = self.buckets[index].buddy_index().unwrap();
            if self.buckets[buddy_index].as_bucket().is_none() {
                continue;
            }
            if self.buckets[buddy_index].as_bucket().unwrap().is_used {
                continue;
            }
            let parent_index = self.buckets[index].parent_index().unwrap();
            self.buckets[index] = BucketEntry::Tombstone;
            self.buckets[buddy_index] = BucketEntry::Tombstone;
            self.buckets[parent_index] = BucketEntry::Bucket(Bucket::combine(
                self.buckets[parent_index].as_parent().unwrap(),
            ));
        }
    }

    fn find_next_two_bucket_indices(&self) -> [usize; 2] {
        let fallback = [self.buckets.len(), self.buckets.len() + 1];
        let mut iter = self
            .buckets
            .iter()
            .enumerate()
            .filter_map(|(index, bucket)| {
                if matches!(bucket, BucketEntry::Tombstone) {
                    Some(index)
                } else {
                    None
                }
            });
        [
            iter.next().unwrap_or(fallback[0]),
            iter.next().unwrap_or(fallback[1]),
        ]
    }

    pub fn reallocate(&mut self, address: u64, size_in_bytes: u64) -> u64 {
        let result = {
            let size_in_words = bytes_to_word(size_in_bytes);
            let expected_size = size_in_words.next_power_of_two();
            let bucket_index = {
                let bucket = if address == 0 {
                    None
                } else {
                    self.find_bucket_from_address_mut(address)
                    .filter(|b| b.size_in_words >= size_in_words)
                };
                match bucket {
                    Some(bucket) => bucket.index,
                    None => {
                        let mut free_buckets = self.free_buckets(expected_size);
                        if free_buckets.is_empty() {
                            // eprintln!("No Memory left!!!!");
                            return 0;
                        }
                        free_buckets.remove(0).index
                    },
                }
            };

            while self.buckets[bucket_index].size_in_words() / 2 >= expected_size {
                let old_buddy_index = self.buckets[bucket_index].buddy_index();
                let old_parent_index = self.buckets[bucket_index].parent_index();
                let [buddy_index, parent_index] = self.find_next_two_bucket_indices();
                let (tmp_bucket, parent) = Bucket::split(
                    self.buckets[bucket_index].as_bucket_mut().unwrap(),
                    buddy_index,
                    parent_index,
                );

                if let Some(old_buddy) = old_buddy_index {
                    self.buckets[old_buddy].set_buddy_index(Some(parent.index));
                }
                if let Some(old_parent) = old_parent_index {
                    if let Some(old_parent) =
                        self.buckets.get_mut(old_parent).unwrap().as_parent_mut()
                    {
                        let index = old_parent
                            .child_indices
                            .iter()
                            .position(|&child_index| child_index == bucket_index)
                            .unwrap();
                        old_parent.child_indices[index] = parent.index;
                    } else {
                        unreachable!(
                            "Bucket had a {:#?} as parent and not an actual parent.",
                            self.buckets[old_parent]
                        );
                    }
                }
                // let index = tmp_bucket.index;
                while self.buckets.len() <= parent_index {
                    self.buckets.push(BucketEntry::Tombstone);
                }
                self.buckets[buddy_index] = BucketEntry::Bucket(tmp_bucket);
                self.buckets[parent_index] = BucketEntry::Parent(parent);
                // bucket_index = self.buckets[index].as_bucket_mut().unwrap().index;
                assert!(!self.buckets[bucket_index].as_bucket_mut().unwrap().is_used);
            }
            let result_bucket = self.buckets[bucket_index].as_bucket_mut().unwrap();
            result_bucket.is_used = true;
            result_bucket.address as u64 * WORD_SIZE_IN_BYTES
        };
        let result = result | HEAP_POINTER;
        if self.debug_heap_as_string {
            print_heap_as_string(&self.data);
        }
        result
    }

    pub fn read_flagged_word(&self, address: u64) -> FlaggedWord {
        let address = clear_address(address) as u64;
        #[cfg(debug_assertions)]
        assert!(
            self.find_bucket_from_address(address as _).is_used,
            "address = 0x{:x}",
            address
        );
        if address % WORD_SIZE_IN_BYTES == 0 {
            self.read_flagged_word_aligned((address / WORD_SIZE_IN_BYTES) as _)
        } else {
            todo!("address = {:x}", address)
        }
    }

    pub fn read_flagged_word_unchecked(&self, address: u64) -> FlaggedWord {
        let address = clear_address(address) as u64;
        if address % WORD_SIZE_IN_BYTES == 0 {
            self.read_flagged_word_aligned_unchecked((address / WORD_SIZE_IN_BYTES) as _)
        } else {
            todo!("address = {:x}", address)
        }
    }

    fn read_word_aligned(&self, address: u64) -> u64 {
        let address = clear_address(address);
        #[cfg(debug_assertions)]
        assert!(
            self.find_bucket_from_address(address * WORD_SIZE_IN_BYTES)
                .is_used
        );
        self.data[address as usize]
    }

    pub fn read_flagged_word_aligned(&self, address: u64) -> FlaggedWord {
        let address = clear_address(address);
        #[cfg(debug_assertions)]
        assert!(
            self.find_bucket_from_address(address * WORD_SIZE_IN_BYTES)
                .is_used
        );
        FlaggedWord::value(self.data[address as usize]).flags(self.flags[address as usize])
    }

    pub fn read_flagged_word_aligned_unchecked(&self, address: u64) -> FlaggedWord {
        let address = clear_address(address);
        FlaggedWord::value(self.data[address as usize]).flags(self.flags[address as usize])
    }

    pub fn write_byte(&mut self, address: u64, value: u8) {
        let address = clear_address(address);
        #[cfg(debug_assertions)]
        assert!(self.find_bucket_from_address(address).is_used);
        let word_address = address & !(WORD_SIZE_IN_BYTES - 1);
        let old_value = self.read_word_aligned(word_address / WORD_SIZE_IN_BYTES);
        let mut bytes = old_value.to_be_bytes();
        bytes[(address % WORD_SIZE_IN_BYTES) as usize] = value;
        let value = u64::from_be_bytes(bytes);
        self.write_word_aligned(word_address / WORD_SIZE_IN_BYTES, value)
    }

    pub fn write_word(&mut self, address: u64, value: u64) {
        let address = clear_address(address);

        #[cfg(debug_assertions)]
        assert!(self.find_bucket_from_address(address).is_used);
        if address % WORD_SIZE_IN_BYTES == 0 {
            self.write_word_aligned(address / WORD_SIZE_IN_BYTES, value)
        } else {
            todo!("address = {:x}", address)
        }
    }

    pub fn write_flagged_word(&mut self, address: u64, value: FlaggedWord) {
        let address = clear_address(address);

        #[cfg(debug_assertions)]
        assert!(self.find_bucket_from_address(address).is_used);
        if address % WORD_SIZE_IN_BYTES == 0 {
            self.write_flagged_word_aligned(address / WORD_SIZE_IN_BYTES, value)
        } else {
            todo!("address = {:x}", address)
        }
    }

    fn write_word_aligned(&mut self, address: u64, value: u64) {
        #[cfg(debug_assertions)]
        assert!(
            self.find_bucket_from_address(address * WORD_SIZE_IN_BYTES)
                .is_used,
            "bucket = {:#?}, address = 0x{:x}",
            self.find_bucket_from_address(address),
            address
        );
        self.data[address as usize] = value;
        self.flags[address as usize] = Flags::default();

        if self.debug_heap_as_string {
            print_heap_as_string(&self.data);
        }
    }

    fn write_flagged_word_aligned(&mut self, address: u64, value: FlaggedWord) {
        #[cfg(debug_assertions)]
        assert!(
            self.find_bucket_from_address(address * WORD_SIZE_IN_BYTES)
                .is_used,
            "bucket = {:#?}, address = 0x{:x}",
            self.find_bucket_from_address(address),
            address
        );
        self.data[address as usize] = value.value;
        self.flags[address as usize] = value.flags;

        if self.debug_heap_as_string {
            print_heap_as_string(&self.data);
        }
    }
}

pub fn print_heap_as_string(heap: &[u64]) {
    let mut string_buffer = Vec::with_capacity(heap.len() * WORD_SIZE_IN_BYTES as usize);
    for &word in heap {
        string_buffer.extend_from_slice(&word.to_be_bytes());
    }
    println!("heap = {:x?}", heap);
    println!("= '{}'", String::from_utf8_lossy(&string_buffer));
}

fn clear_address(address: u64) -> u64 {
    address & !HEAP_POINTER
}

#[derive(Debug, Clone, Copy)]
pub enum BucketEntry {
    Bucket(Bucket),
    Parent(BucketParent),
    Tombstone,
}

impl BucketEntry {
    pub fn root(size: u64) -> Self {
        Self::Bucket(Bucket::root(size))
    }

    pub fn buddy_index(&self) -> Option<usize> {
        match self {
            BucketEntry::Bucket(e) => e.buddy_index,
            BucketEntry::Parent(e) => e.buddy_index,
            BucketEntry::Tombstone => panic!("Unchecked Tombstone found!"),
        }
    }

    pub fn parent_index(&self) -> Option<usize> {
        match self {
            BucketEntry::Bucket(e) => e.parent_index,
            BucketEntry::Parent(e) => e.parent_index,
            BucketEntry::Tombstone => panic!("Unchecked Tombstone found!"),
        }
    }

    pub fn size_in_words(&self) -> u64 {
        match self {
            BucketEntry::Bucket(e) => e.size_in_words,
            BucketEntry::Parent(e) => e.size_in_words,
            BucketEntry::Tombstone => panic!("Unchecked Tombstone found!"),
        }
    }

    pub fn set_buddy_index(&mut self, buddy_index: Option<usize>) {
        match self {
            BucketEntry::Bucket(e) => e.buddy_index = buddy_index,
            BucketEntry::Parent(e) => e.buddy_index = buddy_index,
            BucketEntry::Tombstone => panic!("Unchecked Tombstone found!"),
        }
    }

    fn as_bucket(&self) -> Option<&Bucket> {
        if let Self::Bucket(v) = self {
            Some(v)
        } else {
            None
        }
    }

    fn as_bucket_mut(&mut self) -> Option<&mut Bucket> {
        if let Self::Bucket(v) = self {
            Some(v)
        } else {
            None
        }
    }

    fn as_parent(&self) -> Option<&BucketParent> {
        if let Self::Parent(v) = self {
            Some(v)
        } else {
            None
        }
    }

    fn as_parent_mut(&mut self) -> Option<&mut BucketParent> {
        if let Self::Parent(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BucketParent {
    pub index: usize,
    buddy_index: Option<usize>,
    parent_index: Option<usize>,
    address: u64,
    size_in_words: u64,
    depth: usize,
    pub child_indices: [usize; 2],
}

#[derive(Debug, Clone, Copy)]
pub struct Bucket {
    index: usize,
    buddy_index: Option<usize>,
    parent_index: Option<usize>,
    address: u64,
    size_in_words: u64,
    depth: usize,
    is_used: bool,
}

impl Bucket {
    pub fn root(size: u64) -> Self {
        Self {
            index: 0,
            buddy_index: None,
            parent_index: None,
            address: 0,
            size_in_words: size,
            depth: 0,
            is_used: false,
        }
    }

    pub fn buddy(buddy: &Self, index: usize, parent_index: usize) -> Self {
        let address = buddy.address + buddy.size_in_words / 2;
        Self {
            index,
            buddy_index: Some(buddy.index),
            parent_index: Some(parent_index),
            address,
            size_in_words: buddy.size_in_words / 2,
            depth: buddy.depth + 1,
            is_used: false,
        }
    }

    pub fn split(
        buddy: &mut Self,
        buddy_index: usize,
        parent_index: usize,
    ) -> (Bucket, BucketParent) {
        let result = Self::buddy(buddy, buddy_index, parent_index);
        let parent = BucketParent {
            index: parent_index,
            buddy_index: buddy.buddy_index,
            parent_index: buddy.parent_index,
            address: buddy.address,
            size_in_words: buddy.size_in_words,
            depth: buddy.depth,
            child_indices: [result.index, buddy.index],
        };

        buddy.buddy_index = Some(result.index);
        buddy.parent_index = Some(parent.index);
        buddy.size_in_words /= 2;
        buddy.depth += 1;

        (result, parent)
    }

    pub fn combine(parent_bucket: &BucketParent) -> Bucket {
        Self {
            index: parent_bucket.index,
            buddy_index: parent_bucket.buddy_index,
            parent_index: parent_bucket.parent_index,
            address: parent_bucket.address,
            size_in_words: parent_bucket.size_in_words,
            depth: parent_bucket.depth,
            is_used: false,
        }
    }
}
