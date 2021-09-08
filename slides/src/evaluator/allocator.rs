use super::HEAP_POINTER;

pub struct Allocator {
    pub data: Vec<u64>,
    buckets: Vec<BucketEntry>,
}

impl Allocator {
    pub fn new(size: usize) -> Self {
        assert_eq!(size % 4, 0);
        let size = size / 4;
        assert!(size.is_power_of_two());
        Self {
            data: vec![0; size],
            buckets: vec![BucketEntry::root(size)],
        }
    }

    fn free_buckets(&mut self) -> Vec<&mut Bucket> {
        let mut result: Vec<_> = self
            .buckets
            .iter_mut()
            .filter_map(|b| match b {
                BucketEntry::Bucket(b) if !b.is_used => Some(b),
                _ => None,
            })
            .collect();
        result.sort_by_key(|b| b.size);
        result
    }

    pub fn allocate(&mut self, size: u64) -> u64 {
        let result = {
            let mut bucket_index = {
                let mut free_buckets = self.free_buckets();
                if free_buckets.is_empty() {
                    // eprintln!("No Memory left!!!!");
                    return 0;
                }
                free_buckets.remove(0).index
            };
            let size = (size + 3) / 4;
            let expected_size = size.next_power_of_two() as usize;
            let expected_size = expected_size.max(4);
            while self.buckets[bucket_index].size() > expected_size {
                let old_buddy_index = self.buckets[bucket_index]
                .buddy_index().clone(); 
                let new_index = self.buckets.len();
                let (tmp_bucket, parent) = Bucket::split(
                    self.buckets[bucket_index].as_bucket_mut().unwrap(),
                    new_index,
                );
                
                if let Some(old_buddy) = old_buddy_index {
                    self.buckets[old_buddy].set_buddy_index(Some(parent.index));
                }
                let index = tmp_bucket.index;
                self.buckets.push(BucketEntry::Bucket(tmp_bucket));
                self.buckets.push(BucketEntry::Parent(parent));
                bucket_index = self.buckets[index].as_bucket_mut().unwrap().index;
                assert!(!self.buckets[bucket_index].as_bucket_mut().unwrap().is_used);
            }
            let result_bucket = self.buckets[bucket_index].as_bucket_mut().unwrap();
            result_bucket.is_used = true;
            result_bucket.address as u64 * 4
        };
        let result = result | HEAP_POINTER;
        result
    }

    pub fn read_byte(&self, address: usize) -> u8 {
        let address = clear_address(address);
        let word_address = address & !3;
        let word = self.read_word_aligned(word_address / 4);
        let word = match address % 4 {
            0 => word,
            1 => word >> 8,
            2 => word >> 16,
            3 => word >> 24,
            _ => unreachable!(),
        };
        (word & 0xFF) as u8
    }

    pub fn read_word(&self, address: usize) -> u64 {
        let address = clear_address(address);
        if address % 4 == 0 {
            self.read_word_aligned(address / 4)
        } else {
            todo!("address = {:x}", address)
        }
    }

    pub fn read_word_aligned(&self, address: usize) -> u64 {
        let address = clear_address(address);
        // println!("address = 0x{:x}", address);
        self.data[address]
    }

    pub fn write_byte(&mut self, address: usize, value: u8) {
        // println!("write_byte::value : u8 = 0x{:x}", value);
        let address = clear_address(address);
        let word_address = address & !3;
        let old_value = self.read_word_aligned(word_address / 4);
        let value = value as u64;
        let value = match address % 4 {
            0 => value,
            1 => value << 8,
            2 => value << 16,
            3 => value << 24,
            _ => unreachable!(),
        };
        let clear_value: u64 = !match address % 4 {
            0 => 0xFF,
            1 => 0xFF << 8,
            2 => 0xFF << 16,
            3 => 0xFF << 24,
            _ => unreachable!(),
        };
        let value = (old_value & clear_value) | value;
        self.write_word_aligned(word_address / 4, value)
    }

    pub fn write_word(&mut self, address: usize, value: u64) {
        let address = clear_address(address);
        if address % 4 == 0 {
            self.write_word_aligned(address / 4, value)
        } else {
            todo!("address = {:x}", address)
        }
    }

    fn write_word_aligned(&mut self, address: usize, value: u64) {
        // println!("data[0x{:x}] = 0x{:x}", address, value);
        self.data[address] = value;
    }
}

fn clear_address(address: usize) -> usize {
    let address = address as u64;
    let address = address & !HEAP_POINTER;
    address as usize
}

#[derive(Debug)]
enum BucketEntry {
    Bucket(Bucket),
    Parent(BucketParent),
}

impl BucketEntry {
    pub fn root(size: usize) -> Self {
        Self::Bucket(Bucket::root(size))
    }

    pub fn buddy_index(&self) -> Option<usize> {
        match self {
            BucketEntry::Bucket(e) => e.buddy_index,
            BucketEntry::Parent(e) => e.buddy_index,
        }
    }

    pub fn size(&self) -> usize {
        match self {
            BucketEntry::Bucket(e) => e.size,
            BucketEntry::Parent(e) => e.size,
        }
    }

    pub fn set_buddy_index(&mut self, buddy_index: Option<usize>) {
        match self {
            BucketEntry::Bucket(e) => e.buddy_index = buddy_index,
            BucketEntry::Parent(e) => e.buddy_index = buddy_index,
        }
    }

    fn as_bucket_mut(&mut self) -> Option<&mut Bucket> {
        if let Self::Bucket(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug)]
struct BucketParent {
    index: usize,
    buddy_index: Option<usize>,
    parent_index: Option<usize>,
    address: usize,
    size: usize,
    depth: usize,
    child_indices: [usize; 2],
}

#[derive(Debug)]
struct Bucket {
    index: usize,
    buddy_index: Option<usize>,
    parent_index: Option<usize>,
    address: usize,
    size: usize,
    depth: usize,
    is_used: bool,
}

impl Bucket {
    pub fn root(size: usize) -> Self {
        Self {
            index: 0,
            buddy_index: None,
            parent_index: None,
            address: 0,
            size,
            depth: 0,
            is_used: false,
        }
    }

    pub fn buddy(buddy: &Self, index: usize, parent_index: usize) -> Self {
        let address = buddy.address + buddy.size / 2;
        // println!(
        //     "creating buddy at index {} with buddy {}",
        //     index, buddy.index
        // );
        Self {
            index,
            buddy_index: Some(buddy.index),
            parent_index: Some(parent_index),
            address,
            size: buddy.size / 2,
            depth: buddy.depth + 1,
            is_used: false,
        }
    }

    pub fn split(
        buddy: &mut Self,
        new_index: usize,
    ) -> (Bucket, BucketParent) {
        // println!("splitting now, new_index = {}", new_index);
        let result = Self::buddy(buddy, new_index, new_index + 1);
        let parent = BucketParent {
            index: new_index + 1,
            buddy_index: buddy.buddy_index,
            parent_index: buddy.parent_index,
            address: buddy.address,
            size: buddy.size,
            depth: buddy.depth,
            child_indices: [result.index, buddy.index],
        };
        // println!(
        //     "creating parent at index {} with buddy {:?}",
        //     parent.index, parent.buddy_index
        // );
        buddy.buddy_index = Some(result.index);
        buddy.parent_index = Some(parent.index);
        buddy.size /= 2;
        buddy.depth += 1;
        // println!(
        //     "assigning buddy for index {} with buddy {}",
        //     buddy.index, result.index
        // );

        (result, parent)
    }
}
