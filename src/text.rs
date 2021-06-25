#[derive(Debug, Clone, Copy)]
pub struct TextSpan {
    start: usize,
    length: usize,
}

#[allow(dead_code)]
impl TextSpan {
    pub fn bounds(start: Self, end: Self) -> Self {
        Self {start: start.start, length: end.end() - start.start}
    }

    pub fn new(start: usize, length: usize) -> Self {
        Self { start, length }
    }

    /// Get a reference to the text span's start.
    pub fn start(&self) -> usize {
        self.start
    }

    /// Get a reference to the text span's length.
    pub fn length(&self) -> usize {
        self.length
    }

    /// Get a reference to the text span's end.
    pub fn end(&self) -> usize {
        self.start + self.length
    }
}
