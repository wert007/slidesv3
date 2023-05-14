use std::ops::Index;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct SourceTextId(usize);

impl SourceTextId {
    pub fn as_raw(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct SourceTextCollection {
    texts: Vec<SourceText>,
}

impl Default for SourceTextCollection {
    fn default() -> Self {
        Self {
            texts: Default::default(),
        }
    }
}

impl SourceTextCollection {
    pub fn add(&mut self, text: SourceText) -> SourceTextId {
        let index = self.texts.len();
        self.texts.push(text);
        SourceTextId(index)
    }
}

impl Index<SourceTextId> for SourceTextCollection {
    type Output = SourceText;

    fn index(&self, index: SourceTextId) -> &Self::Output {
        &self.texts[index.0]
    }
}

impl Index<TextLocation> for SourceTextCollection {
    type Output = str;

    fn index(&self, index: TextLocation) -> &Self::Output {
        &self[index.source_text].text[index.span.start..index.span.end()]
    }
}

#[derive(Debug, Clone)]
pub struct SourceText {
    pub text: String,
    pub file_name: String,
    lines: Vec<TextLine>,
}

impl SourceText {
    pub fn new(text: impl Into<String>, file_name: impl Into<String>) -> Self {
        let text = text.into();
        let file_name = file_name.into();
        let lines = parse_text_lines(&text);
        Self {
            text,
            file_name,
            lines,
        }
    }

    pub fn line_index(&self, position: usize) -> usize {
        let mut lower = 0;
        let mut upper = self.lines.len() - 1;
        while lower <= upper {
            let index = (lower + upper) / 2;
            let start = self.lines[index].start;
            match position.cmp(&start) {
                std::cmp::Ordering::Less => upper = index - 1,
                std::cmp::Ordering::Equal => return index,
                std::cmp::Ordering::Greater => lower = index + 1,
            }
        }
        lower - 1
    }

    pub fn column_index(&self, position: usize) -> usize {
        position - self.lines[self.line_index(position)].start
    }

    pub fn directory(&self) -> &str {
        std::path::Path::new(&self.file_name)
            .parent()
            .unwrap()
            .to_str()
            .unwrap()
    }
}

fn parse_text_lines(text: &str) -> Vec<TextLine> {
    let mut result = vec![];
    let mut line_start = 0;
    let mut line_break_width = 0;
    let mut last_char = '\0';
    for (byte_index, char) in text.char_indices() {
        match char {
            '\r' if last_char != '\r' && line_break_width < 2 => {
                line_break_width += 1;
            }
            '\n' if last_char != '\n' && line_break_width < 2 => {
                line_break_width += 1;
            }
            _ if line_break_width > 0 => {
                let length = byte_index - line_start - line_break_width;
                let length_including_line_break = length + line_break_width;
                let text_line = TextLine {
                    start: line_start,
                    length,
                    length_including_line_break,
                };
                result.push(text_line);
                line_break_width = if char == '\n' || char == '\r' { 1 } else { 0 };
                line_start = byte_index;
            }
            _ => {}
        }
        last_char = char;
    }
    let length = text.len() - line_start - line_break_width;
    let length_including_line_break = length + line_break_width;
    result.push(TextLine {
        start: line_start,
        length,
        length_including_line_break,
    });
    result
}

#[derive(Clone, Copy)]
pub struct TextLine {
    start: usize,
    length: usize,
    length_including_line_break: usize,
}

impl std::fmt::Debug for TextLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TextLine")
            .field("start", &self.start)
            .field("length", &self.length)
            .field(
                "length_including_line_break",
                &self.length_including_line_break,
            )
            .finish()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextLocation {
    pub span: TextSpan,
    pub source_text: SourceTextId,
}
impl TextLocation {
    /// Panics if start and end are from different source files.
    pub(crate) fn bounds(start: TextLocation, end: TextLocation) -> TextLocation {
        assert_eq!(start.source_text, end.source_text);
        Self {
            span: TextSpan::bounds(start.span, end.span),
            source_text: start.source_text,
        }
    }

    pub(crate) fn zero_in_file(source_text: SourceTextId) -> TextLocation {
        Self {
            span: TextSpan::zero(),
            source_text,
        }
    }

    pub fn display(self, source_text_collection: &SourceTextCollection) -> String {
        let source_text = &source_text_collection[self.source_text];
        if source_text.file_name.is_empty() {
            format!("at {}-{}", self.span.start(), self.span.end())
        } else {
            let start_line = source_text.line_index(self.span.start()) + 1;
            let start_column = source_text.column_index(self.span.start()) + 1;
            // let end_line = source_text.line_index(self.span.end()) + 1;
            // let end_column = source_text.column_index(self.span.end()) + 1;
            format!(
                "in {}:{}:{}",
                source_text.file_name, start_line, start_column
            )
        }
    }
}

// impl std::fmt::Display for TextLocation {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//     }
// }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextSpan {
    start: usize,
    length: usize,
    is_foreign: bool,
}

#[allow(dead_code)]
impl TextSpan {
    pub fn zero() -> Self {
        Self {
            start: 0,
            length: 0,
            is_foreign: true,
        }
    }

    pub fn bounds(start: Self, end: Self) -> Self {
        let is_foreign = start.is_foreign || end.is_foreign;
        Self {
            start: start.start,
            length: end.end() - start.start,
            is_foreign,
        }
    }

    pub fn new(start: usize, length: usize, is_foreign: bool) -> Self {
        Self {
            start,
            length,
            is_foreign,
        }
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

    pub fn is_foreign(&self) -> bool {
        self.is_foreign
    }

    pub fn set_is_foreign(&mut self, is_foreign: bool) {
        self.is_foreign = is_foreign;
    }
}
