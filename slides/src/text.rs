#[derive(Debug, Clone)]
pub struct SourceText<'a> {
    pub text: &'a str,
    pub file_name: &'a str,
    lines: Vec<TextLine<'a>>,
}

impl<'a> SourceText<'a> {
    pub fn new(text: &'a str, file_name: &'a str) -> Self {
        let lines = parse_text_lines(text);
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
        std::path::Path::new(self.file_name).parent().unwrap().to_str().unwrap()
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
                    text,
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
        text,
        start: line_start,
        length,
        length_including_line_break,
    });
    result
}

#[derive(Clone, Copy)]
pub struct TextLine<'a> {
    text: &'a str,
    start: usize,
    length: usize,
    length_including_line_break: usize,
}

impl<'a> TextLine<'a> {
    pub fn end(&self) -> usize {
        self.start + self.length
    }

    pub fn line_text(&self) -> &'a str {
        &self.text[self.start..self.end()]
    }
}

impl std::fmt::Debug for TextLine<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TextLine")
            .field("text", &self.line_text())
            .field("start", &self.start)
            .field("length", &self.length)
            .field(
                "length_including_line_break",
                &self.length_including_line_break,
            )
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct TextLocation<'a> {
    pub span: TextSpan,
    pub source_text: &'a SourceText<'a>,
}

impl std::fmt::Display for TextLocation<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.source_text.file_name.is_empty() {
            write!(f, "at {}-{}", self.span.start(), self.span.end())
        } else {
            let start_line = self.source_text.line_index(self.span.start()) + 1;
            let start_column = self.source_text.column_index(self.span.start()) + 1;
            // let end_line = self.source_text.line_index(self.span.end()) + 1;
            // let end_column = self.source_text.column_index(self.span.end()) + 1;
            write!(
                f,
                "in {}:{}:{}",
                self.source_text.file_name, start_line, start_column
            )
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextSpan {
    start: usize,
    length: usize,
}

#[allow(dead_code)]
impl TextSpan {
    pub fn zero() -> Self {
        Self {
            start: 0,
            length: 0,
        }
    }

    pub fn bounds(start: Self, end: Self) -> Self {
        Self {
            start: start.start,
            length: end.end() - start.start,
        }
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
