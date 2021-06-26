#[derive(Debug, Clone)]
pub struct SourceText<'a, 'b> {
    text: &'a str,
    pub file_name: &'b str,
    lines: Vec<TextLine<'a>>,
}

impl<'a, 'b> SourceText<'a, 'b> {
    pub fn new(text: &'a str, file_name: &'b str) -> Self {
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
}

fn parse_text_lines<'a>(text: &'a str) -> Vec<TextLine<'a>> {
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
            .field("length_including_line_break", &self.length_including_line_break)
            .finish()
    }
}


#[derive(Debug, Clone)]
pub struct TextLocation<'a> {
    pub span: TextSpan,
    pub file_name: &'a str
}


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
