// MIT License
//
// Copyright (c) 2025 Leonard Schütz
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#[derive(Debug, Clone)]
pub struct TextPosition {
    pub byte_offset: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone)]
pub struct TextSpan {
    /// inclusive
    pub start: TextPosition,

    /// exclusive
    pub end: TextPosition,
}

pub struct WindowBuffer {
    buffer: String,
    pub window_span: TextSpan,
    pub last_read_char: Option<char>,
}

impl From<&str> for WindowBuffer {
    fn from(value: &str) -> Self {
        let mut buffer = Self::with_capacity(value.len());
        buffer.push_str(value);
        buffer
    }
}

impl WindowBuffer {
    pub fn with_capacity(initial_capacity: usize) -> Self {
        Self {
            buffer: String::with_capacity(initial_capacity),
            window_span: TextSpan {
                start: TextPosition { byte_offset: 0, line: 1, column: 1 },
                end: TextPosition { byte_offset: 0, line: 1, column: 1 },
            },
            last_read_char: None,
        }
    }

    pub fn push_str(&mut self, value: &str) {
        self.buffer.push_str(value);
    }

    pub fn read_char(&mut self) -> Option<char> {
        let char = self.peek_char();

        if let Some(char) = char {
            let char_len = char.len_utf8();

            self.increment_offset(char_len);
            match char {
                '\n' => self.increment_row(),
                _ => self.increment_column(),
            }

            self.last_read_char = Some(char);
        }

        char
    }

    pub fn advance(&mut self, length: usize) {
        for _ in 0.. length {
            self.read_char();
        }
    }

    pub fn peek_char(&self) -> Option<char> {
        self.peek_char_with_lookahead(0)
    }

    pub fn peek_char_with_lookahead(&self, lookahead: usize) -> Option<char> {
        let rest_start = self.window_span.end.byte_offset;
        let rest_end = self.buffer.len();
        let rest_slice = &self.buffer[rest_start..rest_end];

        let mut chars = rest_slice.chars();
        let mut peek_char: Option<char> = None;

        for _ in 0..=lookahead {
            peek_char = chars.next();
        }

        peek_char
    }

    pub fn peek_str(&self, length: usize) -> Option<&str> {
        let rest_start = self.window_span.end.byte_offset;
        let rest_end = self.buffer.len();
        let rest_slice = &self.buffer[rest_start..rest_end];

        let mut chars = rest_slice.chars();
        let mut byte_length = 0;

        for _ in 0..length {
            if let Some(char) = chars.next() {
                byte_length += char.len_utf8();
            } else {
                return None;
            }
        }

        let end = rest_start + byte_length;
        let slice = &self.buffer[rest_start..end];
        Some(slice)
    }

    fn increment_row(&mut self) {
        self.window_span.end.line += 1;
        self.window_span.end.column = 1;
    }

    fn increment_column(&mut self) {
        self.window_span.end.column += 1;
    }

    fn increment_offset(&mut self, length: usize) {
        self.window_span.end.byte_offset += length;
    }

    pub fn window_as_str(&self) -> &str {
        let start = self.window_span.start.byte_offset;
        let end = self.window_span.end.byte_offset;
        let slice = &self.buffer[start..end];
        slice
    }

    pub fn reset_window(&mut self) {
        self.window_span.start = self.window_span.end.clone();
    }
}
