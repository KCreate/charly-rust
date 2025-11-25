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

use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct TextPosition {
    /// character-based offset
    pub offset: usize,
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone, Default)]
pub struct TextSpan {
    pub start: TextPosition,
    pub end: TextPosition,
}

impl Display for TextPosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}:{}>", self.line, self.column)
    }
}

impl Display for TextSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.start, self.end)
    }
}

pub struct WindowBuffer<'a> {
    buffer: &'a str,
    pub window_span: TextSpan,
    pub last_read_char: Option<char>,
}

impl<'a> WindowBuffer<'a> {
    pub fn new(value: &'a str) -> Self {
        Self {
            buffer: value,
            window_span: TextSpan {
                start: TextPosition {
                    offset: 0,
                    line: 1,
                    column: 1,
                },
                end: TextPosition {
                    offset: 0,
                    line: 1,
                    column: 1,
                },
            },
            last_read_char: None,
        }
    }

    pub fn read_char(&mut self) -> Option<char> {
        match self.peek_char() {
            Some(char) => {
                self.increment_offset(char);
                match char {
                    '\n' => self.increment_row(),
                    _ => self.increment_column(),
                }
                self.last_read_char = Some(char);
                Some(char)
            }

            _ => None,
        }
    }

    pub fn advance(&mut self, length: usize) {
        for _ in 0..length {
            self.read_char();
        }
    }

    pub fn eat_char(&mut self, char: char) -> bool {
        if self.peek_char() == Some(char) {
            self.advance(1);
            true
        } else {
            false
        }
    }

    pub fn peek_char(&self) -> Option<char> {
        self.peek_char_with_lookahead(0)
    }

    pub fn peek_char_with_lookahead(&self, lookahead: usize) -> Option<char> {
        let rest_start = self.window_span.end.offset;
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
        let rest_start = self.window_span.end.offset;
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

    fn increment_offset(&mut self, char: char) {
        self.window_span.end.offset += char.len_utf8();
    }

    pub fn window_as_str(&self) -> &str {
        let start = self.window_span.start.offset;
        let end = self.window_span.end.offset;
        let slice = &self.buffer[start..end];
        slice
    }

    pub fn reset_window(&mut self) {
        self.window_span.start = self.window_span.end.clone();
    }

    pub fn cursor_position(&self) -> TextPosition {
        self.window_span.end.clone()
    }
}
