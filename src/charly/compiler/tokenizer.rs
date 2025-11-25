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

use crate::charly::compiler::token::{
    IntegerBaseSpecifier, NumberSuffix, Token, TokenKind, TokenValue, TOKEN_MAX_STR_LEN,
};
use crate::charly::utils::diagnostics::{DiagnosticContext, DiagnosticLocation, FileId};
use crate::charly::utils::window_buffer::{TextPosition, TextSpan, WindowBuffer};

/// The Tokenizer supports parsing interpolated expressions and identifiers within
/// string literals.
///
/// ```text
/// TopLevel -> Interpolated
/// Interpolated -> String
/// String -> Interpolated
/// String -> TopLevel
/// ```
#[derive(PartialEq)]
pub enum TokenizerMode {
    TopLevel, // top level parsing, not inside any interpolated expression
    String,   // finished parsing interpolated expression, continue parsing string
    InterpolatedExpression, // currently parsing interpolated expression
    InterpolatedIdentifier, // currently parsing interpolated identifier
}

pub struct Tokenizer<'a> {
    buffer: WindowBuffer<'a>,
    file_id: FileId,
    mode: TokenizerMode,
    diagnostic_context: &'a mut DiagnosticContext,

    /// contains opened paren, brace and bracket tokens
    bracket_stack: Vec<TokenKind>,

    /// contains the size of the bracket_stack the last
    /// time a string interpolation was started
    interpolation_stack: Vec<usize>,

    /// keeps track of opened string literals
    /// stacks grows if a string literal is opened recursively within
    /// a string interpolation
    opened_strings_stack: Vec<TextPosition>,
}

impl Iterator for Tokenizer<'_> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_impl()
    }
}

impl<'a> Tokenizer<'a> {
    pub fn new(
        source: &'a str,
        file_id: FileId,
        context: &'a mut DiagnosticContext,
    ) -> Self {
        let mut lexer = Self {
            buffer: WindowBuffer::new(source),
            file_id,
            mode: TokenizerMode::TopLevel,
            diagnostic_context: context,
            bracket_stack: Vec::new(),
            interpolation_stack: Vec::new(),
            opened_strings_stack: Vec::new(),
        };
        lexer.consume_shebang();
        lexer
    }

    fn next_impl(&mut self) -> Option<Token> {
        self.buffer.reset_window();

        let char = self.buffer.peek_char()?;

        match self.mode {
            TokenizerMode::String => return Some(self.consume_string(false)),
            TokenizerMode::InterpolatedIdentifier => {
                let id = self.consume_identifier();
                self.mode = TokenizerMode::String;
                return Some(id);
            }
            _ => {}
        }

        let token = match char {
            c if c.is_whitespace() => Some(self.consume_whitespace()),
            c if c.is_identifier_begin() => Some(self.consume_identifier()),
            c if c.is_decimal_digit() => Some(self.consume_number()),

            // string literals
            '\"' => {
                self.opened_strings_stack
                    .push(self.buffer.window_span.start);
                Some(self.consume_string(true))
            }

            // comments
            '/' => match self.buffer.peek_char_with_lookahead(1) {
                Some('/') => Some(self.consume_comment()),
                Some('*') => Some(self.consume_multiline_comment()),
                _ => None,
            },

            _ => None,
        };

        if token.is_some() {
            return token;
        }

        // punctuation tokens
        for n in (1..=TOKEN_MAX_STR_LEN).rev() {
            let Some(lookahead) = self.buffer.peek_str(n) else {
                continue;
            };

            let Some(kind) = TokenKind::try_from_str(lookahead) else {
                continue;
            };

            self.buffer.advance(n);

            // brace matching
            if kind.is_opening_bracket() {
                self.bracket_stack.push(kind);
            } else if kind.is_closing_bracket() {
                let expected_match = kind.matching_bracket().unwrap();
                let Some(actual_match) = self.bracket_stack.last() else {
                    continue;
                };

                if *actual_match == expected_match {
                    self.bracket_stack.pop();

                    if self.mode == TokenizerMode::InterpolatedExpression {
                        if let Some(top) = self.interpolation_stack.last() {
                            if *top == self.bracket_stack.len() {
                                self.interpolation_stack.pop();
                                self.mode = TokenizerMode::String;
                                return Some(self.consume_string(false));
                            }
                        }
                    }
                }
            }

            return Some(self.build_token(kind));
        }

        // encountered an unexpected character
        self.buffer.advance(1);
        self.emit_error_unexpected_char(char);
        Some(self.build_token(TokenKind::Error))
    }

    fn consume_shebang(&mut self) {
        if self.buffer.peek_str(2) == Some("#!") {
            self.consume_until_newline();
            self.buffer.reset_window();
        }
    }

    fn consume_whitespace(&mut self) -> Token {
        let char = self.buffer.peek_char().unwrap();
        assert!(char.is_whitespace());

        loop {
            match self.buffer.peek_char() {
                Some('\r') | Some('\n') => return self.consume_newline(),
                Some(c) if c.is_whitespace() => self.buffer.advance(1),
                _ => break,
            }
        }

        self.build_token(TokenKind::Whitespace)
    }

    fn consume_newline(&mut self) -> Token {
        let char = self.buffer.read_char().unwrap();
        assert!(char == '\n' || char == '\r');

        match char {
            // newlines
            '\n' => self.build_token(TokenKind::Newline),
            '\r' => {
                if self.buffer.eat_char('\n') {
                    self.build_token(TokenKind::Newline)
                } else {
                    self.build_token(TokenKind::Whitespace)
                }
            }
            _ => unreachable!(),
        }
    }

    fn consume_string(&mut self, skip_initial: bool) -> Token {
        if skip_initial {
            self.buffer.advance(1);
        }

        let mut content = String::new();

        loop {
            let start_pos = self.buffer.cursor_position();
            match self.buffer.read_char() {
                Some(c) if c == '\"' => {
                    self.opened_strings_stack.pop();
                    break;
                }
                Some(c) if c == '\\' => match self.buffer.read_char() {
                    Some('n') => content.push('\n'),
                    Some('r') => content.push('\r'),
                    Some('t') => content.push('\t'),
                    Some('"') => content.push('"'),
                    Some('\\') => content.push('\\'),
                    Some(c) => {
                        let end_pos = self.buffer.cursor_position();
                        let location = self.build_location(&start_pos, &end_pos);
                        self.emit_warning_unnecessary_escape_sequence(location);
                        content.push(c);
                    }
                    None => {
                        self.emit_error_unclosed_string();
                        return self.build_token(TokenKind::Error);
                    }
                },
                Some(c) if c == '$' => match self.buffer.peek_char() {
                    Some('{') => {
                        self.buffer.advance(1);
                        self.mode = TokenizerMode::InterpolatedExpression;
                        self.interpolation_stack.push(self.bracket_stack.len());
                        self.bracket_stack.push(TokenKind::LeftBrace);
                        return self.build_token_with_value(
                            TokenKind::FormatStringPart,
                            TokenValue::TextValue(content),
                        );
                    }
                    Some(char) if char.is_identifier_begin() => {
                        self.mode = TokenizerMode::InterpolatedIdentifier;
                        return self.build_token_with_value(
                            TokenKind::FormatStringPart,
                            TokenValue::TextValue(content),
                        );
                    }
                    Some(_) => content.push(c),
                    None => {
                        self.emit_error_unclosed_string();
                        return self.build_token(TokenKind::Error);
                    }
                },
                Some(c) => content.push(c),
                None => {
                    self.emit_error_unclosed_string();
                    return self.build_token(TokenKind::Error);
                }
            }
        }

        if self.interpolation_stack.is_empty() {
            self.mode = TokenizerMode::TopLevel;
        } else {
            self.mode = TokenizerMode::InterpolatedExpression;
        }

        self.build_token_with_value(TokenKind::String, TokenValue::TextValue(content))
    }

    fn consume_number(&mut self) -> Token {
        use IntegerBaseSpecifier::*;

        let char = self.buffer.peek_char().unwrap();
        assert!(char.is_decimal_digit());

        match self.buffer.peek_str(2) {
            Some("0x") => self.consume_number_with_base(Hexadecimal),
            Some("0o") => self.consume_number_with_base(Octal),
            Some("0b") => self.consume_number_with_base(Binary),
            _ => self.consume_number_with_base(Decimal),
        }
    }

    fn consume_number_with_base(&mut self, base: IntegerBaseSpecifier) -> Token {
        use IntegerBaseSpecifier::*;

        // skip the base specifier if present
        match base {
            Hexadecimal => self.buffer.advance(2),
            Decimal => {}
            Octal => self.buffer.advance(2),
            Binary => self.buffer.advance(2),
        }

        match base {
            Hexadecimal => self.consume_integer_with_base(base),
            Decimal => self.consume_decimal(),
            Octal => self.consume_integer_with_base(base),
            Binary => self.consume_integer_with_base(base),
        }
    }

    fn consume_integer_with_base(&mut self, base: IntegerBaseSpecifier) -> Token {
        // consume numeric digits
        let mut digit_buf = String::new();
        self.consume_digits_of_base(&mut digit_buf, &base);

        // consume suffix
        let suffix = self.consume_numeric_suffix();

        self.build_token_with_value(
            TokenKind::Integer,
            TokenValue::IntegerValue(digit_buf, base, suffix),
        )
    }

    /// Decimal = Digit+ ("." Digit+) Suffix?
    fn consume_decimal(&mut self) -> Token {
        // consume Digit+
        let mut digit_buf = String::new();
        self.consume_digits_of_base(&mut digit_buf, &IntegerBaseSpecifier::Decimal);

        // consume ("." Digit+)
        let mut is_float = false;
        if self.buffer.peek_char() == Some('.') {
            if let Some(char) = self.buffer.peek_char_with_lookahead(1) {
                if char.is_digit_of_base(&IntegerBaseSpecifier::Decimal) {
                    is_float = true;
                    digit_buf.push('.');
                    self.buffer.advance(1);
                    self.consume_digits_of_base(
                        &mut digit_buf,
                        &IntegerBaseSpecifier::Decimal,
                    );
                }
            }
        }

        // consume Suffix?
        let suffix = self.consume_numeric_suffix();

        match is_float {
            true => self.build_token_with_value(
                TokenKind::Float,
                TokenValue::FloatValue(digit_buf, suffix),
            ),
            false => self.build_token_with_value(
                TokenKind::Integer,
                TokenValue::IntegerValue(
                    digit_buf,
                    IntegerBaseSpecifier::Decimal,
                    suffix,
                ),
            ),
        }
    }

    fn consume_digits_of_base(&mut self, buf: &mut String, base: &IntegerBaseSpecifier) {
        while let Some(char) = self.buffer.peek_char() {
            if char.is_digit_of_base(base) {
                self.buffer.advance(1);
                buf.push(char);
            } else if char == '_' {
                self.buffer.advance(1);
            } else {
                break;
            }
        }
    }

    fn consume_numeric_suffix(&mut self) -> Option<NumberSuffix> {
        if let Some(char) = self.buffer.peek_char() {
            if char.is_numeric_suffix_part() {
                let mut suffix_buf = String::new();

                while let Some(char) = self.buffer.peek_char() {
                    if !char.is_numeric_suffix_part() {
                        break;
                    }

                    self.buffer.advance(1);
                    suffix_buf.push(char);
                }

                return Some(suffix_buf);
            }
        }

        None
    }

    fn consume_identifier(&mut self) -> Token {
        let char = self.buffer.read_char().unwrap();
        assert!(char.is_identifier_begin());

        while let Some(c) = self.buffer.peek_char() {
            if !c.is_identifier_part() {
                break;
            }
            self.buffer.advance(1);
        }

        let window_text = self.buffer.window_as_str();
        match TokenKind::try_from_str(window_text) {
            Some(kind) => self.build_token(kind),
            None => self.build_token_with_value(
                TokenKind::Identifier,
                TokenValue::TextValue(window_text.to_string()),
            ),
        }
    }

    fn consume_comment(&mut self) -> Token {
        self.buffer.advance(2);

        let kind = match self.buffer.eat_char('/') {
            true => TokenKind::DocComment,
            false => TokenKind::SingleLineComment,
        };

        self.consume_until_newline();
        self.build_token(kind)
    }

    fn consume_multiline_comment(&mut self) -> Token {
        self.buffer.advance(2);

        loop {
            match self.buffer.peek_str(2) {
                Some("*/") => {
                    self.buffer.advance(2);
                    break;
                }
                Some(_) => self.buffer.advance(1),
                None => break,
            }
        }

        self.build_token(TokenKind::MultiLineComment)
    }

    fn consume_until_newline(&mut self) {
        while let Some(c) = self.buffer.peek_char() {
            if c == '\n' || c == '\r' {
                break;
            }

            self.buffer.advance(1);
        }
    }

    fn emit_warning_unnecessary_escape_sequence(&mut self, location: DiagnosticLocation) {
        self.diagnostic_context.warning(
            "Unnecessary escape sequence",
            &location,
            vec![(None, location.clone())],
            vec![],
        );
    }

    fn emit_error_unexpected_char(&mut self, char: char) {
        self.diagnostic_context.error(
            format!("Unexpected '{}' character", char).as_str(),
            &self.window_location(),
            vec![],
            vec![],
        );
    }

    fn emit_error_unclosed_string(&mut self) {
        let open_quote_position = match self.opened_strings_stack.last() {
            Some(position) => position,
            None => &self.window_location().span.start,
        };

        let mut range_end = open_quote_position.clone();
        range_end.offset += 1;
        range_end.column += 1;

        let label_location = self.build_location(&open_quote_position, &range_end);

        self.diagnostic_context.error(
            "Unclosed string literal",
            &label_location,
            vec![(Some("Opened here"), label_location.clone())],
            vec![],
        );
    }

    fn build_location(
        &self,
        start: &TextPosition,
        end: &TextPosition,
    ) -> DiagnosticLocation {
        DiagnosticLocation {
            file_id: self.file_id,
            span: TextSpan {
                start: start.clone(),
                end: end.clone(),
            },
        }
    }

    fn window_location(&self) -> DiagnosticLocation {
        let start = self.buffer.window_span.start;
        let end = self.buffer.window_span.end;
        self.build_location(&start, &end)
    }

    fn build_token(&mut self, kind: TokenKind) -> Token {
        Token {
            kind,
            location: self.window_location(),
            raw: self.buffer.window_as_str().to_string(),
            value: None,
        }
    }

    fn build_token_with_value(&mut self, kind: TokenKind, value: TokenValue) -> Token {
        Token {
            kind,
            location: self.window_location(),
            raw: self.buffer.window_as_str().to_string(),
            value: Some(value),
        }
    }
}

trait CharType {
    fn is_decimal_digit(&self) -> bool;
    fn is_identifier_begin(&self) -> bool;
    fn is_identifier_part(&self) -> bool;
    fn is_numeric_suffix_part(&self) -> bool;
    fn is_digit_of_base(&self, base: &IntegerBaseSpecifier) -> bool;
}

impl CharType for char {
    fn is_decimal_digit(&self) -> bool {
        self.is_digit(10)
    }

    fn is_identifier_begin(&self) -> bool {
        self.is_ascii_alphabetic() || *self == '$' || *self == '_'
    }

    fn is_identifier_part(&self) -> bool {
        self.is_identifier_begin() || self.is_decimal_digit()
    }

    fn is_numeric_suffix_part(&self) -> bool {
        self.is_ascii_alphanumeric()
    }

    fn is_digit_of_base(&self, base: &IntegerBaseSpecifier) -> bool {
        use IntegerBaseSpecifier::*;

        match base {
            Hexadecimal => self.is_ascii_hexdigit(),
            Decimal => self.is_ascii_digit(),
            Octal => match self {
                '0'..='7' => true,
                _ => false,
            },
            Binary => match self {
                '0'..='1' => true,
                _ => false,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::charly::compiler::token::{Token, TOKEN_KEYWORDS, TOKEN_PUNCTUATORS};
    use crate::charly::test_utils::validate_expected_diagnostics;
    use crate::charly::utils::diagnostics::DiagnosticController;
    use pretty_assertions::assert_eq;
    use std::path::PathBuf;

    enum TokenStreamDetailLevel {
        Full,
        NoWhitespace,
        NoWhitespaceAndComments,
    }

    #[track_caller]
    fn assert_tokens(
        source: &str,
        token_stream_detail_level: TokenStreamDetailLevel,
        expected_tokens: &[&str],
        expected_diagnostics: &[&str],
    ) {
        let mut controller = DiagnosticController::new();
        let path = PathBuf::from("test");
        let file_id = controller.register_file(&path, source);
        let mut context = controller.get_or_create_context(file_id);

        let tokens: Vec<Token> = Tokenizer::new(source, file_id, &mut context)
            .filter(|token| match token_stream_detail_level {
                TokenStreamDetailLevel::Full => true,
                TokenStreamDetailLevel::NoWhitespace => match token.kind {
                    TokenKind::Whitespace => false,
                    TokenKind::Newline => false,
                    _ => true,
                },
                TokenStreamDetailLevel::NoWhitespaceAndComments => match token.kind {
                    TokenKind::Whitespace => false,
                    TokenKind::Newline => false,
                    TokenKind::SingleLineComment => false,
                    TokenKind::MultiLineComment => false,
                    _ => true,
                },
            })
            .collect();

        let mut iter = tokens.iter().map(|t| format!("{}", t));

        for (index, expected) in expected_tokens.iter().enumerate() {
            assert_eq!(iter.next().unwrap(), *expected, "at index {}", index);
        }

        if let Some(token) = iter.next() {
            panic!("unexpected token {}", token);
        }

        validate_expected_diagnostics(&context, expected_diagnostics);
    }

    #[test]
    fn test_empty_program() {
        let source = r#""#;
        assert_tokens(source, TokenStreamDetailLevel::Full, &[], &[]);
    }

    #[test]
    fn test_skip_shebang() {
        let source = r#"#!/bin/bash
            hello world"#;
        assert_tokens(
            source,
            TokenStreamDetailLevel::Full,
            &[
                "Newline",
                "Whitespace",
                "Identifier(hello)",
                "Whitespace",
                "Identifier(world)",
            ],
            &[],
        );
    }

    #[test]
    fn test_tokenize_simple_program() {
        let source = r#"
            let a = 200
            if a > 100 print("hello world")
        "#;

        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            &[
                "Let",
                "Identifier(a)",
                "Assign",
                "Integer(200 base 10 in _)",
                "If",
                "Identifier(a)",
                "Gt",
                "Integer(100 base 10 in _)",
                "Identifier(print)",
                "LeftParen",
                "String(hello world)",
                "RightParen",
            ],
            &[],
        );
    }

    #[test]
    fn test_tokenize_identifiers() {
        let source = "foo Foo foo_bar FOO_BAR $123 _123";
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            &[
                "Identifier(foo)",
                "Identifier(Foo)",
                "Identifier(foo_bar)",
                "Identifier(FOO_BAR)",
                "Identifier($123)",
                "Identifier(_123)",
            ],
            &[],
        );
    }

    #[test]
    fn test_tokenize_numbers() {
        let source = r#"
            0x0123456789abcdef
            0x
            0xcafebabeu64

            1025
            0
            1025u64

            0o0123456
            0o
            0o666u32

            0b01
            0b
            0b1010u32

            256u32
            256f32
            256foobar

            25.0
            0.25
            0.0

            25.0f64
            0.25f64
            0.0f64

            25.0foobar
            0.25foobar
            0.0foobar

            25.foo
            foo.25

            25.25.25
        "#;
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            &[
                "Integer(0123456789abcdef base 16 in _)",
                "Integer( base 16 in _)",
                "Integer(cafebabe base 16 in u64)",
                "Integer(1025 base 10 in _)",
                "Integer(0 base 10 in _)",
                "Integer(1025 base 10 in u64)",
                "Integer(0123456 base 8 in _)",
                "Integer( base 8 in _)",
                "Integer(666 base 8 in u32)",
                "Integer(01 base 2 in _)",
                "Integer( base 2 in _)",
                "Integer(1010 base 2 in u32)",
                "Integer(256 base 10 in u32)",
                "Integer(256 base 10 in f32)",
                "Integer(256 base 10 in foobar)",
                "Float(25.0 in _)",
                "Float(0.25 in _)",
                "Float(0.0 in _)",
                "Float(25.0 in f64)",
                "Float(0.25 in f64)",
                "Float(0.0 in f64)",
                "Float(25.0 in foobar)",
                "Float(0.25 in foobar)",
                "Float(0.0 in foobar)",
                "Integer(25 base 10 in _)",
                "Dot",
                "Identifier(foo)",
                "Identifier(foo)",
                "Dot",
                "Integer(25 base 10 in _)",
                "Float(25.25 in _)",
                "Dot",
                "Integer(25 base 10 in _)",
            ],
            &[],
        );
    }

    #[test]
    fn test_tokenize_strings() {
        let source = r#"
            "hello world"
            ""
            "hello\nworld"
            "äöü"
            "❤️"
            "\n\r\t\"\\"
        "#;
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            &[
                "String(hello world)",
                "String()",
                "String(hello\nworld)",
                "String(äöü)",
                "String(❤️)",
                "String(\n\r\t\"\\)",
            ],
            &[],
        );
    }

    #[test]
    fn test_tokenize_unnecessary_string_escapes() {
        let source = r#"
            "\a\b\c"
        "#;
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            &[r#"String(abc)"#],
            &["Unnecessary escape sequence"],
        );
    }

    #[test]
    fn test_tokenize_format_strings() {
        let source = r#"
            "hello $name!"
            "$foo"
            "$foo $bar"
            "${foo} ${bar}"
            "${foo + bar}"
            "foo${"foo"}foo"
        "#;
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            &[
                r#"FormatStringPart(hello )"#,
                r#"Identifier(name)"#,
                r#"String(!)"#,
                r#"FormatStringPart()"#,
                r#"Identifier(foo)"#,
                r#"String()"#,
                r#"FormatStringPart()"#,
                r#"Identifier(foo)"#,
                r#"FormatStringPart( )"#,
                r#"Identifier(bar)"#,
                r#"String()"#,
                r#"FormatStringPart()"#,
                r#"Identifier(foo)"#,
                r#"FormatStringPart( )"#,
                r#"Identifier(bar)"#,
                r#"String()"#,
                r#"FormatStringPart()"#,
                r#"Identifier(foo)"#,
                r#"Add"#,
                r#"Identifier(bar)"#,
                r#"String()"#,
                r#"FormatStringPart(foo)"#,
                r#"String(foo)"#,
                r#"String(foo)"#,
            ],
            &[],
        );
    }

    #[test]
    fn test_tokenize_comments() {
        let source = r#"
// single line comment
/*
    multi
    line
    comment
*/
foo // single line comment
        "#;
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespace,
            &[
                "SingleLineComment",
                "MultiLineComment",
                "Identifier(foo)",
                "SingleLineComment",
            ],
            &[],
        );
    }

    #[test]
    fn test_tokenize_keywords_and_punctuators() {
        // build the input source string
        let mut source_buf = String::new();
        for keyword in TOKEN_KEYWORDS {
            source_buf.push_str(format!("{} ", keyword.to_string()).as_str());
        }
        for punctuator in TOKEN_PUNCTUATORS {
            source_buf.push_str(format!("{} ", punctuator.to_string()).as_str());
        }

        // build the expected list of tokens
        let mut expected_tokens: Vec<String> = Vec::new();
        for keyword in TOKEN_KEYWORDS {
            expected_tokens.push(format!("{:?}", keyword));
        }
        for punctuator in TOKEN_PUNCTUATORS {
            expected_tokens.push(format!("{:?}", punctuator));
        }

        let expected_tokens_str_ref: Vec<&str> =
            expected_tokens.iter().map(String::as_str).collect();

        assert_tokens(
            source_buf.as_str(),
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            expected_tokens_str_ref.as_slice(),
            &[],
        );
    }

    #[test]
    fn test_tokenize_whitespace() {
        let source = "    foo\n    bar\r\n    baz";
        assert_tokens(
            source,
            TokenStreamDetailLevel::Full,
            &[
                "Whitespace",
                "Identifier(foo)",
                "Newline",
                "Whitespace",
                "Identifier(bar)",
                "Newline",
                "Whitespace",
                "Identifier(baz)",
            ],
            &[],
        );
    }

    #[test]
    fn test_unexpected_character() {
        let source = "ä";
        assert_tokens(
            source,
            TokenStreamDetailLevel::Full,
            &["Error"],
            &["Unexpected 'ä' character"],
        );
    }

    #[test]
    fn test_unclosed_string_literal() {
        let source = "\"hello world";
        assert_tokens(
            source,
            TokenStreamDetailLevel::Full,
            &["Error"],
            &["Unclosed string literal"],
        );
    }

    #[test]
    fn test_nested_unclosed_string_literal() {
        let source = r#""hello ${"}"#;
        assert_tokens(
            source,
            TokenStreamDetailLevel::Full,
            &["FormatStringPart(hello )", "Error"],
            &["Unclosed string literal"],
        );
    }
}
