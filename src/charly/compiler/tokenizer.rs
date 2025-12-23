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
use crate::charly::utils::fuel_store::FuelStore;
use crate::charly::utils::window_buffer::{TextPosition, TextSpan, WindowBuffer};

pub enum TokenizerMode {
    Token,
    String,
}

enum BraceStackEntry {
    Regular,
    StringInterpolation,
}

pub struct Tokenizer<'a> {
    diagnostic_context: &'a mut DiagnosticContext,
    buffer: WindowBuffer<'a>,
    file_id: FileId,
    mode: TokenizerMode,
    tokens: Vec<Token>,
    fuel: FuelStore,
    brace_stack: Vec<BraceStackEntry>,
    opened_string_literals: Vec<TextSpan>,
}

impl<'a> Tokenizer<'a> {
    pub fn tokenize(
        buffer: &'a str,
        diagnostic_context: &'a mut DiagnosticContext,
    ) -> Vec<Token> {
        let file_id = diagnostic_context.file_id;
        let mut tokenizer = Self {
            diagnostic_context,
            buffer: WindowBuffer::new(buffer),
            file_id,
            mode: TokenizerMode::Token,
            tokens: Vec::new(),
            fuel: FuelStore::new("Tokenizer", 32),
            brace_stack: Vec::new(),
            opened_string_literals: Vec::new(),
        };

        tokenizer.collect_tokens();
        tokenizer.finalize()
    }

    pub fn finalize(self) -> Vec<Token> {
        self.tokens
    }

    fn collect_tokens(&mut self) {
        while !self.buffer.eof() {
            self.fuel.consume();
            match self.mode {
                TokenizerMode::Token => self.consume_token(),
                TokenizerMode::String => self.consume_string(),
            }
        }

        self.emit_token(TokenKind::EndOfFile);
    }

    fn consume_token(&mut self) {
        let char = self
            .buffer
            .peek_char()
            .expect("expected at least one character");

        // CRLF line separator
        if let Some("\r\n") = self.buffer.peek_str_with_length(2) {
            assert!(self.buffer.eat_str("\r\n"));
            self.emit_token(TokenKind::Newline);
            return;
        }

        // doc comments
        if let Some("///") = self.buffer.peek_str_with_length(3) {
            assert!(self.buffer.eat_str("///"));
            self.eat_until_newline();
            self.emit_token(TokenKind::DocComment);
            return;
        }

        // single line comments
        if let Some("//") = self.buffer.peek_str_with_length(2) {
            self.consume_single_line_comment();
            return;
        }

        // multi line comments
        if let Some("/*") = self.buffer.peek_str_with_length(2) {
            self.consume_multi_line_comment();
            return;
        }

        match char {
            // LF line separator
            '\n' => {
                self.buffer.advance();
                self.emit_token(TokenKind::Newline);
                return;
            }

            // string literals
            '"' => {
                self.buffer.advance();
                let quote_span = self.buffer.window_span.clone();
                self.opened_string_literals.push(quote_span);
                self.emit_token(TokenKind::StringStart);
                self.mode = TokenizerMode::String;
                return;
            }

            // raw identifier literals
            '`' => {
                self.consume_raw_text_identifier();
                return;
            }

            // character literals
            '\'' => {
                self.consume_character_literal();
                return;
            }

            // dash comments
            '#' => {
                self.consume_single_line_comment();
                return;
            }

            c if c.is_whitespace() => {
                self.consume_whitespace();
                return;
            }

            c if c.is_identifier_begin() => {
                self.consume_identifier();
                return;
            }

            c if c.is_decimal_digit() => {
                self.consume_number();
                return;
            }

            _ => {}
        }

        // punctuation tokens
        for n in (1..=TOKEN_MAX_STR_LEN).rev() {
            let Some(lookahead) = self.buffer.peek_str_with_length(n) else {
                continue;
            };

            let Some(kind) = TokenKind::try_from_str(lookahead) else {
                continue;
            };

            assert!(self.buffer.eat_str(kind.to_string()));

            match kind {
                TokenKind::LeftBrace => {
                    self.brace_stack.push(BraceStackEntry::Regular);
                }
                TokenKind::RightBrace => {
                    if let Some(entry) = self.brace_stack.pop() {
                        match entry {
                            BraceStackEntry::Regular => {}
                            BraceStackEntry::StringInterpolation => {
                                self.emit_token(TokenKind::StringExprEnd);
                                self.mode = TokenizerMode::String;
                                return;
                            }
                        }
                    }
                }
                _ => {}
            }

            self.emit_token(kind);
            return;
        }

        // unexpected character
        self.buffer.advance();
        self.emit_error_unexpected_char(char);
        self.emit_token(TokenKind::Error);
    }

    fn consume_string(&mut self) {
        let mut buf = String::new();

        while let Some(char) = self.buffer.peek_char() {
            let begin_pos = self.buffer.cursor_position();

            match char {
                // terminate string
                '"' => {
                    if buf.len() > 0 {
                        let value = TokenValue::TextValue(buf.to_string());
                        self.emit_token_with_value(TokenKind::StringText, value);
                    }

                    self.opened_string_literals
                        .pop()
                        .expect("expected opened string literal");
                    self.buffer.advance();
                    self.emit_token(TokenKind::StringEnd);
                    self.mode = TokenizerMode::Token;
                    return;
                }

                // string interpolation
                '{' => {
                    if buf.len() > 0 {
                        let value = TokenValue::TextValue(buf.to_string());
                        self.emit_token_with_value(TokenKind::StringText, value);
                    }

                    self.brace_stack.push(BraceStackEntry::StringInterpolation);
                    self.buffer.advance();
                    self.emit_token(TokenKind::StringExprStart);
                    self.mode = TokenizerMode::Token;
                    return;
                }

                // escape sequence
                '\\' => match self.buffer.peek_char_with_lookahead(1) {
                    Some(char) => {
                        self.buffer.advance_multiple(2);
                        match char {
                            'n' => buf.push('\n'),
                            'r' => buf.push('\r'),
                            't' => buf.push('\t'),
                            '"' => buf.push('"'),
                            '{' => buf.push('{'),
                            '\\' => buf.push('\\'),

                            // invalid escape sequence
                            invalid_character => {
                                let end_pos = self.buffer.cursor_position();
                                self.emit_error_invalid_escape_sequence(
                                    invalid_character,
                                    &TextSpan {
                                        start: begin_pos,
                                        end: end_pos,
                                    },
                                );
                            }
                        }
                    }

                    // handle EOF in next loop iteration
                    None => {
                        self.buffer.advance();
                    }
                },

                // part of the string
                _ => {
                    buf.push(char);
                    assert!(self.buffer.eat_char(char));
                }
            }
        }

        // unclosed string literal
        self.emit_error_unclosed_string_literal();
        self.emit_token(TokenKind::StringText);
    }

    fn consume_character_literal(&mut self) {
        assert!(self.buffer.eat_char('\''));
        let open_quote_span = self.buffer.window_span.clone();

        let mut buf = String::new();

        loop {
            let begin_pos = self.buffer.cursor_position();
            let char = self.buffer.peek_char();
            match char {
                // char literal terminated
                Some('\'') => {
                    self.buffer.advance();
                    match buf.chars().count() {
                        // correct character count
                        1 => {
                            self.emit_token_with_value(
                                TokenKind::Character,
                                TokenValue::CharacterValue(buf.chars().next().unwrap()),
                            );
                            return;
                        }

                        // empty character literal
                        0 => {
                            self.emit_error_empty_character_literal();
                            self.emit_token(TokenKind::Character);
                            return;
                        }

                        // too many characters in character literal
                        _ => {
                            self.emit_error_too_many_characters_in_character_literal();
                            self.emit_token(TokenKind::Character);
                            return;
                        }
                    }
                }

                // reached end of line -> unclosed char literal
                Some('\n') => {
                    self.emit_error_unclosed_char_literal(&open_quote_span);
                    self.emit_token(TokenKind::Character);
                    return;
                }

                Some('\r') => match self.buffer.peek_char_with_lookahead(1) {
                    Some('\n') => {
                        self.emit_error_unclosed_char_literal(&open_quote_span);
                        self.emit_token(TokenKind::Character);
                        return;
                    }

                    // ignore lone CR
                    // handle possible EOF in next loop iteration
                    _ => self.buffer.advance(),
                },

                // escape sequence
                Some('\\') => match self.buffer.peek_char_with_lookahead(1) {
                    Some(char) => {
                        self.buffer.advance_multiple(2);
                        match char {
                            'n' => buf.push('\n'),
                            'r' => buf.push('\r'),
                            't' => buf.push('\t'),
                            '\'' => buf.push('\''),
                            '\\' => buf.push('\\'),

                            // invalid escape sequence
                            invalid_character => {
                                let end_pos = self.buffer.cursor_position();
                                self.emit_error_invalid_escape_sequence(
                                    invalid_character,
                                    &TextSpan {
                                        start: begin_pos,
                                        end: end_pos,
                                    },
                                );
                            }
                        }
                    }

                    // handle EOF in next loop iteration
                    None => self.buffer.advance(),
                },

                // valid character, consume and add to buffer
                Some(char) => {
                    self.buffer.advance();
                    buf.push(char);
                }

                // unclosed character literal
                None => {
                    self.emit_error_unclosed_char_literal(&open_quote_span);
                    self.emit_token(TokenKind::Character);
                    return;
                }
            }
        }
    }

    fn consume_identifier(&mut self) {
        let first_char = self
            .buffer
            .read_char()
            .expect("expected at least one character");

        assert!(first_char.is_identifier_begin());

        while let Some(char) = self.buffer.peek_char() {
            if !char.is_identifier_part() {
                break;
            }
            self.buffer.advance();
        }

        let identifier = self.buffer.window_as_str();
        match TokenKind::try_from_str(identifier) {
            Some(kind) => self.emit_token(kind),
            None => self.emit_token_with_value(
                TokenKind::Identifier,
                TokenValue::TextValue(identifier.to_string()),
            ),
        }
    }

    fn consume_raw_text_identifier(&mut self) {
        assert!(self.buffer.eat_char('`'));
        let open_span = self.buffer.window_span.clone();

        // consume any character until we hit a closing `
        let mut buf = String::new();

        loop {
            if let Some(char) = self.buffer.peek_char() {
                if self.buffer.eat_char('`') {
                    break;
                }
                self.buffer.advance();
                buf.push(char);
            } else {
                // reached EOF without closing `
                self.emit_error_unclosed_raw_identifier_literal(&open_span);
                break;
            }
        }

        self.emit_token_with_value(
            TokenKind::Identifier,
            TokenValue::TextValue(buf.to_string()),
        )
    }

    fn consume_number(&mut self) {
        use IntegerBaseSpecifier::*;

        let char = self.buffer.peek_char().unwrap();
        assert!(char.is_decimal_digit());

        match self.buffer.peek_str_with_length(2) {
            Some("0x") => self.consume_number_with_base(Hexadecimal),
            Some("0o") => self.consume_number_with_base(Octal),
            Some("0b") => self.consume_number_with_base(Binary),
            _ => self.consume_number_with_base(Decimal),
        }
    }

    fn consume_number_with_base(&mut self, base: IntegerBaseSpecifier) {
        use IntegerBaseSpecifier::*;

        // skip the base specifier if present
        match base {
            Hexadecimal => assert!(self.buffer.eat_str("0x")),
            Decimal => {}
            Octal => assert!(self.buffer.eat_str("0o")),
            Binary => assert!(self.buffer.eat_str("0b")),
        }

        match base {
            Hexadecimal => self.consume_integer_with_base(base),
            Decimal => self.consume_decimal(),
            Octal => self.consume_integer_with_base(base),
            Binary => self.consume_integer_with_base(base),
        }
    }

    fn consume_integer_with_base(&mut self, base: IntegerBaseSpecifier) {
        // consume numeric digits
        let mut digit_buf = String::new();
        self.consume_digits_of_base(&mut digit_buf, &base);

        // consume suffix
        let suffix = self.consume_numeric_suffix();

        self.emit_token_with_value(
            TokenKind::Integer,
            TokenValue::IntegerValue(digit_buf, base, suffix),
        )
    }

    /// Decimal = Digit+ ("." Digit+) Suffix?
    fn consume_decimal(&mut self) {
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
                    assert!(self.buffer.eat_char('.'));
                    self.consume_digits_of_base(
                        &mut digit_buf,
                        &IntegerBaseSpecifier::Decimal,
                    );
                }
            }
        }

        // consume Suffix?
        let suffix = self.consume_numeric_suffix();

        if is_float {
            self.emit_token_with_value(
                TokenKind::Float,
                TokenValue::FloatValue(digit_buf, suffix),
            )
        } else {
            self.emit_token_with_value(
                TokenKind::Integer,
                TokenValue::IntegerValue(
                    digit_buf,
                    IntegerBaseSpecifier::Decimal,
                    suffix,
                ),
            )
        }
    }

    fn consume_digits_of_base(&mut self, buf: &mut String, base: &IntegerBaseSpecifier) {
        while let Some(char) = self.buffer.peek_char() {
            if char.is_digit_of_base(base) {
                self.buffer.advance();
                buf.push(char);
            } else if char == '_' {
                self.buffer.advance();
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

                    self.buffer.advance();
                    suffix_buf.push(char);
                }

                return Some(suffix_buf);
            }
        }

        None
    }

    fn consume_single_line_comment(&mut self) {
        self.eat_until_newline();
        self.emit_token(TokenKind::SingleLineComment);
    }

    fn consume_multi_line_comment(&mut self) {
        assert!(self.buffer.eat_str("/*"));
        let comment_open_span = self.buffer.window_span.clone();

        let mut nest_count = 1;
        while let Some(_) = self.buffer.peek_char() {
            match self.buffer.peek_str_with_length(2) {
                Some("/*") => {
                    nest_count += 1;
                    assert!(self.buffer.eat_str("/*"));
                    continue;
                }

                Some("*/") => {
                    nest_count -= 1;
                    assert!(self.buffer.eat_str("*/"));

                    if nest_count == 0 {
                        self.emit_token(TokenKind::MultiLineComment);
                        return;
                    }

                    continue;
                }

                _ => {}
            }

            self.buffer.advance();
        }

        // unclosed block comment
        self.emit_error_unclosed_multi_line_comment(&comment_open_span);
        self.emit_token(TokenKind::MultiLineComment);
    }

    fn consume_whitespace(&mut self) {
        // consume whitespace characters until we find LF or CRLF
        while let Some(char) = self.buffer.peek_char() {
            match char {
                '\n' => break,

                '\r' => match self.buffer.peek_char_with_lookahead(1) {
                    Some('\n') => break,
                    _ => self.buffer.advance(),
                },

                c if c.is_whitespace() => self.buffer.advance(),

                _ => break,
            }
        }

        self.emit_token(TokenKind::Whitespace);
    }

    fn eat_until_newline(&mut self) {
        while let Some(char) = self.buffer.peek_char() {
            match char {
                '\n' => break,
                '\r' => {
                    if self.buffer.peek_char_with_lookahead(1) == Some('\n') {
                        break;
                    } else {
                        self.buffer.advance();
                    }
                }
                _ => self.buffer.advance(),
            }
        }
    }

    fn emit_token(&mut self, kind: TokenKind) {
        let token = self.build_token(kind, None);
        self.push_token(token);
    }

    fn emit_token_with_value(&mut self, kind: TokenKind, value: TokenValue) {
        let token = self.build_token(kind, Some(value));
        self.push_token(token);
    }

    fn push_token(&mut self, token: Token) {
        self.tokens.push(token);
        self.buffer.reset_window();
        self.fuel.replenish();
    }

    fn build_token(&mut self, kind: TokenKind, value: Option<TokenValue>) -> Token {
        Token {
            kind,
            location: self.window_location(),
            raw: self.buffer.window_as_str().to_string(),
            value,
        }
    }

    fn window_location(&self) -> DiagnosticLocation {
        self.build_location_from_span(&self.buffer.window_span)
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

    fn build_location_from_span(&self, span: &TextSpan) -> DiagnosticLocation {
        self.build_location(&span.start, &span.end)
    }

    fn emit_error_unexpected_char(&mut self, char: char) {
        self.diagnostic_context.error(
            format!("Unexpected character: '{}'", char).as_str(),
            &self.window_location(),
            vec![],
            vec![],
        );
    }

    fn emit_error_invalid_escape_sequence(&mut self, character: char, span: &TextSpan) {
        self.diagnostic_context.error(
            format!("Illegal escape sequence: '\\{}'", character).as_str(),
            &self.build_location_from_span(&span),
            vec![],
            vec![],
        );
    }

    fn emit_error_unclosed_string_literal(&mut self) {
        let open_quote_span = match self.opened_string_literals.last() {
            Some(position) => position,
            None => &self.buffer.window_span,
        };

        let label_location = self.build_location_from_span(&open_quote_span);
        self.diagnostic_context.error(
            "Unclosed string literal",
            &label_location,
            vec![(Some("Opened here"), label_location.clone())],
            vec![],
        );
    }

    fn emit_error_unclosed_multi_line_comment(&mut self, open_span: &TextSpan) {
        let label_location = self.build_location_from_span(open_span);
        self.diagnostic_context.error(
            "Unclosed multi-line comment",
            &label_location,
            vec![(Some("Opened here"), label_location.clone())],
            vec![],
        );
    }

    fn emit_error_empty_character_literal(&mut self) {
        self.diagnostic_context.error(
            "Empty character literal",
            &self.window_location(),
            vec![(None, self.window_location())],
            vec![],
        );
    }

    fn emit_error_too_many_characters_in_character_literal(&mut self) {
        self.diagnostic_context.error(
            "Too many characters in character literal",
            &self.window_location(),
            vec![(None, self.window_location())],
            vec![],
        );
    }

    fn emit_error_unclosed_char_literal(&mut self, open_span: &TextSpan) {
        let label_location = self.build_location_from_span(open_span);
        self.diagnostic_context.error(
            "Unclosed character literal",
            &label_location,
            vec![(Some("Opened here"), label_location.clone())],
            vec![],
        );
    }

    fn emit_error_unclosed_raw_identifier_literal(&mut self, open_span: &TextSpan) {
        let label_location = self.build_location_from_span(open_span);
        self.diagnostic_context.error(
            "Unclosed raw identifier literal",
            &label_location,
            vec![(Some("Opened here"), label_location.clone())],
            vec![],
        );
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
    use crate::charly::compiler::token::{TOKEN_KEYWORDS, TOKEN_PUNCTUATORS};
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

        let tokens: Vec<String> = Tokenizer::tokenize(source, &mut context)
            .iter()
            .filter(|token| token.kind != TokenKind::EndOfFile)
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
            .map(|token| format!("{}", token))
            .collect();

        let mut token_iter = tokens.iter();

        for (index, expected) in expected_tokens.iter().enumerate() {
            let actual = match token_iter.next() {
                Some(token) => token,
                None => panic!("expected token {} at index {}", expected, index),
            };
            assert_eq!(actual, *expected, "at index {}", index);
        }

        if let Some(token) = token_iter.next() {
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
                "SingleLineComment",
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
        let source = "let a = 200 if a > 100 print(\"hello world\")";
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            &[
                "Let",
                "Identifier(a)",
                "Assign",
                "Integer(200 base 10)",
                "If",
                "Identifier(a)",
                "Gt",
                "Integer(100 base 10)",
                "Identifier(print)",
                "LeftParen",
                "StringStart",
                "StringText(hello world)",
                "StringEnd",
                "RightParen",
            ],
            &[],
        );
    }

    #[test]
    fn test_different_line_separators() {
        let source = "foo\nbar\r\nbaz";
        assert_tokens(
            source,
            TokenStreamDetailLevel::Full,
            &[
                "Identifier(foo)",
                "Newline",
                "Identifier(bar)",
                "Newline",
                "Identifier(baz)",
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
            0xcafe_babe
            0xcafe_babeu64
            100_000_000
            100_000_000u32
            0o666_555
            0o666_555u32
            0b1010_1010
            0b1010_1010u8


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
                "Integer(cafebabe base 16)",
                "Integer(cafebabe base 16 in u64)",
                "Integer(100000000 base 10)",
                "Integer(100000000 base 10 in u32)",
                "Integer(666555 base 8)",
                "Integer(666555 base 8 in u32)",
                "Integer(10101010 base 2)",
                "Integer(10101010 base 2 in u8)",
                "Integer(0123456789abcdef base 16)",
                "Integer( base 16)",
                "Integer(cafebabe base 16 in u64)",
                "Integer(1025 base 10)",
                "Integer(0 base 10)",
                "Integer(1025 base 10 in u64)",
                "Integer(0123456 base 8)",
                "Integer( base 8)",
                "Integer(666 base 8 in u32)",
                "Integer(01 base 2)",
                "Integer( base 2)",
                "Integer(1010 base 2 in u32)",
                "Integer(256 base 10 in u32)",
                "Integer(256 base 10 in f32)",
                "Integer(256 base 10 in foobar)",
                "Float(25.0)",
                "Float(0.25)",
                "Float(0.0)",
                "Float(25.0 in f64)",
                "Float(0.25 in f64)",
                "Float(0.0 in f64)",
                "Float(25.0 in foobar)",
                "Float(0.25 in foobar)",
                "Float(0.0 in foobar)",
                "Integer(25 base 10)",
                "Dot",
                "Identifier(foo)",
                "Identifier(foo)",
                "Dot",
                "Integer(25 base 10)",
                "Float(25.25)",
                "Dot",
                "Integer(25 base 10)",
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
            "😂"
            "\n\r\t\"\\\{"
        "#;
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            &[
                "StringStart",
                "StringText(hello world)",
                "StringEnd",
                "StringStart",
                "StringEnd",
                "StringStart",
                "StringText(hello\\nworld)",
                "StringEnd",
                "StringStart",
                "StringText(äöü)",
                "StringEnd",
                "StringStart",
                "StringText(😂)",
                "StringEnd",
                "StringStart",
                "StringText(\\n\\r\\t\\\"\\\\{)",
                "StringEnd",
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
            &["StringStart", "StringEnd"],
            &[
                "Illegal escape sequence: '\\a'",
                "Illegal escape sequence: '\\b'",
                "Illegal escape sequence: '\\c'",
            ],
        );
    }

    #[test]
    fn test_tokenize_format_strings() {
        let source = r#"
            "hello {name}!"
            "{foo}"
            "{foo} {bar}"
            "{foo + bar}"
            "foo{"foo"}foo"
        "#;
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            &[
                "StringStart",
                "StringText(hello )",
                "StringExprStart",
                "Identifier(name)",
                "StringExprEnd",
                "StringText(!)",
                "StringEnd",
                "StringStart",
                "StringExprStart",
                "Identifier(foo)",
                "StringExprEnd",
                "StringEnd",
                "StringStart",
                "StringExprStart",
                "Identifier(foo)",
                "StringExprEnd",
                "StringText( )",
                "StringExprStart",
                "Identifier(bar)",
                "StringExprEnd",
                "StringEnd",
                "StringStart",
                "StringExprStart",
                "Identifier(foo)",
                "Add",
                "Identifier(bar)",
                "StringExprEnd",
                "StringEnd",
                "StringStart",
                "StringText(foo)",
                "StringExprStart",
                "StringStart",
                "StringText(foo)",
                "StringEnd",
                "StringExprEnd",
                "StringText(foo)",
                "StringEnd",
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

                /*
                    nested multi-line comment
                */
            */
            /// foo bar
            /// foo bar baz
            # foo bar
            # hello world
            foo // single line comment
        "#;
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespace,
            &[
                "SingleLineComment",
                "MultiLineComment",
                "DocComment",
                "DocComment",
                "SingleLineComment",
                "SingleLineComment",
                "Identifier(foo)",
                "SingleLineComment",
            ],
            &[],
        );
    }

    #[test]
    fn test_unclosed_multi_line_comment() {
        let source = r#"
            /*
                multi
                line
                comment
        "#;
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespace,
            &["MultiLineComment"],
            &["Unclosed multi-line comment"],
        );
    }

    #[test]
    fn test_tokenize_keywords_and_punctuators() {
        // build the input source string
        let mut source_buf = String::new();
        for keyword in TOKEN_KEYWORDS.iter() {
            source_buf.push_str(format!("{} ", keyword.to_string()).as_str());
        }
        for punctuator in TOKEN_PUNCTUATORS.iter() {
            source_buf.push_str(format!("{} ", punctuator.to_string()).as_str());
        }

        // build the expected list of tokens
        let mut expected_tokens: Vec<String> = Vec::new();
        for keyword in TOKEN_KEYWORDS.iter() {
            expected_tokens.push(format!("{:?}", keyword));
        }
        for punctuator in TOKEN_PUNCTUATORS.iter() {
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
            &["Unexpected character: 'ä'"],
        );
    }

    #[test]
    fn test_unclosed_string_literal() {
        let source = "\"hello world";
        assert_tokens(
            source,
            TokenStreamDetailLevel::Full,
            &["StringStart", "StringText"],
            &["Unclosed string literal"],
        );
    }

    #[test]
    fn test_unclosed_string_literal_with_crlf() {
        let source = "\"hello world\r\n";
        assert_tokens(
            source,
            TokenStreamDetailLevel::Full,
            &["StringStart", "StringText"],
            &["Unclosed string literal"],
        );
    }

    #[test]
    fn test_crlf_line_separator() {
        let source = "\"hello world\r\nfoo\"";
        assert_tokens(
            source,
            TokenStreamDetailLevel::Full,
            &[
                "StringStart",
                "StringText(hello world\\r\\nfoo)",
                "StringEnd",
            ],
            &[],
        );
    }

    #[test]
    fn test_unclosed_string_literal_after_unfinished_escape_sequence() {
        let source = "\"hello world\\";
        assert_tokens(
            source,
            TokenStreamDetailLevel::Full,
            &["StringStart", "StringText"],
            &["Unclosed string literal"],
        );
    }

    #[test]
    fn test_nested_unclosed_string_literal() {
        let source = r#""hello {"}"#;
        assert_tokens(
            source,
            TokenStreamDetailLevel::Full,
            &[
                "StringStart",
                "StringText(hello )",
                "StringExprStart",
                "StringStart",
                "StringText",
            ],
            &["Unclosed string literal"],
        );
    }

    #[test]
    fn test_raw_identifier_literal() {
        let source = r#"
            `hello world`
            `hello {name}`
            `\t\n`
        "#;
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            &[
                "Identifier(hello world)",
                "Identifier(hello {name})",
                "Identifier(\\\\t\\\\n)",
            ],
            &[],
        );
    }

    #[test]
    fn test_unclosed_raw_identifier_literal() {
        let source = "`hello world";
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            &["Identifier(hello world)"],
            &["Unclosed raw identifier literal"],
        );
    }

    #[test]
    fn test_character_literals() {
        let source = r#"
            'x'
            'ä'
            '😂'
            '\t'
            '\n'
            '\r'
            '\''
            '\\'
        "#;
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            &[
                "Character(x)",
                "Character(ä)",
                "Character(😂)",
                "Character(\\t)",
                "Character(\\n)",
                "Character(\\r)",
                "Character(\\')",
                "Character(\\\\)",
            ],
            &[],
        );
    }

    #[test]
    fn test_empty_character_literal() {
        let source = "''";
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            &["Character"],
            &["Empty character literal"],
        );
    }

    #[test]
    fn test_too_many_characters_in_character_literal() {
        let source = "'foo'";
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            &["Character"],
            &["Too many characters in character literal"],
        );
    }

    #[test]
    fn test_unclosed_character_literal() {
        let source = "'f";
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            &["Character"],
            &["Unclosed character literal"],
        );
    }

    #[test]
    fn test_unclosed_character_literal_with_lf() {
        let source = "'f\n";
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            &["Character"],
            &["Unclosed character literal"],
        );
    }

    #[test]
    fn test_unclosed_character_literal_with_crlf() {
        let source = "'f\r\n";
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            &["Character"],
            &["Unclosed character literal"],
        );
    }

    #[test]
    fn test_illegal_escape_sequence_in_character_literal() {
        let source = "'\\a'";
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            &["Character"],
            &["Empty character literal", "Illegal escape sequence: '\\a'"],
        );
    }

    #[test]
    fn test_unclosed_character_literal_after_unfinished_escape_sequence() {
        let source = "'\\";
        assert_tokens(
            source,
            TokenStreamDetailLevel::NoWhitespaceAndComments,
            &["Character"],
            &["Unclosed character literal"],
        );
    }
}
