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

use crate::charly::diagnostics::{DiagnosticContext, DiagnosticLocation, FileId};
use crate::charly::token::TokenError::*;
use crate::charly::token::{Token, TokenType};
use crate::charly::window_buffer::{TextPosition, TextSpan, WindowBuffer};
use std::str::FromStr;

#[derive(PartialEq)]
pub enum TokenizerMode {
    TopLevel,
    String,
    InterpolatedExpression,
    InterpolatedIdentifier,
}

pub struct Tokenizer<'a> {
    buffer: WindowBuffer<'a>,
    file_id: FileId,
    mode: TokenizerMode,

    /// contains opened paren, brace and bracket tokens
    bracket_stack: Vec<TokenType>,

    /// contains the size of the bracket_stack the last
    /// time a string interpolation was started
    interpolation_stack: Vec<usize>,

    diagnostic_context: &'a mut DiagnosticContext,
}

#[allow(dead_code)]
pub enum TokenDetailLevel {
    Full,
    NoWhitespace,
    NoWhitespaceAndComments,
}

impl Iterator for Tokenizer<'_> {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        self.read_token()
    }
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str, file_id: FileId, context: &'a mut DiagnosticContext) -> Self {
        Self {
            buffer: WindowBuffer::new(source),
            file_id,
            mode: TokenizerMode::TopLevel,
            bracket_stack: Vec::new(),
            interpolation_stack: Vec::new(),
            diagnostic_context: context,
        }
    }

    pub fn iter(&mut self, detail_level: TokenDetailLevel) -> impl Iterator<Item = Token> {
        self.filter(move |t| match detail_level {
            TokenDetailLevel::Full => true,
            TokenDetailLevel::NoWhitespace => match t.token_type {
                TokenType::Whitespace => false,
                TokenType::Newline => false,
                _ => true,
            },
            TokenDetailLevel::NoWhitespaceAndComments => match t.token_type {
                TokenType::Whitespace => false,
                TokenType::Newline => false,
                TokenType::SingleLineComment(_) => false,
                TokenType::MultiLineComment(_) => false,
                _ => true,
            },
        })
    }

    fn read_token(&mut self) -> Option<Token> {
        let token = self.read_token_impl()?;

        match &token.token_type {
            TokenType::Error(error) => match error {
                UnexpectedCharacter(character) => {
                    self.diagnostic_context.error(
                        format!("Unexpected '{}' character", character).as_str(),
                        &token.location,
                        vec![],
                        vec![],
                    );
                }
                UnclosedStringLiteral => {
                    self.diagnostic_context.error(
                        "Unclosed string literal",
                        &token.location,
                        vec![],
                        vec![],
                    );
                }
                MalformedInteger(parse_error) => self.diagnostic_context.error(
                    format!("Malformed integer ({})", parse_error).as_str(),
                    &token.location,
                    vec![],
                    vec![],
                ),
                MalformedFloat(parse_error) => self.diagnostic_context.error(
                    format!("Malformed float ({})", parse_error).as_str(),
                    &token.location,
                    vec![],
                    vec![],
                ),
            },
            _ => {}
        }

        Some(token)
    }

    fn read_token_impl(&mut self) -> Option<Token> {
        self.buffer.reset_window();

        let char = self.buffer.peek_char()?;

        match self.mode {
            TokenizerMode::String => return Some(self.consume_string(false)),
            TokenizerMode::InterpolatedIdentifier => {
                return Some(self.consume_identifier());
            }
            _ => {}
        }

        let token = match char {
            c if c.is_whitespace() => Some(self.consume_whitespace()),
            c if c.is_identifier_begin() => Some(self.consume_identifier()),
            c if c.is_decimal_digit() => Some(self.consume_number()),

            // string literals
            '\"' => Some(self.consume_string(true)),

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

        // recognize known operators and punctuators
        for n in (1..=4).rev() {
            let lookahead = self.buffer.peek_str(n);

            // TODO: refactor this mess
            match TokenType::try_token(lookahead) {
                Some(token_type) => {
                    self.buffer.advance(n);

                    match token_type {
                        TokenType::LeftParen | TokenType::LeftBrace | TokenType::LeftBracket => {
                            self.bracket_stack.push(token_type.clone())
                        }

                        TokenType::RightParen | TokenType::RightBrace | TokenType::RightBracket => {
                            let expected_match = token_type.matching_bracket().unwrap();
                            let actual_match = self.bracket_stack.last();

                            match actual_match {
                                Some(actual) if *actual == expected_match => {
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
                                _ => {}
                            }
                        }

                        _ => {}
                    }

                    return Some(self.build_token(token_type));
                }
                None => {}
            }
        }

        self.buffer.advance(1);
        Some(self.build_token(TokenType::Error(UnexpectedCharacter(char))))
    }

    fn build_token(&mut self, token_type: TokenType) -> Token {
        let location = DiagnosticLocation {
            file_id: self.file_id,
            span: self.buffer.window_span.clone(),
        };
        Token {
            token_type,
            location,
            raw: self.buffer.window_as_str().to_string(),
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

        self.build_token(TokenType::Whitespace)
    }

    fn consume_newline(&mut self) -> Token {
        let char = self.buffer.read_char().unwrap();
        assert!(char == '\n' || char == '\r');

        match char {
            // newlines
            '\n' => self.build_token(TokenType::Newline),
            '\r' => {
                let char = self.buffer.peek_char();
                match char {
                    Some('\n') => {
                        self.buffer.advance(1);
                        self.build_token(TokenType::Newline)
                    }

                    _ => self.build_token(TokenType::Whitespace),
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
                        let loc = self.build_location(&start_pos, &end_pos);
                        self.diagnostic_context.warning(
                            "Unnecessary escape sequence",
                            &loc,
                            vec![(None, loc.clone())],
                            vec![],
                        );
                        content.push(c);
                    }
                    None => return self.build_token(TokenType::Error(UnclosedStringLiteral)),
                },
                Some(c) if c == '$' => match self.buffer.peek_char() {
                    Some('{') => {
                        self.buffer.advance(1);
                        self.mode = TokenizerMode::InterpolatedExpression;
                        self.interpolation_stack.push(self.bracket_stack.len());
                        self.bracket_stack.push(TokenType::LeftBrace);
                        return self.build_token(TokenType::FormatStringPart(content));
                    }
                    Some(char) if char.is_identifier_begin() => {
                        self.mode = TokenizerMode::InterpolatedIdentifier;
                        return self.build_token(TokenType::FormatStringPart(content));
                    }
                    Some(_) => content.push(c),
                    None => return self.build_token(TokenType::Error(UnclosedStringLiteral)),
                },
                Some(c) => content.push(c),
                None => return self.build_token(TokenType::Error(UnclosedStringLiteral)),
            }
        }

        if self.interpolation_stack.is_empty() {
            self.mode = TokenizerMode::TopLevel;
        } else {
            self.mode = TokenizerMode::InterpolatedExpression;
        }

        self.build_token(TokenType::String(content))
    }

    fn consume_number(&mut self) -> Token {
        let char = self.buffer.peek_char().unwrap();
        assert!(char.is_decimal_digit());

        match self.buffer.peek_str(2) {
            Some("0x") => self.consume_number_with_base(16, 'x'),
            Some("0o") => self.consume_number_with_base(8, 'o'),
            Some("0b") => self.consume_number_with_base(2, 'b'),
            _ => self.consume_decimal(),
        }
    }

    fn consume_decimal(&mut self) -> Token {
        assert!(self.buffer.peek_char().unwrap().is_decimal_digit());

        let mut found_decimal_point = false;

        loop {
            match self.buffer.peek_char() {
                Some(c) if c.is_decimal_digit() => self.buffer.advance(1),
                Some('.') => {
                    // there can only be one decimal point
                    if found_decimal_point {
                        break;
                    }

                    let next = self.buffer.peek_char_with_lookahead(1);
                    match next {
                        // only allow decimal point if there is a digit after it,
                        // otherwise it is treated as a separate dot token
                        Some(c) if c.is_decimal_digit() => {
                            self.buffer.advance(2);
                            found_decimal_point = true;
                        }
                        _ => break,
                    }
                }
                _ => break,
            }
        }

        if found_decimal_point {
            let window_text = self.buffer.window_as_str();
            let value = f64::from_str(window_text);

            match value {
                Ok(value) => self.build_token(TokenType::Float(value)),
                Err(err) => self.build_token(TokenType::Error(MalformedFloat(err))),
            }
        } else {
            let window_text = self.buffer.window_as_str();
            let value = i64::from_str_radix(window_text, 10);

            match value {
                Ok(value) => self.build_token(TokenType::Integer(value)),
                Err(err) => self.build_token(TokenType::Error(MalformedInteger(err))),
            }
        }
    }

    fn consume_number_with_base(&mut self, base: u32, type_char: char) -> Token {
        assert_eq!(self.buffer.read_char().unwrap(), '0');
        assert_eq!(self.buffer.read_char().unwrap(), type_char);

        let mut buffer = String::new();

        loop {
            match self.buffer.peek_char() {
                Some(char) if char.is_digit(base) => {
                    self.buffer.advance(1);
                    buffer.push(char);
                }
                _ => break,
            }
        }

        let value = i64::from_str_radix(buffer.as_str(), base);

        match value {
            Ok(value) => self.build_token(TokenType::Integer(value)),
            Err(err) => self.build_token(TokenType::Error(MalformedInteger(err))),
        }
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
        let token_type = TokenType::try_keyword_or_identifier(window_text);

        if self.mode == TokenizerMode::InterpolatedIdentifier {
            self.mode = TokenizerMode::String;
        }

        self.build_token(token_type)
    }

    fn consume_comment(&mut self) -> Token {
        assert_eq!(self.buffer.read_char().unwrap(), '/');
        assert_eq!(self.buffer.read_char().unwrap(), '/');

        while let Some(c) = self.buffer.peek_char() {
            if c == '\n' {
                break;
            }
            self.buffer.advance(1);
        }

        let window_text = self.buffer.window_as_str();
        let comment = &window_text[2..].trim();

        self.build_token(TokenType::SingleLineComment(comment.to_string()))
    }

    fn consume_multiline_comment(&mut self) -> Token {
        assert_eq!(self.buffer.read_char().unwrap(), '/');
        assert_eq!(self.buffer.read_char().unwrap(), '*');

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

        // TODO: strip min common indentation from comment
        // TODO: strip leading empty lines from comment
        let window_text = self.buffer.window_as_str();
        let comment = &window_text[2..window_text.len() - 2].trim_end();
        self.build_token(TokenType::MultiLineComment(comment.to_string()))
    }

    fn build_location(&self, start: &TextPosition, end: &TextPosition) -> DiagnosticLocation {
        DiagnosticLocation {
            file_id: self.file_id,
            span: TextSpan {
                start: start.clone(),
                end: end.clone(),
            },
        }
    }
}

trait CharType {
    fn is_decimal_digit(&self) -> bool;
    fn is_identifier_begin(&self) -> bool;
    fn is_identifier_part(&self) -> bool;
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::charly::diagnostics::DiagnosticController;
    use std::path::PathBuf;

    #[track_caller]
    fn assert_tokens(
        source: &str,
        detail_level: TokenDetailLevel,
        expected_tokens: Vec<&str>,
        expected_diagnostic_messages: Vec<String>,
    ) {
        let mut controller = DiagnosticController::new();
        let path = PathBuf::from("test");
        let file_id = controller.register_file(&path, source);
        let context = controller.get_or_create_context(file_id);

        let tokens: Vec<Token> = {
            let mut tokenizer = Tokenizer::new(source, file_id, context);
            tokenizer.iter(detail_level).collect()
        };

        let mut iter = tokens.iter().map(|t| format!("{}", t.token_type));

        for (index, expected) in expected_tokens.iter().enumerate() {
            assert_eq!(iter.next().unwrap(), *expected, "at index {}", index);
        }
        assert!(iter.next().is_none());

        for diagnostic_message in &context.messages {
            let was_expected = expected_diagnostic_messages
                .iter()
                .find(|message| diagnostic_message.title.eq(*message));
            assert!(
                was_expected.is_some(),
                "Unexpected diagnostic message: {}",
                diagnostic_message.title
            );
        }

        for expected_message in expected_diagnostic_messages {
            let was_expected = &context
                .messages
                .iter()
                .find(|message| expected_message.eq(&message.title));
            assert!(
                was_expected.is_some(),
                "Expected diagnostic message not found: {}",
                expected_message
            );
        }
    }

    #[test]
    fn test_tokenize_simple_program() {
        let source = r#"
            let a = 200
            if a > 100 print("hello world")
        "#;

        assert_tokens(
            source,
            TokenDetailLevel::NoWhitespaceAndComments,
            vec![
                "Let",
                "Identifier(a)",
                "Assign",
                "Integer(200)",
                "If",
                "Identifier(a)",
                "ComparisonOp(Gt)",
                "Integer(100)",
                "Identifier(print)",
                "LeftParen",
                "String(hello world)",
                "RightParen",
            ],
            vec![],
        );
    }

    #[test]
    fn test_tokenize_identifiers() {
        let source = "foo Foo foo_bar FOO_BAR $123 _123";
        assert_tokens(
            source,
            TokenDetailLevel::NoWhitespaceAndComments,
            vec![
                "Identifier(foo)",
                "Identifier(Foo)",
                "Identifier(foo_bar)",
                "Identifier(FOO_BAR)",
                "Identifier($123)",
                "Identifier(_123)",
            ],
            vec![],
        );
    }

    #[test]
    fn test_tokenize_numbers() {
        let source = "123 0x123 0b1010 0o123 0.25 10.0 123.5 25.foo 25.25.25 25.25.foo";
        assert_tokens(
            source,
            TokenDetailLevel::NoWhitespaceAndComments,
            vec![
                "Integer(123)",
                "Integer(291)",
                "Integer(10)",
                "Integer(83)",
                "Float(0.25)",
                "Float(10)",
                "Float(123.5)",
                "Integer(25)",
                "Dot",
                "Identifier(foo)",
                "Float(25.25)",
                "Dot",
                "Integer(25)",
                "Float(25.25)",
                "Dot",
                "Identifier(foo)",
            ],
            vec![],
        );
    }

    #[test]
    fn test_tokenize_strings() {
        let source = r#"
            "hello world"
            ""
            "hello\nworld"
            "\n\r\t\"\\"
        "#;
        assert_tokens(
            source,
            TokenDetailLevel::NoWhitespaceAndComments,
            vec![
                r#"String(hello world)"#,
                "String()",
                r#"String(hello\nworld)"#,
                r#"String(\n\r\t\"\\)"#,
            ],
            vec![],
        );
    }

    #[test]
    fn test_tokenize_unnecessary_string_escapes() {
        let source = r#"
            "\a\b\c"
        "#;
        assert_tokens(
            source,
            TokenDetailLevel::NoWhitespaceAndComments,
            vec![r#"String(abc)"#],
            vec!["Unnecessary escape sequence".to_string()],
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
            TokenDetailLevel::NoWhitespaceAndComments,
            vec![
                "FormatStringPart(hello )",
                "Identifier(name)",
                "String(!)",
                "FormatStringPart()",
                "Identifier(foo)",
                "String()",
                "FormatStringPart()",
                "Identifier(foo)",
                "FormatStringPart( )",
                "Identifier(bar)",
                "String()",
                "FormatStringPart()",
                "Identifier(foo)",
                "FormatStringPart( )",
                "Identifier(bar)",
                "String()",
                "FormatStringPart()",
                "Identifier(foo)",
                "Operator(Add)",
                "Identifier(bar)",
                "String()",
                "FormatStringPart(foo)",
                "String(foo)",
                "String(foo)",
            ],
            vec![],
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
            TokenDetailLevel::NoWhitespace,
            vec![
                "SingleLineComment(single line comment)",
                "MultiLineComment(\n    multi\n    line\n    comment)",
                "Identifier(foo)",
                "SingleLineComment(single line comment)",
            ],
            vec![],
        );
    }

    #[test]
    fn test_tokenize_keywords_and_punctuators() {
        let source = r#"
            true false null super

            as assert await break builtin case catch class const continue default
            defer do else export extends final finally for from fn guard if import
            in instanceof let loop match private return spawn static switch throw
            try typeof unless until while

            + - * / % ** && || | & ^ << >> >>> ! ~
            == != > >= < <=
            =
            += -= *= /= %= **= |= &= ^= <<= >>= >>>=

            () {} [] . .. ... : ; , @ <- -> => ?
        "#;
        assert_tokens(
            source,
            TokenDetailLevel::NoWhitespaceAndComments,
            vec![
                "True",
                "False",
                "Null",
                "Super",
                "As",
                "Assert",
                "Await",
                "Break",
                "Builtin",
                "Case",
                "Catch",
                "Class",
                "Const",
                "Continue",
                "Default",
                "Defer",
                "Do",
                "Else",
                "Export",
                "Extends",
                "Final",
                "Finally",
                "For",
                "From",
                "Fn",
                "Guard",
                "If",
                "Import",
                "In",
                "Instanceof",
                "Let",
                "Loop",
                "Match",
                "Private",
                "Return",
                "Spawn",
                "Static",
                "Switch",
                "Throw",
                "Try",
                "Typeof",
                "Unless",
                "Until",
                "While",
                "Operator(Add)",
                "Operator(Sub)",
                "Operator(Mul)",
                "Operator(Div)",
                "Operator(Mod)",
                "Operator(Pow)",
                "Operator(And)",
                "Operator(Or)",
                "Operator(BitOr)",
                "Operator(BitAnd)",
                "Operator(BitXor)",
                "Operator(BitLeftShift)",
                "Operator(BitRightShift)",
                "Operator(BitUnsignedRightShift)",
                "Operator(Not)",
                "Operator(BitNot)",
                "ComparisonOp(Eq)",
                "ComparisonOp(Neq)",
                "ComparisonOp(Gt)",
                "ComparisonOp(Gte)",
                "ComparisonOp(Lt)",
                "ComparisonOp(Lte)",
                "Assign",
                "OperatorAssign(Add)",
                "OperatorAssign(Sub)",
                "OperatorAssign(Mul)",
                "OperatorAssign(Div)",
                "OperatorAssign(Mod)",
                "OperatorAssign(Pow)",
                "OperatorAssign(BitOr)",
                "OperatorAssign(BitAnd)",
                "OperatorAssign(BitXor)",
                "OperatorAssign(BitLeftShift)",
                "OperatorAssign(BitRightShift)",
                "OperatorAssign(BitUnsignedRightShift)",
                "LeftParen",
                "RightParen",
                "LeftBrace",
                "RightBrace",
                "LeftBracket",
                "RightBracket",
                "Dot",
                "DoubleDot",
                "TripleDot",
                "Colon",
                "Semicolon",
                "Comma",
                "AtSign",
                "LeftArrow",
                "RightArrow",
                "RightThickArrow",
                "QuestionMark",
            ],
            vec![],
        );
    }

    #[test]
    fn test_tokenize_whitespace() {
        let source = "    foo\n    bar\r\n    baz";
        assert_tokens(
            source,
            TokenDetailLevel::Full,
            vec![
                "Whitespace",
                "Identifier(foo)",
                "Newline",
                "Whitespace",
                "Identifier(bar)",
                "Newline",
                "Whitespace",
                "Identifier(baz)",
            ],
            vec![],
        );
    }

    #[test]
    fn test_unexpected_character() {
        let source = "ä";
        assert_tokens(
            source,
            TokenDetailLevel::Full,
            vec!["Error(UnexpectedCharacter('ä'))"],
            vec!["Unexpected 'ä' character".to_string()],
        );
    }

    #[test]
    fn test_unclosed_string_literal() {
        let source = "\"hello world";
        assert_tokens(
            source,
            TokenDetailLevel::Full,
            vec!["Error(UnclosedStringLiteral)"],
            vec!["Unclosed string literal".to_string()],
        );
    }
}
