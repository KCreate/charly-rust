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

use std::str::FromStr;
use crate::charly::token::{Token, TokenType};
use crate::charly::token::TokenError::MalformedFloat;
use crate::charly::token::TokenError::MalformedInteger;
use crate::charly::token::TokenError::UnclosedStringLiteral;
use crate::charly::token::TokenError::UnexpectedCharacter;
use crate::charly::window_buffer::WindowBuffer;

pub struct Tokenizer {
    buffer: WindowBuffer,
}

impl Iterator for Tokenizer {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        self.read_token()
    }
}

impl Tokenizer {
    pub fn new(source: &str) -> Self {
        Self { buffer: WindowBuffer::from(source), }
    }

    pub fn iter_stripped(&mut self) -> impl Iterator<Item = Token> {
        self.filter_map(|t| match t.token_type {
            TokenType::Whitespace | TokenType::Newline => None,
            _ => Some(t),
        })
    }

    pub fn read_token(&mut self) -> Option<Token> {
        self.buffer.reset_window();

        let char = self.buffer.peek_char()?;

        /*
            TODO:
            - format strings "$variable" and "${<expression>}"
            - error reporting
        */
        let token = match char {
            c if c.is_whitespace() => Some(self.consume_whitespace()),
            c if c.is_identifier_begin() => Some(self.consume_identifier()),
            c if c.is_decimal_digit() => Some(self.consume_number()),

            // string literals
            '\"' => Some(self.consume_string()),

            // comments
            '/' => match self.buffer.peek_char_with_lookahead(1) {
                Some('/') => Some(self.consume_comment()),
                Some('*') => Some(self.consume_multiline_comment()),
                _ => None,
            },

            _ => None
        };

        if token.is_some() {
            return token;
        }

        // recognize known operators and punctuators
        for n in (1..=4).rev() {
            let lookahead = self.buffer.peek_str(n);
            match TokenType::try_token(lookahead) {
                Some(token_type) => {
                    self.buffer.advance(n);
                    return Some(self.build_token(token_type));
                }
                None => {}
            }
        }

        self.buffer.advance(1);
        Some(self.build_token(TokenType::Error(UnexpectedCharacter(char))))
    }

    fn build_token(&mut self, token_type: TokenType) -> Token {
        Token {
            token_type,
            span: self.buffer.window_span.clone(),
            raw: self.buffer.window_as_str().to_string()
        }
    }

    fn consume_whitespace(&mut self) -> Token {
        let char = self.buffer.read_char().unwrap();
        assert!(char.is_whitespace());

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

            // whitespace
            _ => self.build_token(TokenType::Whitespace),
        }
    }

    fn consume_string(&mut self) -> Token {
        assert_eq!(self.buffer.read_char().unwrap(), '\"');

        let mut content = String::new();

        loop {
            match self.buffer.read_char() {
                Some(c) if c == '\"' => {
                    break;
                }
                Some(c) if c == '\\' => {
                    match self.buffer.read_char() {
                        Some('n') => content.push('\n'),
                        Some('r') => content.push('\r'),
                        Some('t') => content.push('\t'),
                        Some('"') => content.push('\"'),
                        Some('\\') => content.push('\\'),
                        Some(c) => {
                            // unnecessary escape sequence
                            content.push(c);
                        },
                        None => {
                            // unclosed string literal
                            return self.build_token(TokenType::Error(UnclosedStringLiteral));
                        }
                    }
                }
                Some(c) => content.push(c),
                None => {
                    // unclosed string literal
                    return self.build_token(TokenType::Error(UnclosedStringLiteral));
                }
            }
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
            _ => self.consume_decimal()
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
                        _ => break
                    }
                }
                _ => break,
            }
        }

        if found_decimal_point {
            let window_text = self.buffer.window_as_str();
            let value = f64::from_str(window_text);

            match value {
                Ok(value) => {
                    self.build_token(TokenType::Float(value))
                }
                Err(err) => self.build_token(TokenType::Error(MalformedFloat(err))),
            }
        } else {
            let window_text = self.buffer.window_as_str();
            let value = i64::from_str_radix(window_text, 10);

            match value {
                Ok(value) => {
                    self.build_token(TokenType::Integer(value))
                }
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
    use crate::charly::tokenizer::Tokenizer;

    fn assert_tokens(source: &str, expected_tokens: Vec<&str>) {
        let mut tokenizer = Tokenizer::new(source);
        let iter = tokenizer.iter_stripped();
        let mut iter = iter.map(|t| format!("{}", t.token_type));

        for expected in expected_tokens {
            assert_eq!(iter.next().unwrap(), expected);
        }
        assert!(iter.next().is_none());
    }

    #[test]
    fn tokenize_simple_program() {
        let source = r#"
            let a = 200
            if a > 100 print("hello world")
        "#;

        assert_tokens(source, vec![
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
        ]);
    }

    #[test]
    fn tokenize_identifiers() {
        let source = "foo Foo foo_bar FOO_BAR $123 _123";
        assert_tokens(source, vec![
            "Identifier(foo)",
            "Identifier(Foo)",
            "Identifier(foo_bar)",
            "Identifier(FOO_BAR)",
            "Identifier($123)",
            "Identifier(_123)",
        ]);
    }

    #[test]
    fn tokenize_numbers() {
        let source = "123 0x123 0b1010 0o123 0.25 10.0 123.5 25.foo 25.25.25 25.25.foo";
        assert_tokens(source, vec![
            "Integer(123)",
            "Integer(291)",
            "Integer(10)",
            "Integer(83)",
            "Float(0.25)",
            "Float(10)",
            "Float(123.5)",
            "Integer(25)", "Dot", "Identifier(foo)",
            "Float(25.25)", "Dot", "Integer(25)",
            "Float(25.25)", "Dot", "Identifier(foo)",
        ]);
    }

    #[test]
    fn tokenize_strings() {
        let source = r#"
            "hello world"
            ""
            "hello\nworld"
            "\n\r\t\"\\"
            "\a\b\c"
        "#;
        assert_tokens(source, vec![
            r#"String(hello world)"#,
            "String()",
            r#"String(hello\nworld)"#,
            r#"String(\n\r\t\"\\)"#,
            r#"String(abc)"#,
        ]);
    }

    #[test]
    fn tokenize_comments() {
        let source = r#"
// single line comment
/*
    multi
    line
    comment
*/
foo // single line comment
        "#;
        assert_tokens(source, vec![
            "SingleLineComment(single line comment)",
            "MultiLineComment(\n    multi\n    line\n    comment)",
            "Identifier(foo)",
            "SingleLineComment(single line comment)",
        ]);
    }

    #[test]
    fn tokenize_keywords_and_punctuators() {
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
        assert_tokens(source, vec![
            "True", "False", "Null", "Super",

            "As", "Assert", "Await", "Break", "Builtin", "Case", "Catch", "Class", "Const",
            "Continue", "Default", "Defer", "Do", "Else", "Export", "Extends", "Final",
            "Finally", "For", "From", "Fn", "Guard", "If", "Import", "In", "Instanceof",
            "Let", "Loop", "Match", "Private", "Return", "Spawn", "Static", "Switch",
            "Throw", "Try", "Typeof", "Unless", "Until", "While",

            "Operator(Add)", "Operator(Sub)", "Operator(Mul)", "Operator(Div)",
            "Operator(Mod)", "Operator(Pow)", "Operator(And)", "Operator(Or)",
            "Operator(BitOr)", "Operator(BitAnd)", "Operator(BitXor)", "Operator(BitLeftShift)",
            "Operator(BitRightShift)", "Operator(BitUnsignedRightShift)",
            "Operator(Not)", "Operator(BitNot)",

            "ComparisonOp(Eq)", "ComparisonOp(Neq)", "ComparisonOp(Gt)", "ComparisonOp(Gte)",
            "ComparisonOp(Lt)", "ComparisonOp(Lte)",
            "Assign", "OperatorAssign(Add)", "OperatorAssign(Sub)", "OperatorAssign(Mul)",
            "OperatorAssign(Div)", "OperatorAssign(Mod)", "OperatorAssign(Pow)",
            "OperatorAssign(BitOr)", "OperatorAssign(BitAnd)", "OperatorAssign(BitXor)",
            "OperatorAssign(BitLeftShift)", "OperatorAssign(BitRightShift)",
            "OperatorAssign(BitUnsignedRightShift)",

            "LeftParen", "RightParen", "LeftBrace", "RightBrace", "LeftBracket", "RightBracket",
            "Dot", "DoubleDot", "TripleDot", "Colon", "Semicolon", "Comma", "AtSign", "LeftArrow",
            "RightArrow", "RightThickArrow", "QuestionMark",
        ]);
    }
}
