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

use crate::charly::window_buffer::TextSpan;
use std::fmt::{Display, Formatter};
use std::num::{ParseFloatError, ParseIntError};

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub span: TextSpan,
    pub raw: String,
}

#[derive(Debug, PartialEq)]
pub enum TokenError {
    UnexpectedCharacter(char),
    UnclosedStringLiteral,
    MalformedInteger(ParseIntError),
    MalformedFloat(ParseFloatError),
}

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Error(TokenError),

    // literals
    Integer(i64),
    Float(f64),
    String(String),
    FormatStringComponent(String),
    Identifier(String),

    // literal keywords
    True,
    False,
    Null,
    Super,

    // language keywords
    As,
    Assert,
    Await,
    Break,
    Builtin,
    Case,
    Catch,
    Class,
    Const,
    Continue,
    Default,
    Defer,
    Do,
    Else,
    Export,
    Extends,
    Final,
    Finally,
    For,
    From,
    Fn,
    Guard,
    If,
    Import,
    In,
    Instanceof,
    Let,
    Loop,
    Match,
    Private,
    Return,
    Spawn,
    Static,
    Switch,
    Throw,
    Try,
    Typeof,
    Unless,
    Until,
    While,

    // operators
    Operator(OperatorType),
    ComparisonOp(ComparisonOperatorType),
    Assign,
    OperatorAssign(OperatorType),

    // punctuation
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Dot,
    DoubleDot,
    TripleDot,
    Colon,
    Semicolon,
    Comma,
    AtSign,
    LeftArrow,
    RightArrow,
    RightThickArrow,
    QuestionMark,

    // whitespace
    Whitespace,
    Newline,
    SingleLineComment(String),
    MultiLineComment(String),
}

#[derive(Debug, PartialEq)]
pub enum OperatorType {
    // binary operators
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    And,
    Or,

    // bitwise binary operators
    BitOr,
    BitAnd,
    BitXor,
    BitLeftShift,
    BitRightShift,
    BitUnsignedRightShift,

    // unary operators
    Not,
    BitNot,
}

#[derive(Debug, PartialEq)]
pub enum ComparisonOperatorType {
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,
}

impl TokenType {
    pub fn try_token(value: Option<&str>) -> Option<TokenType> {
        match value? {
            "+" => Some(TokenType::Operator(OperatorType::Add)),
            "-" => Some(TokenType::Operator(OperatorType::Sub)),
            "*" => Some(TokenType::Operator(OperatorType::Mul)),
            "/" => Some(TokenType::Operator(OperatorType::Div)),
            "%" => Some(TokenType::Operator(OperatorType::Mod)),
            "|" => Some(TokenType::Operator(OperatorType::BitOr)),
            "&" => Some(TokenType::Operator(OperatorType::BitAnd)),
            "^" => Some(TokenType::Operator(OperatorType::BitXor)),
            "!" => Some(TokenType::Operator(OperatorType::Not)),
            "~" => Some(TokenType::Operator(OperatorType::BitNot)),
            "=" => Some(TokenType::Assign),
            "(" => Some(TokenType::LeftParen),
            ")" => Some(TokenType::RightParen),
            "{" => Some(TokenType::LeftBrace),
            "}" => Some(TokenType::RightBrace),
            "[" => Some(TokenType::LeftBracket),
            "]" => Some(TokenType::RightBracket),
            "." => Some(TokenType::Dot),
            ":" => Some(TokenType::Colon),
            ";" => Some(TokenType::Semicolon),
            "," => Some(TokenType::Comma),
            "@" => Some(TokenType::AtSign),
            "?" => Some(TokenType::QuestionMark),
            ">" => Some(TokenType::ComparisonOp(ComparisonOperatorType::Gt)),
            "<" => Some(TokenType::ComparisonOp(ComparisonOperatorType::Lt)),

            "**" => Some(TokenType::Operator(OperatorType::Pow)),
            "&&" => Some(TokenType::Operator(OperatorType::And)),
            "||" => Some(TokenType::Operator(OperatorType::Or)),
            "<<" => Some(TokenType::Operator(OperatorType::BitLeftShift)),
            ">>" => Some(TokenType::Operator(OperatorType::BitRightShift)),
            "==" => Some(TokenType::ComparisonOp(ComparisonOperatorType::Eq)),
            "!=" => Some(TokenType::ComparisonOp(ComparisonOperatorType::Neq)),
            ">=" => Some(TokenType::ComparisonOp(ComparisonOperatorType::Gte)),
            "<=" => Some(TokenType::ComparisonOp(ComparisonOperatorType::Lte)),
            ".." => Some(TokenType::DoubleDot),
            "<-" => Some(TokenType::LeftArrow),
            "->" => Some(TokenType::RightArrow),
            "=>" => Some(TokenType::RightThickArrow),
            "+=" => Some(TokenType::OperatorAssign(OperatorType::Add)),
            "-=" => Some(TokenType::OperatorAssign(OperatorType::Sub)),
            "*=" => Some(TokenType::OperatorAssign(OperatorType::Mul)),
            "/=" => Some(TokenType::OperatorAssign(OperatorType::Div)),
            "%=" => Some(TokenType::OperatorAssign(OperatorType::Mod)),
            "|=" => Some(TokenType::OperatorAssign(OperatorType::BitOr)),
            "&=" => Some(TokenType::OperatorAssign(OperatorType::BitAnd)),
            "^=" => Some(TokenType::OperatorAssign(OperatorType::BitXor)),

            "..." => Some(TokenType::TripleDot),
            ">>>" => Some(TokenType::Operator(OperatorType::BitUnsignedRightShift)),
            "**=" => Some(TokenType::OperatorAssign(OperatorType::Pow)),
            "<<=" => Some(TokenType::OperatorAssign(OperatorType::BitLeftShift)),
            ">>=" => Some(TokenType::OperatorAssign(OperatorType::BitRightShift)),

            ">>>=" => Some(TokenType::OperatorAssign(
                OperatorType::BitUnsignedRightShift,
            )),
            _ => None,
        }
    }

    pub fn try_keyword_or_identifier(value: &str) -> TokenType {
        if let Some(keyword_type) = Self::try_keyword(value) {
            keyword_type
        } else {
            TokenType::Identifier(value.to_string())
        }
    }

    pub fn try_keyword(value: &str) -> Option<TokenType> {
        let keyword_type = match value {
            "true" => TokenType::True,
            "false" => TokenType::False,
            "null" => TokenType::Null,
            "super" => TokenType::Super,
            "as" => TokenType::As,
            "assert" => TokenType::Assert,
            "await" => TokenType::Await,
            "break" => TokenType::Break,
            "builtin" => TokenType::Builtin,
            "case" => TokenType::Case,
            "catch" => TokenType::Catch,
            "class" => TokenType::Class,
            "const" => TokenType::Const,
            "continue" => TokenType::Continue,
            "default" => TokenType::Default,
            "defer" => TokenType::Defer,
            "do" => TokenType::Do,
            "else" => TokenType::Else,
            "export" => TokenType::Export,
            "extends" => TokenType::Extends,
            "final" => TokenType::Final,
            "finally" => TokenType::Finally,
            "for" => TokenType::For,
            "from" => TokenType::From,
            "fn" => TokenType::Fn,
            "guard" => TokenType::Guard,
            "if" => TokenType::If,
            "import" => TokenType::Import,
            "in" => TokenType::In,
            "instanceof" => TokenType::Instanceof,
            "let" => TokenType::Let,
            "loop" => TokenType::Loop,
            "match" => TokenType::Match,
            "private" => TokenType::Private,
            "return" => TokenType::Return,
            "spawn" => TokenType::Spawn,
            "static" => TokenType::Static,
            "switch" => TokenType::Switch,
            "throw" => TokenType::Throw,
            "try" => TokenType::Try,
            "typeof" => TokenType::Typeof,
            "unless" => TokenType::Unless,
            "until" => TokenType::Until,
            "while" => TokenType::While,
            _ => return None,
        };

        Some(keyword_type)
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Error(error) => write!(f, "Error({:?})", error),
            TokenType::Integer(value) => write!(f, "Integer({})", value),
            TokenType::Float(value) => write!(f, "Float({})", value),
            TokenType::String(value) => write!(f, "String({})", value.escape_debug()),
            TokenType::FormatStringComponent(value) => {
                write!(f, "FormatStringComponent({})", value.escape_debug())
            }
            TokenType::Identifier(value) => write!(f, "Identifier({})", value),
            TokenType::SingleLineComment(value) => write!(f, "SingleLineComment({})", value),
            TokenType::MultiLineComment(value) => write!(f, "MultiLineComment({})", value),
            _ => write!(f, "{:?}", self),
        }
    }
}
