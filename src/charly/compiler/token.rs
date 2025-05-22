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

use crate::charly::utils::diagnostics::DiagnosticLocation;
use std::fmt::{Display, Formatter};
use std::num::{ParseFloatError, ParseIntError};

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub location: DiagnosticLocation,
    pub raw: String,
    pub value: Option<TokenValue>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenValue {
    Integer(i64),
    Float(f64),
    String(String),
    Error(TokenError),
}

impl Token {
    pub fn integer_value(&self) -> i64 {
        match self.value {
            Some(TokenValue::Integer(value)) => value,
            _ => panic!("Token is not an integer"),
        }
    }

    pub fn float_value(&self) -> f64 {
        match self.value {
            Some(TokenValue::Float(value)) => value,
            _ => panic!("Token is not a float"),
        }
    }

    pub fn string_value(&self) -> &String {
        match &self.value {
            Some(TokenValue::String(value)) => value,
            _ => panic!("Token is not a string"),
        }
    }

    pub fn error_value(&self) -> &TokenError {
        match &self.value {
            Some(TokenValue::Error(value)) => value,
            _ => panic!("Token is not an error"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenError {
    UnexpectedCharacter(char),
    UnclosedStringLiteral,
    MalformedInteger(ParseIntError),
    MalformedFloat(ParseFloatError),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Error,
    Eof,

    // literals
    Integer,
    Float,
    String,
    FormatStringPart,
    Identifier,

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
    Fn,
    For,
    From,
    Guard,
    If,
    Import,
    In,
    Instanceof,
    Let,
    Loop,
    Match,
    Module,
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
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    And,
    Or,
    BitOr,
    BitAnd,
    BitXor,
    BitLeftShift,
    BitRightShift,
    BitUnsignedRightShift,
    Not,
    BitNot,

    // comparison operators
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,

    // assign operators
    Assign,
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignMod,
    AssignPow,
    AssignBitOr,
    AssignBitAnd,
    AssignBitXor,
    AssignBitLeftShift,
    AssignBitRightShift,
    AssignBitUnsignedRightShift,

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
    SingleLineComment,
    MultiLineComment,
}

pub static TOKEN_TEXT_MAPPING: &[(&str, TokenKind)] = &[
    ("as", TokenKind::As),
    ("assert", TokenKind::Assert),
    ("await", TokenKind::Await),
    ("break", TokenKind::Break),
    ("builtin", TokenKind::Builtin),
    ("case", TokenKind::Case),
    ("catch", TokenKind::Catch),
    ("class", TokenKind::Class),
    ("const", TokenKind::Const),
    ("continue", TokenKind::Continue),
    ("default", TokenKind::Default),
    ("defer", TokenKind::Defer),
    ("do", TokenKind::Do),
    ("else", TokenKind::Else),
    ("export", TokenKind::Export),
    ("extends", TokenKind::Extends),
    ("false", TokenKind::False),
    ("final", TokenKind::Final),
    ("finally", TokenKind::Finally),
    ("fn", TokenKind::Fn),
    ("for", TokenKind::For),
    ("from", TokenKind::From),
    ("guard", TokenKind::Guard),
    ("if", TokenKind::If),
    ("import", TokenKind::Import),
    ("in", TokenKind::In),
    ("instanceof", TokenKind::Instanceof),
    ("let", TokenKind::Let),
    ("loop", TokenKind::Loop),
    ("match", TokenKind::Match),
    ("module", TokenKind::Module),
    ("null", TokenKind::Null),
    ("private", TokenKind::Private),
    ("return", TokenKind::Return),
    ("spawn", TokenKind::Spawn),
    ("static", TokenKind::Static),
    ("super", TokenKind::Super),
    ("switch", TokenKind::Switch),
    ("throw", TokenKind::Throw),
    ("true", TokenKind::True),
    ("try", TokenKind::Try),
    ("typeof", TokenKind::Typeof),
    ("unless", TokenKind::Unless),
    ("until", TokenKind::Until),
    ("while", TokenKind::While),
    ("+", TokenKind::Add),
    ("-", TokenKind::Sub),
    ("*", TokenKind::Mul),
    ("/", TokenKind::Div),
    ("%", TokenKind::Mod),
    ("**", TokenKind::Pow),
    ("&&", TokenKind::And),
    ("||", TokenKind::Or),
    ("|", TokenKind::BitOr),
    ("&", TokenKind::BitAnd),
    ("^", TokenKind::BitXor),
    ("<<", TokenKind::BitLeftShift),
    (">>", TokenKind::BitRightShift),
    (">>>", TokenKind::BitUnsignedRightShift),
    ("!", TokenKind::Not),
    ("~", TokenKind::BitNot),
    ("==", TokenKind::Eq),
    ("!=", TokenKind::Neq),
    (">", TokenKind::Gt),
    (">=", TokenKind::Gte),
    ("<", TokenKind::Lt),
    ("<=", TokenKind::Lte),
    ("=", TokenKind::Assign),
    ("+=", TokenKind::AssignAdd),
    ("-=", TokenKind::AssignSub),
    ("*=", TokenKind::AssignMul),
    ("/=", TokenKind::AssignDiv),
    ("%=", TokenKind::AssignMod),
    ("**=", TokenKind::AssignPow),
    ("|=", TokenKind::AssignBitOr),
    ("&=", TokenKind::AssignBitAnd),
    ("^=", TokenKind::AssignBitXor),
    ("<<=", TokenKind::AssignBitLeftShift),
    (">>=", TokenKind::AssignBitRightShift),
    (">>>=", TokenKind::AssignBitUnsignedRightShift),
    ("(", TokenKind::LeftParen),
    (")", TokenKind::RightParen),
    ("{", TokenKind::LeftBrace),
    ("}", TokenKind::RightBrace),
    ("[", TokenKind::LeftBracket),
    ("]", TokenKind::RightBracket),
    (".", TokenKind::Dot),
    ("..", TokenKind::DoubleDot),
    ("...", TokenKind::TripleDot),
    (":", TokenKind::Colon),
    (";", TokenKind::Semicolon),
    (",", TokenKind::Comma),
    ("@", TokenKind::AtSign),
    ("<-", TokenKind::LeftArrow),
    ("->", TokenKind::RightArrow),
    ("=>", TokenKind::RightThickArrow),
    ("?", TokenKind::QuestionMark),
];

pub static TOKEN_ASSIGN_OPERATOR_MAPPING: &[(TokenKind, TokenKind)] = &[
    (TokenKind::AssignAdd, TokenKind::Add),
    (TokenKind::AssignSub, TokenKind::Sub),
    (TokenKind::AssignMul, TokenKind::Mul),
    (TokenKind::AssignDiv, TokenKind::Div),
    (TokenKind::AssignMod, TokenKind::Mod),
    (TokenKind::AssignPow, TokenKind::Pow),
    (TokenKind::AssignBitOr, TokenKind::BitOr),
    (TokenKind::AssignBitAnd, TokenKind::BitAnd),
    (TokenKind::AssignBitXor, TokenKind::BitXor),
    (TokenKind::AssignBitLeftShift, TokenKind::BitLeftShift),
    (TokenKind::AssignBitRightShift, TokenKind::BitRightShift),
    (
        TokenKind::AssignBitUnsignedRightShift,
        TokenKind::BitUnsignedRightShift,
    ),
];

pub static TOKEN_BRACE_PAIR_MAPPING: &[(TokenKind, TokenKind)] = &[
    (TokenKind::LeftBrace, TokenKind::RightBrace),
    (TokenKind::LeftBracket, TokenKind::RightBracket),
    (TokenKind::LeftParen, TokenKind::RightParen),
];

pub fn try_token(value: &str) -> Option<TokenKind> {
    for (raw, kind) in TOKEN_TEXT_MAPPING {
        if *raw == value {
            return Some(*kind);
        }
    }

    None
}

pub fn try_keyword_or_identifier(value: &str) -> TokenKind {
    let kind = try_token(value);
    kind.unwrap_or(TokenKind::Identifier)
}

impl TokenKind {
    pub fn matching_bracket(&self) -> Option<TokenKind> {
        for (left, right) in TOKEN_BRACE_PAIR_MAPPING {
            if left == self {
                return Some(*right);
            } else if right == self {
                return Some(*left);
            }
        }

        None
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            TokenKind::Error => {
                let error_description = match self.error_value() {
                    TokenError::UnexpectedCharacter(_) => "UnexpectedCharacter",
                    TokenError::UnclosedStringLiteral => "UnclosedStringLiteral",
                    TokenError::MalformedInteger(_) => "MalformedInteger",
                    TokenError::MalformedFloat(_) => "MalformedFloat",
                };
                write!(f, "Error({})", error_description)
            }
            TokenKind::Integer => {
                write!(f, "Integer({})", self.integer_value())
            }
            TokenKind::Float => {
                write!(f, "Float({})", self.float_value())
            }
            TokenKind::String => {
                write!(f, "String({})", self.string_value().escape_debug())
            }
            TokenKind::FormatStringPart => {
                write!(
                    f,
                    "FormatStringPart({})",
                    self.string_value().escape_debug()
                )
            }
            TokenKind::Identifier => {
                write!(f, "Identifier({})", self.string_value())
            }
            TokenKind::SingleLineComment => {
                write!(f, "SingleLineComment({})", self.string_value())
            }
            TokenKind::MultiLineComment => {
                write!(f, "MultiLineComment({})", self.string_value())
            }
            _ => {
                write!(f, "{:?}", self.kind)
            }
        }
    }
}
