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

use crate::charly::diagnostics::DiagnosticLocation;
use std::fmt::{Display, Formatter};
use std::num::{ParseFloatError, ParseIntError};

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub location: DiagnosticLocation,
    pub raw: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenError {
    UnexpectedCharacter(char),
    UnclosedStringLiteral,
    MalformedInteger(ParseIntError),
    MalformedFloat(ParseFloatError),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Error(TokenError),

    // literals
    Integer(i64),
    Float(f64),
    String(String),
    FormatStringPart(String),
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
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

    pub fn matching_bracket(&self) -> Option<TokenType> {
        match self {
            TokenType::LeftParen => Some(TokenType::RightParen),
            TokenType::LeftBrace => Some(TokenType::RightBrace),
            TokenType::LeftBracket => Some(TokenType::RightBracket),
            TokenType::RightParen => Some(TokenType::LeftParen),
            TokenType::RightBrace => Some(TokenType::LeftBrace),
            TokenType::RightBracket => Some(TokenType::LeftBracket),
            _ => None,
        }
    }

    pub fn to_raw(&self) -> String {
        match self {
            TokenType::Error(_) => "<error>",
            TokenType::Integer(value) => return format!("{}", value).as_str().to_string(),
            TokenType::Float(value) => return format!("{}", value).as_str().to_string(),
            TokenType::String(value) => return format!("\"{}\"", value).as_str().to_string(),
            TokenType::FormatStringPart(part) => part,
            TokenType::Identifier(value) => value,
            TokenType::True => "true",
            TokenType::False => "false",
            TokenType::Null => "null",
            TokenType::Super => "super",
            TokenType::As => "as",
            TokenType::Assert => "assert",
            TokenType::Await => "await",
            TokenType::Break => "break",
            TokenType::Builtin => "builtin",
            TokenType::Case => "case",
            TokenType::Catch => "catch",
            TokenType::Class => "class",
            TokenType::Const => "const",
            TokenType::Continue => "continue",
            TokenType::Default => "default",
            TokenType::Defer => "defer",
            TokenType::Do => "do",
            TokenType::Else => "else",
            TokenType::Export => "export",
            TokenType::Extends => "extends",
            TokenType::Final => "final",
            TokenType::Finally => "finally",
            TokenType::For => "for",
            TokenType::From => "from",
            TokenType::Fn => "fn",
            TokenType::Guard => "guard",
            TokenType::If => "if",
            TokenType::Import => "import",
            TokenType::In => "in",
            TokenType::Instanceof => "instanceof",
            TokenType::Let => "let",
            TokenType::Loop => "loop",
            TokenType::Match => "match",
            TokenType::Private => "private",
            TokenType::Return => "return",
            TokenType::Spawn => "spawn",
            TokenType::Static => "static",
            TokenType::Switch => "switch",
            TokenType::Throw => "throw",
            TokenType::Try => "try",
            TokenType::Typeof => "typeof",
            TokenType::Unless => "unless",
            TokenType::Until => "until",
            TokenType::While => "while",
            TokenType::Operator(operator) | TokenType::OperatorAssign(operator) => match operator {
                OperatorType::Add => "+",
                OperatorType::Sub => "-",
                OperatorType::Mul => "*",
                OperatorType::Div => "/",
                OperatorType::Mod => "%",
                OperatorType::Pow => "**",
                OperatorType::And => "&&",
                OperatorType::Or => "||",
                OperatorType::BitOr => "|",
                OperatorType::BitAnd => "&",
                OperatorType::BitXor => "^",
                OperatorType::BitLeftShift => "<<",
                OperatorType::BitRightShift => ">>",
                OperatorType::BitUnsignedRightShift => ">>>",
                OperatorType::Not => "!",
                OperatorType::BitNot => "~",
            },
            TokenType::ComparisonOp(operator) => match operator {
                ComparisonOperatorType::Eq => "==",
                ComparisonOperatorType::Neq => "!=",
                ComparisonOperatorType::Gt => ">",
                ComparisonOperatorType::Gte => ">=",
                ComparisonOperatorType::Lt => "<",
                ComparisonOperatorType::Lte => "<=",
            },
            TokenType::Assign => "=",
            TokenType::LeftParen => "(",
            TokenType::RightParen => ")",
            TokenType::LeftBrace => "{",
            TokenType::RightBrace => "}",
            TokenType::LeftBracket => "[",
            TokenType::RightBracket => "]",
            TokenType::Dot => ".",
            TokenType::DoubleDot => "..",
            TokenType::TripleDot => "...",
            TokenType::Colon => ":",
            TokenType::Semicolon => ";",
            TokenType::Comma => ",",
            TokenType::AtSign => "@",
            TokenType::LeftArrow => "<-",
            TokenType::RightArrow => "->",
            TokenType::RightThickArrow => "=>",
            TokenType::QuestionMark => "?",
            TokenType::Whitespace => " ",
            TokenType::Newline => "\n",
            _ => "foo",
        }
        .to_string()
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Error(error) => write!(f, "Error({:?})", error),
            TokenType::Integer(value) => write!(f, "Integer({})", value),
            TokenType::Float(value) => write!(f, "Float({})", value),
            TokenType::String(value) => {
                write!(f, "String({})", value.escape_debug())
            }
            TokenType::FormatStringPart(value) => {
                write!(f, "FormatStringPart({})", value.escape_debug())
            }
            TokenType::Identifier(value) => write!(f, "Identifier({})", value),
            TokenType::SingleLineComment(value) => {
                write!(f, "SingleLineComment({})", value)
            }
            TokenType::MultiLineComment(value) => {
                write!(f, "MultiLineComment({})", value)
            }
            _ => write!(f, "{:?}", self),
        }
    }
}
