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

use crate::charly::compiler::tokenset::TokenSet;
use crate::charly::utils::diagnostics::DiagnosticLocation;
use casey::lower;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub location: DiagnosticLocation,
    pub raw: String,
    pub value: Option<TokenValue>,
}

pub type NumberSuffix = String;

#[derive(Debug, Clone)]
pub enum TokenValue {
    IntegerValue(String, IntegerBaseSpecifier, Option<NumberSuffix>),
    FloatValue(String, Option<NumberSuffix>),
    TextValue(String),
    CharacterValue(char),
}

#[derive(Debug, Clone)]
pub enum IntegerBaseSpecifier {
    Hexadecimal,
    Decimal,
    Octal,
    Binary,
}

impl IntegerBaseSpecifier {
    fn radix(&self) -> u32 {
        match self {
            IntegerBaseSpecifier::Hexadecimal => 16,
            IntegerBaseSpecifier::Decimal => 10,
            IntegerBaseSpecifier::Octal => 8,
            IntegerBaseSpecifier::Binary => 2,
        }
    }
}

macro_rules! max_str_len {
    () => { 0usize };
    ($s:expr $(,)?) => { $s.len() };
    ($s:expr, $($rest:expr),+ $(,)?) => {{
        const FIRST: usize = $s.len();
        const REST: usize = max_str_len!($($rest),*);
        if FIRST > REST { FIRST } else { REST }
    }};
}

macro_rules! define_tokens {
    (
        bare: {
            $( $bare_variant:ident ),* $(,)?
        },
        keywords: {
            $( $kw_variant:ident ),* $(,)?
        },
        punctuators: {
            $( $punct_variant:ident => $punct_text:literal ),* $(,)?
        },
    ) => {
        #[derive(Eq, Hash, Debug, PartialEq, Clone, Copy)]
        #[repr(u16)]
        pub enum TokenKind {
            $( $bare_variant, )*
            $( $kw_variant, )*
            $( $punct_variant, )*
            __COUNT
        }

        pub static TOKEN_LIST_ALL: &[TokenKind] = &[
            $( TokenKind::$bare_variant, )*
            $( TokenKind::$kw_variant, )*
            $( TokenKind::$punct_variant, )*
        ];

        pub static TOKEN_ALL: TokenSet = TokenSet::from_kinds(&[
            $( TokenKind::$bare_variant, )*
            $( TokenKind::$kw_variant, )*
            $( TokenKind::$punct_variant, )*
        ]);

        pub static TOKEN_BARE: TokenSet = TokenSet::from_kinds(&[
            $( TokenKind::$kw_variant, )*
        ]);

        pub static TOKEN_KEYWORDS: TokenSet = TokenSet::from_kinds(&[
            $( TokenKind::$kw_variant, )*
        ]);

        pub static TOKEN_PUNCTUATORS: TokenSet = TokenSet::from_kinds(&[
            $( TokenKind::$punct_variant, )*
        ]);

        pub static TOKEN_MAX_STR_LEN: usize = max_str_len!(
            $( $punct_text, )*
        );

        impl TokenKind {
            pub const TOKEN_KIND_COUNT: usize = TokenKind::__COUNT as usize;

            pub fn try_from_str(text: &str) -> Option<Self> {
                match text {
                    $(
                        $punct_text => Some(TokenKind::$punct_variant),
                    )*
                    $(
                        lower!(stringify!($kw_variant)) => Some(TokenKind::$kw_variant),
                    )*
                    _ => None,
                }
            }

            pub fn to_string(&self) -> &'static str {
                match self {
                    $(
                        TokenKind::$punct_variant => $punct_text,
                    )*
                    $(
                        TokenKind::$kw_variant => lower!(stringify!($kw_variant)),
                    )*
                    $(
                        TokenKind::$bare_variant => lower!(stringify!($bare_variant)),
                    )*
                    TokenKind::__COUNT => unreachable!()
                }
            }

            pub fn is_bare(&self) -> bool {
                match self {
                    $(
                        TokenKind::$bare_variant => true,
                    )*
                    _ => false,
                }
            }

            pub fn is_keyword(&self) -> bool {
                match self {
                    $(
                        TokenKind::$kw_variant => true,
                    )*
                    _ => false,
                }
            }

            pub fn is_punctuator(&self) -> bool {
                match self {
                    $(
                        TokenKind::$punct_variant => true,
                    )*
                    _ => false,
                }
            }
        }
    }
}

define_tokens! {
    bare: {
        EndOfFile,
        Error,
        Whitespace,
        Newline,
        SingleLineComment,
        MultiLineComment,
        DocComment,
        Identifier,
        Integer,
        Float,
        Character,
        StringStart,
        StringEnd,
        StringText,
        StringExprStart,
        StringExprEnd,
    },
    keywords: {
        As,
        Assert,
        Await,
        Break,
        Const,
        Continue,
        Defer,
        Else,
        Enum,
        Export,
        Extends,
        False,
        Final,
        Flags,
        Fn,
        For,
        From,
        If,
        Impl,
        Import,
        In,
        Interface,
        Internal,
        Is,
        Let,
        Loop,
        Match,
        Native,
        Null,
        Operator,
        Private,
        Public,
        Return,
        Select,
        Spawn,
        Static,
        Struct,
        Super,
        True,
        Typealias,
        Union,
        When,
        While,
    },
    punctuators: {
        Add => "+",
        Sub => "-",
        Mul => "*",
        Div => "/",
        Mod => "%",
        Pow => "**",
        And => "&&",
        DoublePipe => "||",
        Pipe => "|",
        BitAnd => "&",
        BitXor => "^",
        BitLeftShift => "<<",
        BitRightShift => ">>",
        BitUnsignedRightShift => ">>>",
        Not => "!",
        DoubleNot => "!!",
        BitNot => "~",
        Eq => "==",
        Neq => "!=",
        Gt => ">",
        Gte => ">=",
        Lt => "<",
        Lte => "<=",
        Assign => "=",
        AssignAdd => "+=",
        AssignSub => "-=",
        AssignMul => "*=",
        AssignDiv => "/=",
        AssignMod => "%=",
        AssignPow => "**=",
        AssignBitOr => "|=",
        AssignBitAnd => "&=",
        AssignBitXor => "^=",
        AssignBitLeftShift => "<<=",
        AssignBitRightShift => ">>=",
        AssignBitUnsignedRightShift => ">>>=",
        LeftParen => "(",
        RightParen => ")",
        LeftBrace => "{",
        RightBrace => "}",
        LeftBracket => "[",
        RightBracket => "]",
        Dot => ".",
        DoubleDot => "..",
        TripleDot => "...",
        Colon => ":",
        Semicolon => ";",
        Comma => ",",
        AtSign => "@",
        LeftArrow => "<-",
        RightArrow => "->",
        RightThickArrow => "=>",
        QuestionMark => "?",
        QuestionMarkDot => "?.",
        QuestionMarkColon => "?:",
        RangeExclusive => "..<",
    },
}

pub const TOKEN_INFIX_OPERATORS: TokenSet = TokenSet::from_kinds(&[
    TokenKind::DoublePipe,
    TokenKind::And,
    TokenKind::Pipe,
    TokenKind::BitXor,
    TokenKind::BitAnd,
    TokenKind::Eq,
    TokenKind::Neq,
    TokenKind::Gt,
    TokenKind::Gte,
    TokenKind::Lt,
    TokenKind::Lte,
    TokenKind::BitLeftShift,
    TokenKind::BitRightShift,
    TokenKind::BitUnsignedRightShift,
    TokenKind::Add,
    TokenKind::Sub,
    TokenKind::Mul,
    TokenKind::Div,
    TokenKind::Mod,
    TokenKind::Pow,
    TokenKind::Is,
    TokenKind::In,
    TokenKind::QuestionMarkColon,
    TokenKind::RangeExclusive,
    TokenKind::DoubleDot,
]);

pub const TOKEN_PREFIX_OPERATORS: TokenSet = TokenSet::from_kinds(&[
    TokenKind::Await,
    TokenKind::Add,
    TokenKind::Sub,
    TokenKind::Not,
    TokenKind::BitNot,
    TokenKind::TripleDot,
]);

pub const TOKEN_POSTFIX_OPERATORS: TokenSet =
    TokenSet::from_kinds(&[TokenKind::DoubleNot]);

pub const TOKEN_ASSIGN_INFIX_OPERATORS: TokenSet = TokenSet::from_kinds(&[
    TokenKind::AssignAdd,
    TokenKind::AssignSub,
    TokenKind::AssignMul,
    TokenKind::AssignDiv,
    TokenKind::AssignMod,
    TokenKind::AssignPow,
    TokenKind::AssignBitOr,
    TokenKind::AssignBitAnd,
    TokenKind::AssignBitXor,
    TokenKind::AssignBitLeftShift,
    TokenKind::AssignBitRightShift,
    TokenKind::AssignBitUnsignedRightShift,
]);

pub const TOKEN_OPENING_BRACKET: TokenSet = TokenSet::from_kinds(&[
    TokenKind::LeftBrace,
    TokenKind::LeftParen,
    TokenKind::LeftBracket,
]);

pub const TOKEN_CLOSING_BRACKET: TokenSet = TokenSet::from_kinds(&[
    TokenKind::RightBrace,
    TokenKind::RightParen,
    TokenKind::RightBracket,
]);

pub const TOKEN_WHITESPACE: TokenSet =
    TokenSet::from_kinds(&[TokenKind::Whitespace, TokenKind::Newline]);

pub const TOKEN_COMMENT: TokenSet = TokenSet::from_kinds(&[
    TokenKind::SingleLineComment,
    TokenKind::MultiLineComment,
    TokenKind::DocComment,
]);

impl TokenKind {
    pub fn is_infix_operator(&self) -> bool {
        TOKEN_INFIX_OPERATORS.has(*self)
    }

    //noinspection RsUnnecessaryQualifications
    pub fn is_prefix_operator(&self) -> bool {
        TOKEN_PREFIX_OPERATORS.has(*self)
    }

    //noinspection RsUnnecessaryQualifications
    pub fn is_postfix_operator(&self) -> bool {
        TOKEN_POSTFIX_OPERATORS.has(*self)
    }

    pub fn is_assign_infix_operator(&self) -> bool {
        TOKEN_ASSIGN_INFIX_OPERATORS.has(*self)
    }

    pub fn is_opening_bracket(&self) -> bool {
        TOKEN_OPENING_BRACKET.has(*self)
    }

    pub fn is_closing_bracket(&self) -> bool {
        TOKEN_CLOSING_BRACKET.has(*self)
    }

    pub fn is_whitespace(&self) -> bool {
        TOKEN_WHITESPACE.has(*self)
    }

    pub fn is_comment(&self) -> bool {
        TOKEN_COMMENT.has(*self)
    }

    pub fn matching_infix_operator(&self) -> TokenKind {
        match self {
            TokenKind::AssignAdd => TokenKind::Add,
            TokenKind::AssignSub => TokenKind::Sub,
            TokenKind::AssignMul => TokenKind::Mul,
            TokenKind::AssignDiv => TokenKind::Div,
            TokenKind::AssignMod => TokenKind::Mod,
            TokenKind::AssignPow => TokenKind::Pow,
            TokenKind::AssignBitOr => TokenKind::Pipe,
            TokenKind::AssignBitAnd => TokenKind::BitAnd,
            TokenKind::AssignBitXor => TokenKind::BitXor,
            TokenKind::AssignBitLeftShift => TokenKind::BitLeftShift,
            TokenKind::AssignBitRightShift => TokenKind::BitRightShift,
            TokenKind::AssignBitUnsignedRightShift => TokenKind::BitUnsignedRightShift,
            _ => panic!("not a valid infix assignment operator"),
        }
    }

    pub fn matching_bracket(&self) -> Option<TokenKind> {
        match self {
            TokenKind::LeftBrace => Some(TokenKind::RightBrace),
            TokenKind::LeftParen => Some(TokenKind::RightParen),
            TokenKind::LeftBracket => Some(TokenKind::RightBracket),

            TokenKind::RightBrace => Some(TokenKind::LeftBrace),
            TokenKind::RightParen => Some(TokenKind::LeftParen),
            TokenKind::RightBracket => Some(TokenKind::LeftBracket),
            _ => None,
        }
    }
}

impl Display for TokenValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use TokenValue::*;

        match self {
            IntegerValue(value, base, suffix) => match suffix {
                Some(suffix) => {
                    write!(f, "{} base {} in {}", value, base.radix(), suffix,)
                }
                None => write!(f, "{} base {}", value, base.radix(),),
            },
            FloatValue(value, suffix) => match suffix {
                Some(suffix) => {
                    write!(f, "{} in {}", value, suffix,)
                }
                None => write!(f, "{}", value),
            },
            TextValue(value) => {
                let formatted = format!("{:?}", value);
                let sliced = formatted
                    .chars()
                    .skip(1)
                    .take(formatted.chars().count() - 2)
                    .collect::<String>();
                write!(f, "{}", sliced)
            }
            CharacterValue(char) => {
                let formatted = format!("{:?}", char);
                let sliced = formatted
                    .chars()
                    .skip(1)
                    .take(formatted.chars().count() - 2)
                    .collect::<String>();
                write!(f, "{}", sliced)
            }
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            Some(value) => write!(f, "{:?}({})", self.kind, value),
            None => write!(f, "{:?}", self.kind),
        }
    }
}
