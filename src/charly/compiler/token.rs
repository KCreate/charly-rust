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

        pub static TOKEN_ALL: &[TokenKind] = &[
            $( TokenKind::$bare_variant, )*
            $( TokenKind::$kw_variant, )*
            $( TokenKind::$punct_variant, )*
        ];

        pub static TOKEN_KEYWORDS: &[TokenKind] = &[
            $( TokenKind::$kw_variant, )*
        ];

        pub static TOKEN_PUNCTUATORS: &[TokenKind] = &[
            $( TokenKind::$punct_variant, )*
        ];

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
        RangeInclusive => "..=",
    },
}

impl TokenKind {
    //noinspection RsUnnecessaryQualifications
    pub fn is_infix_operator(&self) -> bool {
        match self {
            TokenKind::DoublePipe => true,
            TokenKind::And => true,
            TokenKind::Pipe => true,
            TokenKind::BitXor => true,
            TokenKind::BitAnd => true,
            TokenKind::Eq => true,
            TokenKind::Neq => true,
            TokenKind::Gt => true,
            TokenKind::Gte => true,
            TokenKind::Lt => true,
            TokenKind::Lte => true,
            TokenKind::BitLeftShift => true,
            TokenKind::BitRightShift => true,
            TokenKind::BitUnsignedRightShift => true,
            TokenKind::Add => true,
            TokenKind::Sub => true,
            TokenKind::Mul => true,
            TokenKind::Div => true,
            TokenKind::Mod => true,
            TokenKind::Pow => true,
            TokenKind::Is => true,
            TokenKind::In => true,
            TokenKind::QuestionMarkColon => true,
            TokenKind::RangeInclusive => true,
            TokenKind::DoubleDot => true,
            _ => false,
        }
    }

    //noinspection RsUnnecessaryQualifications
    pub fn is_prefix_operator(&self) -> bool {
        match self {
            TokenKind::Await => true,
            TokenKind::Add => true,
            TokenKind::Sub => true,
            TokenKind::Not => true,
            TokenKind::BitNot => true,
            TokenKind::TripleDot => true,
            _ => false,
        }
    }

    //noinspection RsUnnecessaryQualifications
    pub fn is_postfix_operator(&self) -> bool {
        match self {
            TokenKind::DoubleNot => true,
            _ => false,
        }
    }

    pub fn is_assign_infix_operator(&self) -> bool {
        match self {
            TokenKind::AssignAdd => true,
            TokenKind::AssignSub => true,
            TokenKind::AssignMul => true,
            TokenKind::AssignDiv => true,
            TokenKind::AssignMod => true,
            TokenKind::AssignPow => true,
            TokenKind::AssignBitOr => true,
            TokenKind::AssignBitAnd => true,
            TokenKind::AssignBitXor => true,
            TokenKind::AssignBitLeftShift => true,
            TokenKind::AssignBitRightShift => true,
            TokenKind::AssignBitUnsignedRightShift => true,
            _ => false,
        }
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

    pub fn is_opening_bracket(&self) -> bool {
        match self {
            TokenKind::LeftBrace => true,
            TokenKind::LeftParen => true,
            TokenKind::LeftBracket => true,
            _ => false,
        }
    }

    pub fn is_closing_bracket(&self) -> bool {
        match self {
            TokenKind::RightBrace => true,
            TokenKind::RightParen => true,
            TokenKind::RightBracket => true,
            _ => false,
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

    pub fn is_whitespace(&self) -> bool {
        match self {
            TokenKind::Whitespace => true,
            TokenKind::Newline => true,
            _ => false,
        }
    }

    pub fn is_comment(&self) -> bool {
        match self {
            TokenKind::SingleLineComment => true,
            TokenKind::MultiLineComment => true,
            TokenKind::DocComment => true,
            _ => false,
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
