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

use crate::charly::compiler::token::{TokenKind, TOKEN_ALL};
use std::fmt::{Display, Formatter};

const TOKEN_SET_BACKING_WORDS: usize = (TokenKind::TOKEN_KIND_COUNT + 63) / 64;

#[derive(Copy, Clone, Debug)]
pub struct TokenSet {
    backing: [u64; TOKEN_SET_BACKING_WORDS],
}

impl TokenSet {
    pub const EMPTY: TokenSet = TokenSet {
        backing: [0; TOKEN_SET_BACKING_WORDS],
    };

    pub const fn from_kinds(kinds: &[TokenKind]) -> TokenSet {
        TokenSet::from_kinds_and_sets(kinds, &[])
    }

    pub const fn from_kinds_and_sets(
        kinds: &[TokenKind],
        sets: &[&TokenSet],
    ) -> TokenSet {
        let mut result = TokenSet::EMPTY;

        let mut i = 0;
        while i < kinds.len() {
            result.set(kinds[i]);
            i += 1;
        }

        let mut i = 0;
        while i < sets.len() {
            result = result.union_set(&sets[i]);
            i += 1;
        }

        result
    }

    pub const fn has(&self, kind: TokenKind) -> bool {
        let backing_index = Self::token_kind_to_backing_index(kind);
        let backing_flag = Self::token_kind_to_backing_flag(kind);
        self.backing[backing_index] & backing_flag != 0
    }

    pub const fn set(&mut self, kind: TokenKind) {
        let backing_index = Self::token_kind_to_backing_index(kind);
        let backing_flag = Self::token_kind_to_backing_flag(kind);
        self.backing[backing_index] |= backing_flag;
    }

    pub const fn union(&self, other: TokenKind) -> TokenSet {
        TokenSet::from_kinds_and_sets(&[other], &[self])
    }

    pub const fn union_tokens(&self, other: &[TokenKind]) -> TokenSet {
        TokenSet::from_kinds_and_sets(other, &[self])
    }

    pub const fn union_set(&self, other: &TokenSet) -> TokenSet {
        let mut result = TokenSet::EMPTY;

        let mut i = 0;
        while i < self.backing.len() {
            result.backing[i] = self.backing[i] | other.backing[i];
            i += 1;
        }

        result
    }

    pub const fn without(&self, other: TokenKind) -> TokenSet {
        let mut result = *self;
        let backing_index = Self::token_kind_to_backing_index(other);
        let backing_flag = Self::token_kind_to_backing_flag(other);
        result.backing[backing_index] &= !backing_flag;
        result
    }

    pub const fn without_set(&self, other: &TokenSet) -> TokenSet {
        let mut result = *self;

        let mut i = 0;
        while i < self.backing.len() {
            result.backing[i] &= !other.backing[i];
            i += 1;
        }

        result
    }

    const fn token_kind_to_backing_index(kind: TokenKind) -> usize {
        (kind as usize) / 64
    }

    const fn token_kind_to_backing_flag(kind: TokenKind) -> u64 {
        1 << ((kind as u64) % 64)
    }
}

impl From<TokenKind> for TokenSet {
    fn from(kind: TokenKind) -> Self {
        TokenSet::from_kinds(&[kind])
    }
}

impl Display for TokenSet {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string_formats = TOKEN_ALL
            .iter()
            .filter(|kind| self.has(**kind))
            .map(|kind| match kind {
                _ if kind.is_punctuator() => {
                    return format!("'{}'", kind.to_string());
                }
                _ => {
                    format!("'{}'", kind.to_string())
                }
            })
            .collect::<Vec<String>>();

        let mut buf = String::new();
        for (i, s) in string_formats.iter().enumerate() {
            match i {
                0 => {}
                n if n == string_formats.len() - 1 => {
                    buf.push_str(" or ");
                }
                _ => {
                    buf.push_str(", ");
                }
            }
            buf.push_str(s);
        }

        write!(f, "{}", buf)
    }
}
