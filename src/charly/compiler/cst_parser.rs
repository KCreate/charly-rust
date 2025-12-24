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

use crate::charly::compiler::cst::{CSTNode, CSTTree, CSTTreeKind};
use crate::charly::compiler::token::{
    Token, TokenKind, TOKEN_POSTFIX_OPERATORS, TOKEN_PREFIX_OPERATORS,
};
use crate::charly::compiler::tokenset::TokenSet;
use crate::charly::utils::diagnostics::DiagnosticContext;
use crate::charly::utils::fuel_store::FuelStore;

enum Event {
    Open(CSTTreeKind),
    Close,
    Advance,
    Skip,
}

struct MarkOpened {
    index: usize,
}

struct MarkClosed {
    index: usize,
}

enum SequenceControlFlow {
    Continue,
    Break,
}

/// Implements a Kladov-style parsing algorithm.
/// Reference: https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html
pub struct CSTParser<'a> {
    tokens: &'a Vec<Token>,
    diagnostic_context: &'a mut DiagnosticContext,
    fuel: FuelStore,
    events: Vec<Event>,
    pos: usize,
    previous_pos: usize,
    previous_pos_all: usize,
}

impl<'a> CSTParser<'a> {
    pub fn new(
        tokens: &'a Vec<Token>,
        diagnostic_context: &'a mut DiagnosticContext,
    ) -> Self {
        Self {
            tokens,
            diagnostic_context,
            fuel: FuelStore::new("CSTParser", 256),
            events: Vec::new(),
            pos: 0,
            previous_pos: 0,
            previous_pos_all: 0,
        }
    }

    pub fn parse(&mut self) -> CSTTree {
        self.parse_program();
        self.build_tree()
    }

    /// ```bnf
    /// Program ::= TopLevelItem*
    /// ```
    fn parse_program(&mut self) {
        self.with(CSTTreeKind::Program, |this| {
            this.skip_whitespace_and_comments();
            this.expect_sequence(
                "a top level item",
                &Self::STARTERS_TOP_LEVEL_ITEM,
                &TokenSet::from(TokenKind::EndOfFile),
                &TokenSet::from(TokenKind::EndOfFile),
                None,
                false,
                true,
                false,
                &Self::parse_top_level_item,
            );
        });
    }

    /// ```bnf
    /// TopLevelItem ::= ImportDecl
    ///                | FnDecl
    ///                | VarDecl
    ///                | TypeAliasDecl
    ///                | StructDecl
    ///                | InterfaceDecl
    ///                | UnionDecl
    ///                | EnumDecl
    ///                | ImplDecl
    ///                | Stmt
    /// ```
    const STARTERS_TOP_LEVEL_ITEM: TokenSet = TokenSet::from_kinds_and_sets(
        &[],
        &[&Self::STARTERS_IMPORT_DECL, &Self::STARTERS_EXPR],
    );
    fn parse_top_level_item(&mut self, recovery: &TokenSet) -> MarkClosed {
        self.with(CSTTreeKind::TopLevelItem, |this| {
            match this.current() {
                TokenKind::Import => this.parse_import_decl(recovery),
                _ => this.parse_expr(recovery),
            };
        })
    }

    /// ```bnf
    /// ImportDecl ::= "import" ImportPath ImportAsItem?
    /// ```
    const STARTERS_IMPORT_DECL: TokenSet =
        TokenSet::from_kinds_and_sets(&[TokenKind::Import], &[]);
    fn parse_import_decl(&mut self, recovery: &TokenSet) -> MarkClosed {
        self.with(CSTTreeKind::ImportDecl, |this| {
            this.expect(TokenKind::Import, &recovery.union(TokenKind::As));
            this.parse_import_path(&recovery.union(TokenKind::As));
            this.recover_until(TokenKind::As, recovery);
            if this.at(TokenKind::As) {
                this.with(CSTTreeKind::ImportAsItem, |this| {
                    this.expect(TokenKind::As, recovery);
                    this.expect(TokenKind::Identifier, recovery);
                });
            }
        })
    }

    /// ```bnf
    /// ImportPath ::= Id ("." Id)* ("." "*")?
    /// ```
    const STARTERS_PATH_ENTRY: TokenSet =
        TokenSet::from_kinds(&[TokenKind::Identifier, TokenKind::Mul]);
    fn parse_import_path(&mut self, recovery: &TokenSet) -> MarkClosed {
        self.with(CSTTreeKind::ImportPath, |this| {
            this.expect_sequence(
                "an identifier or '*'",
                &Self::STARTERS_PATH_ENTRY,
                recovery,
                recovery,
                Some(TokenKind::Dot),
                true,
                false,
                true,
                |this, recovery| {
                    this.expect_any(
                        "an identifier or '*'",
                        &Self::STARTERS_PATH_ENTRY,
                        recovery,
                    );
                },
            );
        })
    }

    /// ```bnf
    /// AttributeList ::= Attribute*
    /// ```
    fn parse_attribute_list(
        &mut self,
        terminator_set: &TokenSet,
        recovery: &TokenSet,
    ) -> MarkClosed {
        self.with(CSTTreeKind::AttributeList, |this| {
            this.expect_sequence(
                "an attribute",
                &Self::STARTERS_ATTRIBUTE,
                recovery,
                terminator_set,
                None,
                false,
                false,
                false,
                &Self::parse_attribute,
            )
        })
    }

    /// ```bnf
    /// Attribute ::= "@" Id ("(" AttributeArguments? ")")?
    /// ```
    const STARTERS_ATTRIBUTE: TokenSet =
        TokenSet::from_kinds_and_sets(&[TokenKind::AtSign], &[]);
    fn parse_attribute(&mut self, recovery: &TokenSet) -> MarkClosed {
        self.with(CSTTreeKind::Attribute, |this| {
            this.expect(TokenKind::AtSign, recovery);
            this.expect(TokenKind::Identifier, &recovery.union(TokenKind::LeftParen));
            if this.is_at_any(&Self::STARTERS_ATTRIBUTE_ARGUMENTS) {
                this.parse_attribute_arguments(recovery);
            }
        })
    }

    /// ```bnf
    /// AttributeArguments ::= "(" Expr ("," Expr)* ")"
    /// ```
    const STARTERS_ATTRIBUTE_ARGUMENTS: TokenSet =
        TokenSet::from_kinds_and_sets(&[TokenKind::LeftParen], &[]);
    fn parse_attribute_arguments(&mut self, recovery: &TokenSet) -> MarkClosed {
        self.with(CSTTreeKind::AttributeArguments, |this| {
            this.expect_bracket_group(
                CSTTreeKind::ParenGroup,
                recovery,
                |this, recovery| {
                    this.expect_sequence(
                        "an attribute argument",
                        &Self::STARTERS_EXPR,
                        recovery,
                        recovery,
                        Some(TokenKind::Comma),
                        false,
                        false,
                        false,
                        &Self::parse_expr,
                    );
                },
            );
        })
    }

    const STARTERS_EXPR: TokenSet = TokenSet::from_kinds_and_sets(
        &[],
        &[&Self::STARTERS_ATOM, &TOKEN_PREFIX_OPERATORS],
    );
    fn parse_expr(&mut self, recovery: &TokenSet) -> MarkClosed {
        self.parse_expr_rec(0, recovery)
    }

    fn parse_expr_rec(&mut self, minimum_bp: i8, recovery: &TokenSet) -> MarkClosed {
        // 1( prefix operators and literal parsing
        let mut lhs = if self.is_at_any(&TOKEN_PREFIX_OPERATORS) {
            self.with(CSTTreeKind::PrefixOpExpr, |this| {
                let (_, right_bp) = Self::binding_power_prefix(this.current());
                this.advance();
                this.parse_expr_rec(right_bp, recovery);
            })
        } else {
            self.parse_delimited(recovery)
        };

        // 2( postfix and infix operator parsing
        loop {
            if let Some((left_bp, _)) = Self::binding_power_postfix(self.current()) {
                if left_bp < minimum_bp {
                    break;
                }

                self.with_before(CSTTreeKind::PostfixOpExpr, &lhs, |this| {
                    this.expect_any(
                        "postfix operator",
                        &TOKEN_POSTFIX_OPERATORS,
                        recovery,
                    );
                });

                continue;
            }

            if let Some((left_bp, right_bp)) = Self::binding_power_infix(self.current()) {
                if left_bp < minimum_bp {
                    break;
                }

                lhs = self.with_before(CSTTreeKind::InfixOpExpr, &lhs, |this| {
                    this.advance();
                    this.parse_expr_rec(right_bp, recovery);
                });

                continue;
            }

            break;
        }

        lhs
    }

    fn parse_delimited(&mut self, recovery: &TokenSet) -> MarkClosed {
        let mut lhs = self.parse_atom(recovery);

        loop {
            match self.current() {
                TokenKind::Dot => {
                    lhs = self.with_before(CSTTreeKind::MemberExpr, &lhs, |this| {
                        this.expect(TokenKind::Dot, recovery);
                        this.expect(TokenKind::Identifier, recovery);
                    });
                }
                TokenKind::QuestionMarkDot => {
                    lhs =
                        self.with_before(CSTTreeKind::NullableMemberExpr, &lhs, |this| {
                            this.expect(TokenKind::QuestionMarkDot, recovery);
                            this.expect(TokenKind::Identifier, recovery);
                        });
                }
                _ => return lhs,
            }
        }
    }

    fn binding_power_infix(operator: TokenKind) -> Option<(i8, i8)> {
        match operator {
            TokenKind::Assign
            | TokenKind::AssignAdd
            | TokenKind::AssignSub
            | TokenKind::AssignMul
            | TokenKind::AssignDiv
            | TokenKind::AssignMod
            | TokenKind::AssignPow
            | TokenKind::AssignBitOr
            | TokenKind::AssignBitAnd
            | TokenKind::AssignBitXor
            | TokenKind::AssignBitLeftShift
            | TokenKind::AssignBitRightShift
            | TokenKind::AssignBitUnsignedRightShift => Some((0, 1)),

            TokenKind::DoublePipe => Some((1, 2)),
            TokenKind::And => Some((2, 3)),

            TokenKind::Eq
            | TokenKind::Neq
            | TokenKind::Lt
            | TokenKind::Gt
            | TokenKind::Lte
            | TokenKind::Gte
            | TokenKind::Is
            | TokenKind::As
            | TokenKind::In => Some((3, 4)),

            // right-associative
            TokenKind::QuestionMarkColon => Some((5, 4)),

            TokenKind::Pipe => Some((5, 6)),
            TokenKind::BitXor => Some((6, 7)),
            TokenKind::BitAnd => Some((7, 8)),

            TokenKind::RangeExclusive | TokenKind::DoubleDot => Some((8, 9)),

            TokenKind::Add | TokenKind::Sub => Some((9, 10)),
            TokenKind::Mul | TokenKind::Div | TokenKind::Mod => Some((10, 11)),

            // right-associative
            TokenKind::Pow => Some((12, 11)),

            TokenKind::BitLeftShift
            | TokenKind::BitRightShift
            | TokenKind::BitUnsignedRightShift => Some((12, 13)),
            _ => None,
        }
    }

    fn binding_power_prefix(operator: TokenKind) -> ((), i8) {
        match operator {
            // TODO: this might be too tight??
            TokenKind::Await => ((), 13),
            TokenKind::Add | TokenKind::Sub => ((), 13),
            TokenKind::Not => ((), 13),
            TokenKind::BitNot => ((), 13),
            TokenKind::TripleDot => ((), 0),
            _ => unreachable!(),
        }
    }

    fn binding_power_postfix(operator: TokenKind) -> Option<(i8, ())> {
        match operator {
            TokenKind::DoubleNot => Some((14, ())),
            _ => None,
        }
    }

    const STARTERS_ATOM: TokenSet = TokenSet::from_kinds_and_sets(
        &[
            TokenKind::Identifier,
            TokenKind::Integer,
            TokenKind::Float,
            TokenKind::True,
            TokenKind::False,
            TokenKind::Null,
            TokenKind::StringStart,
            TokenKind::LeftParen,
            TokenKind::LeftBracket,
        ],
        &[],
    );
    fn parse_atom(&mut self, recovery: &TokenSet) -> MarkClosed {
        match self.current() {
            TokenKind::Identifier
            | TokenKind::Integer
            | TokenKind::Float
            | TokenKind::True
            | TokenKind::False
            | TokenKind::Null => self.with(CSTTreeKind::Atom, |this| {
                this.advance();
            }),

            TokenKind::LeftParen => self.expect_bracket_group(
                CSTTreeKind::ParenGroup,
                recovery,
                |this, recovery| {
                    this.expect_sequence(
                        "an expression",
                        &Self::STARTERS_EXPR,
                        recovery,
                        recovery,
                        Some(TokenKind::Comma),
                        false,
                        false,
                        false,
                        &Self::parse_expr,
                    );
                },
            ),

            TokenKind::LeftBracket => self.expect_bracket_group(
                CSTTreeKind::BracketGroup,
                recovery,
                |this, recovery| {
                    this.expect_sequence(
                        "an expression",
                        &Self::STARTERS_EXPR,
                        recovery,
                        recovery,
                        Some(TokenKind::Comma),
                        false,
                        false,
                        false,
                        &Self::parse_expr,
                    );
                },
            ),

            TokenKind::StringStart => self.parse_string(recovery),

            _ => self.with(CSTTreeKind::Error, |this| {
                this.expect_any("an atom", &Self::STARTERS_ATOM, recovery);
            }),
        }
    }

    const STARTERS_STRING_PART: TokenSet = TokenSet::from_kinds_and_sets(
        &[TokenKind::StringText, TokenKind::StringExprStart],
        &[],
    );
    fn parse_string(&mut self, recovery: &TokenSet) -> MarkClosed {
        self.with(CSTTreeKind::String, |this| {
            this.expect(TokenKind::StringStart, recovery);
            this.expect_sequence(
                "a string part",
                &Self::STARTERS_STRING_PART,
                recovery,
                &TokenSet::from(TokenKind::StringEnd),
                None,
                false,
                false,
                false,
                |this, recovery| match this.current() {
                    TokenKind::StringText => {
                        this.advance();
                    }
                    TokenKind::StringExprStart => {
                        this.with(CSTTreeKind::StringInterpolatedExpr, |this| {
                            this.advance();
                            this.parse_expr(&recovery.union(TokenKind::StringExprEnd));
                            this.expect(TokenKind::StringExprEnd, recovery);
                        });
                    }
                    _ => unreachable!("unexpected token"),
                },
            );
            this.expect(TokenKind::StringEnd, recovery);
        })
    }

    fn expect_bracket_group<F>(
        &mut self,
        open_kind: CSTTreeKind,
        recovery: &TokenSet,
        callback: F,
    ) -> MarkClosed
    where
        F: FnOnce(&mut Self, &TokenSet),
    {
        let (open_token_kind, close_token_kind) = open_kind
            .matching_bracket_token_kinds()
            .expect("expected bracket kind");

        self.with(open_kind, |this| {
            let new_recovery = recovery.union(close_token_kind);
            this.expect(open_token_kind, &new_recovery);
            callback(this, &new_recovery);
            this.expect(close_token_kind, recovery);
        })
    }

    fn expect_sequence<F, R>(
        &mut self,
        expected_str: &str,
        starter_set: &TokenSet,
        recovery: &TokenSet,
        terminator_set: &TokenSet,
        separator_token: Option<TokenKind>,
        break_on_missing_separator: bool,
        allow_annotations: bool,
        expect_at_least_one: bool,
        callback: F,
    ) where
        F: Fn(&mut Self, &TokenSet) -> R,
    {
        self.expect_sequence_with_control_flow(
            expected_str,
            starter_set,
            recovery,
            terminator_set,
            separator_token,
            break_on_missing_separator,
            allow_annotations,
            expect_at_least_one,
            |this, recovery| {
                callback(this, recovery);
                SequenceControlFlow::Continue
            },
        )
    }

    fn expect_sequence_with_control_flow<F>(
        &mut self,
        expected_str: &str,
        starter_set: &TokenSet,
        recovery: &TokenSet,
        terminator_set: &TokenSet,
        separator_token: Option<TokenKind>,
        break_on_missing_separator: bool,
        allow_annotations: bool,
        expect_at_least_one: bool,
        callback: F,
    ) where
        F: Fn(&mut Self, &TokenSet) -> SequenceControlFlow,
    {
        let mut effective_starter_set = starter_set.clone();
        if allow_annotations {
            effective_starter_set = effective_starter_set.union(TokenKind::AtSign);
        }

        let mut child_recovery = recovery.clone();
        child_recovery = child_recovery.union_set(&effective_starter_set);
        child_recovery = child_recovery.union_set(terminator_set);
        if let Some(separator) = separator_token {
            child_recovery = child_recovery.union(separator)
        }

        let effective_recovery_set = terminator_set
            .union_set(recovery)
            .without_set(&effective_starter_set);

        let mut iteration_count = 0;

        while !self.eof() {
            if self.eof() || self.is_at_any(&effective_recovery_set) {
                break;
            }

            if !self.is_at_any(&effective_starter_set) {
                self.recover_until_any(
                    expected_str,
                    &effective_starter_set,
                    &child_recovery,
                );
            }

            if self.eof() || self.is_at_any(&effective_recovery_set) {
                break;
            }

            let mut control_flow = SequenceControlFlow::Continue;
            if self.is_at_any(&effective_starter_set) {
                if allow_annotations {
                    self.parse_annotated_node(
                        starter_set,
                        &child_recovery,
                        |this, recovery| {
                            control_flow = callback(this, recovery);
                        },
                    );
                } else {
                    control_flow = callback(self, &child_recovery);
                }
            }

            iteration_count += 1;

            if self.eof() || self.is_at_any(&effective_recovery_set) {
                break;
            }

            if let Some(separator) = separator_token {
                let consumed_separator = self.expect(separator, &child_recovery);
                if break_on_missing_separator && !consumed_separator {
                    break;
                }
            }

            if let SequenceControlFlow::Break = control_flow {
                break;
            }
        }

        if iteration_count == 0 {
            if expect_at_least_one {
                self.diagnostic_context.error(
                    format!("Expected at least one of '{}'", expected_str).as_str(),
                    &self.current_token().location.clone(),
                    vec![],
                    vec![],
                );
            }
        }
    }

    fn parse_annotated_node<F>(
        &mut self,
        starter_set: &TokenSet,
        recovery: &TokenSet,
        callback: F,
    ) where
        F: FnOnce(&mut Self, &TokenSet),
    {
        if self.at(TokenKind::AtSign) {
            self.with(CSTTreeKind::NodeWithAttributes, |this| {
                this.parse_attribute_list(starter_set, recovery);
                callback(this, recovery);
            });
        } else {
            callback(self, recovery);
        }
    }

    /// Convenience method to easily open and close a CST node
    ///
    /// Usage:
    /// ```rust
    /// self.with(CSTTreeKind::TopLevelItem, |this| { ... });
    /// ```
    fn with<F>(&mut self, kind: CSTTreeKind, callback: F) -> MarkClosed
    where
        F: FnOnce(&mut Self),
    {
        let mark = self.open();
        callback(self);
        self.close(mark, kind)
    }

    /// Convenience method to easily open and close a CST node
    /// Opens the new node before another node
    ///
    /// Usage:
    /// ```rust
    /// self.with_before(CSTTreeKind::TopLevelItem, before, |this| { ... });
    /// ```
    fn with_before<F>(
        &mut self,
        kind: CSTTreeKind,
        before: &MarkClosed,
        callback: F,
    ) -> MarkClosed
    where
        F: FnOnce(&mut Self),
    {
        let mark = self.open_before(before);
        callback(self);
        self.close(mark, kind)
    }

    fn open(&mut self) -> MarkOpened {
        let mark = MarkOpened {
            index: self.events.len(),
        };
        self.events.push(Event::Open(CSTTreeKind::Error));
        mark
    }

    fn open_before(&mut self, mark: &MarkClosed) -> MarkOpened {
        let mark = MarkOpened { index: mark.index };
        let event = Event::Open(CSTTreeKind::Error);
        self.events.insert(mark.index, event);
        mark
    }

    fn close(&mut self, mark: MarkOpened, kind: CSTTreeKind) -> MarkClosed {
        self.events[mark.index] = Event::Open(kind);
        self.events.push(Event::Close);
        MarkClosed { index: mark.index }
    }

    fn advance(&mut self) -> TokenKind {
        self.previous_pos = self.pos;
        let kind = self.current();
        self.event_token_advance();
        self.skip_whitespace_and_comments();
        kind
    }

    fn skip_whitespace_and_comments(&mut self) {
        while self.current().is_comment() || self.current().is_whitespace() {
            self.event_token_skip();
        }
    }

    fn recover_until(&mut self, kind: TokenKind, recovery: &TokenSet) {
        self.recover_until_any(
            format!("'{}'", kind).as_str(),
            &TokenSet::from(kind),
            recovery,
        );
    }

    fn recover_until_any(
        &mut self,
        expected_str: &str,
        expected_set: &TokenSet,
        recovery: &TokenSet,
    ) {
        if self.is_at_any(expected_set) {
            return;
        }

        let start_location = self.current_token().location.clone();
        if !self.is_at_any(recovery) {
            self.with(CSTTreeKind::Error, |this| {
                this.advance_until_any(expected_set, recovery);
            });

            let end_location = self.previous_token_all().location.clone();
            let merged_location = start_location.merge(&end_location);

            self.diagnostic_context.error(
                "Unexpected token(s)",
                &merged_location,
                vec![],
                vec![],
            );
        }

        if !self.is_at_any(expected_set) {
            self.diagnostic_context.error(
                format!("Expected {}", expected_str).as_str(),
                &start_location,
                vec![],
                vec![],
            );
        }
    }

    fn advance_until(&mut self, kind: TokenKind, recovery: &TokenSet) -> bool {
        self.advance_until_any(&TokenSet::from(kind), recovery)
            .is_some()
    }

    fn advance_until_any(
        &mut self,
        expected_set: &TokenSet,
        recovery: &TokenSet,
    ) -> Option<TokenKind> {
        if let Some(kind) = self.at_any(expected_set) {
            return Some(kind);
        }

        let merged_set = expected_set.union_set(recovery);
        while !self.eof() && !self.is_at_any(&merged_set) {
            self.event_token_advance();
            self.skip_whitespace_and_comments();
        }

        self.at_any(expected_set)
    }

    fn event_token_advance(&mut self) -> TokenKind {
        self.events.push(Event::Advance);
        let kind = self.current_token().kind;
        self.advance_token_stream();
        kind
    }

    fn event_token_skip(&mut self) {
        self.events.push(Event::Skip);
        self.advance_token_stream();
    }

    fn advance_token_stream(&mut self) {
        assert!(!self.eof());
        self.fuel.replenish();
        self.previous_pos_all = self.pos;
        self.pos += 1;
    }

    fn eof(&self) -> bool {
        self.current() == TokenKind::EndOfFile
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.current() == kind
    }

    fn at_any(&self, set: &TokenSet) -> Option<TokenKind> {
        if set.has(self.current()) {
            Some(self.current())
        } else {
            None
        }
    }

    fn is_at_any(&self, set: &TokenSet) -> bool {
        self.at_any(set).is_some()
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn eat_any(&mut self, expected_set: &TokenSet) -> Option<TokenKind> {
        if self.at_any(expected_set).is_some() {
            Some(self.advance())
        } else {
            None
        }
    }

    fn expect(&mut self, kind: TokenKind, recovery: &TokenSet) -> bool {
        self.recover_until(kind, recovery);
        self.eat(kind)
    }

    fn expect_any(
        &mut self,
        expected_str: &str,
        expected_set: &TokenSet,
        recovery: &TokenSet,
    ) -> Option<TokenKind> {
        self.recover_until_any(expected_str, expected_set, recovery);
        self.eat_any(expected_set)
    }

    fn current(&self) -> TokenKind {
        self.fuel.consume();
        self.current_token().kind
    }

    fn current_token(&self) -> &Token {
        self.tokens
            .get(self.pos)
            .unwrap_or(self.tokens.last().unwrap())
    }

    fn previous_token(&self) -> &Token {
        self.tokens
            .get(self.previous_pos)
            .unwrap_or(self.tokens.last().unwrap())
    }

    fn previous_token_all(&self) -> &Token {
        self.tokens
            .get(self.previous_pos_all)
            .unwrap_or(self.tokens.last().unwrap())
    }

    fn build_tree(&mut self) -> CSTTree {
        let mut tokens = self.tokens.into_iter();
        let events = &mut self.events;
        let mut stack: Vec<CSTTree> = Vec::new();

        assert!(matches!(events.pop(), Some(Event::Close)));

        for event in events {
            match event {
                // starting a new node, push an empty tree to the stack
                Event::Open(kind) => {
                    let tree = CSTTree {
                        kind: *kind,
                        children: Vec::new(),
                    };
                    stack.push(tree);
                }

                // a tree is done,
                // pop it off the stack and append to a new current tree
                // if the produced tree is empty, discard it
                Event::Close => {
                    let tree = stack.pop().unwrap();
                    if !tree.children.is_empty() {
                        let current = stack.last_mut().unwrap();
                        current.children.push(CSTNode::Tree(tree));
                    }
                }

                // consume a token and append it to the current tree
                Event::Advance => {
                    let token = tokens.next().unwrap().clone();
                    let tree = stack.last_mut().unwrap();
                    tree.children.push(CSTNode::Token(token));
                }

                // skip over a token
                Event::Skip => {
                    tokens.next().unwrap();
                }
            }
        }

        assert_eq!(stack.len(), 1);
        assert_eq!(tokens.next().unwrap().kind, TokenKind::EndOfFile);
        stack.pop().unwrap()
    }
}

#[cfg(test)]
#[allow(unused)]
mod tests {
    use crate::charly::compiler::cst_parser::CSTParser;
    use crate::charly::compiler::tokenizer::Tokenizer;
    use crate::charly::test_utils::validate_expected_diagnostics;
    use crate::charly::utils::ascii_tree::IndentStyle;
    use crate::charly::utils::cst_printer::CstPrinter;
    use crate::charly::utils::diagnostics::DiagnosticController;
    use clap::builder::TypedValueParser;
    use pretty_assertions::assert_eq;
    use std::path::PathBuf;

    #[allow(unused)]
    #[track_caller]
    fn assert_tree(source: &str, expected_tree: &str, expected_diagnostics: &[&str]) {
        let mut controller = DiagnosticController::new();
        let path = PathBuf::from("test");
        let file_id = controller.register_file(&path, source);
        let mut context = controller.get_or_create_context(file_id);

        let tokens = Tokenizer::tokenize(source, &mut context);

        let mut parser = CSTParser::new(&tokens, &mut context);
        let tree = parser.parse();

        let mut printer = CstPrinter::new();
        printer.set_indent_style(IndentStyle::Plain);
        printer.set_enable_colors(false);

        let tree_formatted = printer.format(&tree);

        assert_eq!(tree_formatted.trim_end(), expected_tree.trim_end(),);
        validate_expected_diagnostics(&context, expected_diagnostics);
    }
}
