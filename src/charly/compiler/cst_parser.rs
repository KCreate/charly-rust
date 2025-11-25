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
use crate::charly::compiler::token::{Token, TokenKind};
use crate::charly::utils::diagnostics::DiagnosticContext;

enum Event {
    Open { kind: CSTTreeKind },
    Close,
    Advance,
}

struct MarkOpened {
    index: usize,
}

struct MarkClosed {
    index: usize,
}

/// Implements a Kladov-style parsing algorithm
/// Reference: https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html
pub struct CSTParser<'a> {
    tokens: &'a Vec<Token>,
    diagnostic_context: &'a mut DiagnosticContext,
    pos: usize,
    fuel: u32,
    events: Vec<Event>,
}

impl<'a> CSTParser<'a> {
    pub fn new(
        tokens: &'a Vec<Token>,
        diagnostic_context: &'a mut DiagnosticContext,
    ) -> Self {
        Self {
            tokens,
            diagnostic_context,
            pos: 0,
            fuel: 256,
            events: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> CSTTree {
        // self.parse_file();
        self.build_tree()
    }

    fn open(&mut self) -> MarkOpened {
        let mark = MarkOpened {
            index: self.events.len(),
        };
        self.events.push(Event::Open {
            kind: CSTTreeKind::Error,
        });
        mark
    }

    fn open_before(&mut self, mark: &MarkClosed) -> MarkOpened {
        let mark = MarkOpened { index: mark.index };
        let event = Event::Open {
            kind: CSTTreeKind::Error,
        };
        self.events.insert(mark.index, event);
        mark
    }

    fn close(&mut self, mark: MarkOpened, kind: CSTTreeKind) -> MarkClosed {
        self.events[mark.index] = Event::Open { kind };
        self.events.push(Event::Close);
        MarkClosed { index: mark.index }
    }

    fn advance(&mut self) {
        assert!(!self.eof());
        self.fuel = 256;
        self.events.push(Event::Advance);
        self.pos += 1;
    }

    fn eof(&mut self) -> bool {
        self.current().is_none()
    }

    fn at(&mut self, kind: TokenKind) -> bool {
        self.current() == Some(kind)
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn current(&mut self) -> Option<TokenKind> {
        self.nth(0)
    }

    fn current_token(&self) -> Option<&Token> {
        self.nth_token(0)
    }

    fn nth(&mut self, lookahead: usize) -> Option<TokenKind> {
        if self.fuel == 0 {
            panic!("parser ran out of fuel");
        }
        self.fuel = self.fuel - 1;
        self.nth_token(lookahead)
            .map_or(None, |token| Some(token.kind))
    }

    fn nth_token(&self, lookahead: usize) -> Option<&Token> {
        self.tokens.get(self.pos + lookahead)
    }

    // /// ```bnf
    // /// file ::= <stmt>* ;
    // /// ```
    // fn parse_file(&mut self) {
    //     let m = self.open();
    //
    //     while !self.eof() {
    //         if !self.at_set(&Self::STARTERS_STMT) {
    //             self.skip_unexpected_until(&[&Self::STARTERS_STMT]);
    //             continue;
    //         }
    //
    //         self.parse_stmt();
    //     }
    //
    //     self.close(m, CSTTreeKind::File);
    // }
    //
    // const STARTERS_STMT: TokenSet = token_set! {
    //     tokens = [],
    //     includes = [
    //         CSTParser::STARTERS_FLOW_STMT,
    //     ]
    // };
    // /// ```bnf
    // /// <stmt> ::= <moduleDecl>
    // ///          | <useDecl>
    // ///          | <declStmt>
    // ///          | <flowStmt>
    // ///          ;
    // /// ```
    // fn parse_stmt(&mut self) {
    //     match () {
    //         _ if self.at_set(&Self::STARTERS_FLOW_STMT) => {
    //             self.parse_flow_stmt();
    //         }
    //         _ => unreachable!(),
    //     }
    // }
    //
    // const STARTERS_FLOW_STMT: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::Return,
    //         TokenKind::Break,
    //         TokenKind::Continue,
    //     ],
    //     includes = [
    //         CSTParser::STARTERS_EXPR,
    //     ]
    // };
    // /// ```bnf
    // /// <flowStmt> ::= "return" <expr>?
    // ///              | "break"
    // ///              | "continue"
    // ///              | <deferStmt>
    // ///              | <throwStmt>
    // ///              | <forStmt>
    // ///              | <whileStmt>
    // ///              | <loopStmt>
    // ///              | <block>
    // ///              | <expr>
    // ///              ;
    // /// ```
    // fn parse_flow_stmt(&mut self) {
    //     match self.current() {
    //         TokenKind::Return => {
    //             let m = self.open();
    //             self.expect(TokenKind::Return);
    //             if self.at_set(&Self::STARTERS_EXPR) {
    //                 self.parse_expr();
    //             }
    //             self.close(m, CSTTreeKind::ReturnStmt);
    //         }
    //         TokenKind::Break => {
    //             let m = self.open();
    //             self.expect(TokenKind::Break);
    //             self.close(m, CSTTreeKind::BreakStmt);
    //         }
    //         TokenKind::Continue => {
    //             let m = self.open();
    //             self.expect(TokenKind::Continue);
    //             self.close(m, CSTTreeKind::ContinueStmt);
    //         }
    //         _ if self.at_set(&Self::STARTERS_EXPR) => {
    //             self.parse_expr();
    //         }
    //         _ => unreachable!(),
    //     }
    // }
    //
    // const STARTERS_EXPR: TokenSet = token_set! {
    //     tokens = [],
    //     includes = [
    //         // TODO: preop operators
    //         CSTParser::STARTERS_LITERAL,
    //         TOKEN_PREFIX_OPERATORS,
    //     ]
    // };
    // /// NOTE: precedence rules aren't modeled in the grammar spec below
    // ///
    // /// ```bnf
    // /// <expr> ::= <prefixOp> <expr>
    // ///          | <expr> <infixOp> <expr>
    // ///          | <expr> <postfixOp>
    // ///          | <expr> <postfixStmtExpr>
    // ///          | <expr> <postfixLooseExpr>
    // ///          | <expr> <callExpr>
    // ///          | <expr> <indexExpr>
    // ///          | <expr> <memberExpr>
    // ///          | <literal>
    // ///          ;
    // /// ```
    // fn parse_expr(&mut self) {
    //     self.parse_expr_rec(0);
    // }
    //
    // fn parse_expr_rec(&mut self, min_bp: i8) {
    //     // 1) prefix operators and literal parsing
    //     let mut lhs = if self.at_set(&TOKEN_PREFIX_OPERATORS) {
    //         let m = self.open();
    //         let (_, right_bp) = Self::binding_power_prefix(self.current());
    //         self.advance();
    //         self.parse_expr_rec(right_bp);
    //         self.close(m, CSTTreeKind::PrefixOpExpr)
    //     } else {
    //         self.parse_delimited()
    //     };
    //
    //     // 2) postfix and infix operator parsing
    //     loop {
    //         if let Some((left_bp, _)) = Self::binding_power_postfix(self.current()) {
    //             if left_bp < min_bp {
    //                 break;
    //             }
    //
    //             match self.current() {
    //                 TokenKind::If | TokenKind::Catch | TokenKind::For => {
    //                     panic!("postfix if/catch/for not implemented yet")
    //                 }
    //                 _ => {
    //                     let m = self.open_before(&lhs);
    //                     self.advance();
    //                     self.close(m, CSTTreeKind::PostfixOpExpr);
    //                     continue;
    //                 }
    //             }
    //         }
    //
    //         if let Some((left_bp, right_bp)) = Self::binding_power_infix(self.current()) {
    //             if left_bp < min_bp {
    //                 break;
    //             }
    //
    //             let m = self.open_before(&lhs);
    //             self.advance();
    //             self.parse_expr_rec(right_bp);
    //             lhs = self.close(m, CSTTreeKind::InfixOpExpr);
    //
    //             continue;
    //         }
    //
    //         break;
    //     }
    // }
    //
    // fn binding_power_prefix(operator: TokenKind) -> ((), i8) {
    //     match operator {
    //         TokenKind::Await => ((), 13), // TODO: this might be too tight??
    //         TokenKind::Add | TokenKind::Sub => ((), 13),
    //         TokenKind::Not => ((), 13),
    //         TokenKind::BitNot => ((), 13),
    //         TokenKind::TripleDot => ((), 0),
    //         _ => unreachable!(),
    //     }
    // }
    //
    // fn binding_power_infix(operator: TokenKind) -> Option<(i8, i8)> {
    //     match operator {
    //         TokenKind::Or => Some((1, 2)),
    //         TokenKind::And => Some((2, 3)),
    //
    //         TokenKind::Eq
    //         | TokenKind::Neq
    //         | TokenKind::Lt
    //         | TokenKind::Gt
    //         | TokenKind::Lte
    //         | TokenKind::Gte
    //         | TokenKind::Instanceof
    //         | TokenKind::Is
    //         | TokenKind::In => Some((3, 4)),
    //
    //         // right-associative
    //         TokenKind::QuestionMarkColon => Some((5, 4)),
    //
    //         TokenKind::BitOr => Some((5, 6)),
    //         TokenKind::BitXor => Some((6, 7)),
    //         TokenKind::BitAnd => Some((7, 8)),
    //
    //         TokenKind::RangeLessThan | TokenKind::DoubleDot => Some((8, 9)),
    //
    //         TokenKind::Add | TokenKind::Sub => Some((9, 10)),
    //         TokenKind::Mul | TokenKind::Div | TokenKind::Mod => Some((10, 11)),
    //
    //         // right-associative
    //         TokenKind::Pow => Some((12, 11)),
    //
    //         TokenKind::BitLeftShift
    //         | TokenKind::BitRightShift
    //         | TokenKind::BitUnsignedRightShift => Some((12, 13)),
    //         _ => None,
    //     }
    // }
    //
    // fn binding_power_postfix(operator: TokenKind) -> Option<(i8, ())> {
    //     match operator {
    //         TokenKind::DoubleNot => Some((14, ())),
    //         TokenKind::If => Some((0, ())),
    //         TokenKind::Catch => Some((0, ())),
    //         TokenKind::For => Some((0, ())),
    //         _ => None,
    //     }
    // }
    //
    // fn parse_delimited(&mut self) -> MarkClosed {
    //     let mut lhs = self.parse_literal();
    //
    //     loop {
    //         match self.current() {
    //             TokenKind::Dot => {
    //                 let m = self.open_before(&lhs);
    //                 self.advance();
    //                 self.parse_identifier_or_keyword();
    //                 lhs = self.close(m, CSTTreeKind::MemberExpr);
    //             }
    //             TokenKind::QuestionMarkDot => {
    //                 let m = self.open_before(&lhs);
    //                 self.advance();
    //                 self.parse_identifier_or_keyword();
    //                 lhs = self.close(m, CSTTreeKind::NullableMemberExpr);
    //             }
    //             _ => return lhs,
    //         }
    //     }
    // }
    //
    // const STARTERS_LITERAL: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::Integer,
    //         TokenKind::Float,
    //         TokenKind::String,
    //         TokenKind::FormatStringPart,
    //         TokenKind::True,
    //         TokenKind::False,
    //         TokenKind::Null,
    //         TokenKind::Identifier,
    //         TokenKind::LeftParen,
    //     ],
    //     includes = [
    //         CSTParser::STARTERS_LAMBDA,
    //     ]
    // };
    // /// ```bnf
    // /// <literal> ::= <parenExpr>
    // ///             | <integer>
    // ///             | <float>
    // ///             | <string>
    // ///             | <fstring>
    // ///             | "true" | "false"
    // ///             | "null"
    // ///             | <id>
    // ///             | <lambda>
    // ///             | <tuple>
    // ///             | <list>
    // ///             | <map>
    // ///             | <ifStmt>
    // ///             | <whenStmt>
    // ///             | <tryStmt>
    // ///             | <selectStmt>
    // ///             ;
    // /// ```
    // fn parse_literal(&mut self) -> MarkClosed {
    //     match self.current() {
    //         // identifiers
    //         TokenKind::Identifier => {
    //             let m = self.open();
    //             self.advance();
    //             self.close(m, CSTTreeKind::Identifier)
    //         }
    //
    //         // literals
    //         TokenKind::Integer
    //         | TokenKind::Float
    //         | TokenKind::True
    //         | TokenKind::False
    //         | TokenKind::Null
    //         | TokenKind::String => {
    //             let m = self.open();
    //             self.advance();
    //             self.close(m, CSTTreeKind::Literal)
    //         }
    //
    //         // ( expr )
    //         TokenKind::LeftParen => {
    //             let m = self.open();
    //             self.advance();
    //
    //             if self.at(TokenKind::RightParen) {
    //                 self.expect(TokenKind::RightParen);
    //                 return self.close(m, CSTTreeKind::ParenExpr);
    //             }
    //
    //             self.parse_expr();
    //
    //             if self.at(TokenKind::RightParen) {
    //                 self.expect(TokenKind::RightParen);
    //                 return self.close(m, CSTTreeKind::ParenExpr);
    //             }
    //
    //             while self.at(TokenKind::Comma) {
    //                 self.advance();
    //
    //                 if self.at_set(&Self::STARTERS_EXPR) {
    //                     self.parse_expr();
    //                 }
    //             }
    //
    //             self.expect(TokenKind::RightParen);
    //             self.close(m, CSTTreeKind::TupleLiteral)
    //         }
    //
    //         // format strings
    //         TokenKind::FormatStringPart => self.parse_fstring(),
    //
    //         _ if self.at_set(&Self::STARTERS_LAMBDA) => self.parse_lambda(),
    //
    //         _ => {
    //             let m = self.open();
    //             if !self.eof() {
    //                 self.advance_with_error("unexpected token");
    //             }
    //             self.close(m, CSTTreeKind::Error)
    //         }
    //     }
    // }
    //
    // fn parse_identifier_or_keyword(&mut self) -> MarkClosed {
    //     match self.current() {
    //         TokenKind::Identifier => self.parse_literal(),
    //         _ if self.at_set(&TOKEN_LANGUAGE_KEYWORDS) => {
    //             let m = self.open();
    //             self.advance();
    //             self.close(m, CSTTreeKind::Identifier)
    //         }
    //         _ => {
    //             let m = self.open();
    //             if !self.eof() {
    //                 self.advance_with_error("unexpected token");
    //             }
    //             self.close(m, CSTTreeKind::Error)
    //         }
    //     }
    // }
    //
    // /// ```bnf
    // /// <fstring> ::= <fstringStart> (<expr> | <fstringPart>)* <fstringEnd> ;
    // /// ```
    // fn parse_fstring(&mut self) -> MarkClosed {
    //     let m = self.open();
    //     self.advance();
    //     while !self.eof() {
    //         self.parse_expr();
    //
    //         if self.eof() {
    //             self.unexpected_token(&[TokenKind::String]);
    //             break;
    //         }
    //
    //         match self.current() {
    //             TokenKind::FormatStringPart => {
    //                 self.advance();
    //                 continue;
    //             }
    //             TokenKind::String => {
    //                 self.advance();
    //                 break;
    //             }
    //             _ => {
    //                 self.unexpected_token(&[TokenKind::String]);
    //                 break;
    //             }
    //         }
    //     }
    //     self.close(m, CSTTreeKind::FormatString)
    // }
    //
    // const STARTERS_LAMBDA: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::LeftBrace,
    //         TokenKind::BitOr,
    //         TokenKind::Or,
    //     ],
    //     includes = []
    // };
    // /// ```bnf
    // /// <lambda> ::= <lambdaExprForm>
    // ///            | <lambdaBlockForm>
    // ///            ;
    // /// ```
    // fn parse_lambda(&mut self) -> MarkClosed {
    //     match self.current() {
    //         TokenKind::BitOr | TokenKind::Or => self.parse_lambda_expr_form(),
    //         TokenKind::LeftBrace => self.parse_lambda_block_form(),
    //         _ => unreachable!(),
    //     }
    // }
    //
    // /// ```bnf
    // /// <lambdaExprForm> ::= <lambdaParamDecl> <expr>
    // /// ```
    // fn parse_lambda_expr_form(&mut self) -> MarkClosed {
    //     let m = self.open();
    //     self.parse_lambda_param_decl();
    //     self.parse_expr();
    //     self.close(m, CSTTreeKind::LambdaExprFormLiteral)
    // }
    //
    // /// ```bnf
    // /// <lambdaBlockForm> ::= "{" <lambdaParamDecl>? stmt* "}"
    // /// ```
    // fn parse_lambda_block_form(&mut self) -> MarkClosed {
    //     let m = self.open();
    //     self.expect(TokenKind::LeftBrace);
    //
    //     if self.at(TokenKind::BitOr) || self.at(TokenKind::Or) {
    //         self.parse_lambda_param_decl();
    //     }
    //
    //     while !self.eof() {
    //         if !self.at_set(&Self::STARTERS_STMT) {
    //             break;
    //         }
    //
    //         self.parse_stmt();
    //     }
    //
    //     self.expect(TokenKind::RightBrace);
    //
    //     self.close(m, CSTTreeKind::LambdaBlockFormLiteral)
    // }
    //
    // /// ```bnf
    // /// <lambdaParamDecl> ::= "|" <fnParam> ("," <fnParam>)* "|" ;
    // /// ```
    // fn parse_lambda_param_decl(&mut self) {
    //     let m = self.open();
    //
    //     match self.current() {
    //         TokenKind::BitOr => {
    //             self.expect(TokenKind::BitOr);
    //
    //             if !self.at(TokenKind::BitOr) {
    //                 panic!("params not implemented yet")
    //             }
    //
    //             self.expect(TokenKind::BitOr);
    //             self.close(m, CSTTreeKind::LambdaParameterDecl);
    //         }
    //         TokenKind::Or => {
    //             self.expect(TokenKind::Or);
    //             self.close(m, CSTTreeKind::LambdaParameterDecl);
    //         }
    //         _ => unreachable!(),
    //     }
    // }
    //
    // fn at_set(&mut self, types: &TokenSet) -> bool {
    //     types.contains(&self.current())
    // }
    //
    // fn expect(&mut self, kind: TokenKind) {
    //     if self.eat(kind) {
    //         return;
    //     }
    //     self.unexpected_token(&[kind]);
    // }
    //
    // fn advance_with_error(&mut self, error: &str) {
    //     let m = self.open();
    //     let location = self.nth_token(0).location.clone();
    //     self.diagnostic_context
    //         .error(error, &location, vec![], vec![]);
    //     self.advance();
    //     self.close(m, CSTTreeKind::Error);
    // }
    //
    // // consume unexpected tokens until one is found that is contained within
    // // the terminator set
    // // if at least one unexpected token was consumed, a diagnostic error is reported
    // fn skip_unexpected_until(&mut self, terminators: &[&TokenSet]) {
    //     let terminators = union_token_sets(terminators);
    //
    //     // skip unexpected tokens until we reach either an expected or terminator token
    //     while !self.eof() {
    //         // found an expected token, stop skipping
    //         if terminators.contains(&self.current()) {
    //             break;
    //         }
    //
    //         self.advance_with_error("unexpected token");
    //     }
    //
    //     if self.eof() {
    //         let current_loc = self.current_token().location.clone();
    //         self.diagnostic_context.error(
    //             "unexpected end of file",
    //             &current_loc,
    //             vec![],
    //             vec![],
    //         );
    //     }
    // }
    //
    // fn unexpected_token(&mut self, expected: &[TokenKind]) {
    //     let (kind, location) = {
    //         let current = self.nth_token(0);
    //         (current.kind, current.location.clone())
    //     };
    //
    //     self.diagnostic_context.error(
    //         format!("expected token(s) {:?}, got {:?}", expected, kind).as_str(),
    //         &location,
    //         vec![],
    //         vec![],
    //     );
    // }

    fn build_tree(&mut self) -> CSTTree {
        let mut tokens = self.tokens.into_iter();
        let events = &mut self.events;
        let mut stack: Vec<CSTTree> = Vec::new();

        assert!(matches!(events.pop(), Some(Event::Close)));

        for event in events {
            match event {
                // starting a new node, push an empty tree to the stack
                Event::Open { kind } => {
                    let tree = CSTTree {
                        kind: *kind,
                        children: Vec::new(),
                    };
                    stack.push(tree);
                }

                // a tree is done,
                // pop it off the stack and append to a new current tree
                Event::Close => {
                    let tree = stack.pop().unwrap();
                    let current = stack.last_mut().unwrap();
                    current.children.push(CSTNode::Tree(tree));
                }

                // consume a token and append it to the current tree
                Event::Advance => {
                    let token = tokens.next().unwrap().clone();
                    let tree = stack.last_mut().unwrap();
                    tree.children.push(CSTNode::Token(token));
                }
            }
        }

        assert_eq!(stack.len(), 1);
        assert!(tokens.next().is_none());
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

        let mut tokenizer = Tokenizer::new(source, file_id, &mut context);
        let tokens = tokenizer.collect();

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
