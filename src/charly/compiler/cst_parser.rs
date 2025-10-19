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
use once_cell::sync::Lazy;

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

/// reference: https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html
pub struct CSTParser<'a> {
    tokens: &'a Vec<Token>,
    diagnostic_context: &'a mut DiagnosticContext,
    pos: usize,
    fuel: u32,
    events: Vec<Event>,
}

#[macro_export]
macro_rules! token_set {
    (
        tokens = [ $( $token:expr ),* $(,)? ],
        includes = [ $( $set:expr ),* $(,)? ]
    ) => {
        ::once_cell::sync::Lazy::new(|| {
            let mut set = Vec::new();
            $(
                set.extend_from_slice($set.as_slice());
            )*
            $(
                set.push($token);
            )*
            set
        })
    };
}

type TokenSet = Lazy<Vec<TokenKind>>;

impl<'a> CSTParser<'a> {
    pub fn new(tokens: &'a Vec<Token>, diagnostic_context: &'a mut DiagnosticContext) -> Self {
        Self {
            tokens,
            diagnostic_context,
            pos: 0,
            fuel: 256,
            events: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> CSTTree {
        self.parse_file();
        self.build_tree()
    }

    /// file
    ///     : stmt*
    ///     ;
    fn parse_file(&mut self) {}

    /// stmt
    ///     : moduleDecl
    ///     : useDecl
    ///     : declStmt
    ///     : flowStmt
    ///     : block
    ///     ;
    fn parse_stmt(&mut self) {}

    /// declStmt
    ///     : fnDecl
    ///     : variableDecl
    ///     : structDecl
    ///     : interfaceDecl
    ///     : typealiasDecl
    ///     ;
    fn parse_decl_stmt(&mut self) {}

    // /// file
    // ///     : statement*
    // ///     ;
    // fn parse_file(&mut self) {
    //     let m = self.open();
    //     while !self.eof() {
    //         self.skip_unexpected_until(&[&Self::STARTERS_STATEMENT]);
    //         self.parse_statement();
    //     }
    //     self.close(m, CSTTreeKind::File);
    // }
    //
    // /// statement
    // ///     : moduleDecl
    // ///     : useDecl
    // ///     : structDecl
    // ///     : fnDecl
    // ///     : variableDecl
    // ///     : controlStmt
    // ///     : ifStmt
    // ///     : whileStmt
    // ///     : expr
    // ///     ;
    // const STARTERS_STATEMENT: TokenSet = token_set! {
    //     tokens = [],
    //     includes = [
    //         CSTParser::STARTERS_MODULE_DECL,
    //         CSTParser::STARTERS_USE_DECL,
    //         CSTParser::STARTERS_STRUCT_DECL,
    //         CSTParser::STARTERS_FN_DECL,
    //         CSTParser::STARTERS_VARIABLE_DECL,
    //         CSTParser::STARTERS_CONTROL_STMT,
    //         CSTParser::STARTERS_IF_STMT,
    //         CSTParser::STARTERS_WHILE_STMT,
    //         CSTParser::STARTERS_EXPR,
    //     ]
    // };
    // fn parse_statement(&mut self) {
    //     let m = self.open();
    //
    //     let current = self.current();
    //     match current {
    //         TokenKind::Module => self.parse_module_decl(&Self::STARTERS_STATEMENT),
    //         TokenKind::Use => self.parse_use_decl(),
    //         TokenKind::Struct => self.parse_struct_decl(),
    //         TokenKind::Fn => self.parse_fn_decl(),
    //         TokenKind::Let => self.parse_variable_decl(),
    //         _ if Self::STARTERS_CONTROL_STMT.contains(&current) => self.parse_control_stmt(),
    //         TokenKind::If => self.parse_if_stmt(),
    //         TokenKind::While => self.parse_while_stmt(),
    //         _ => self.parse_expr(),
    //     }
    //
    //     self.close(m, CSTTreeKind::Statement);
    // }
    //
    // /// moduleDecl
    // ///     : "module" modulePathSpec
    // ///     ;
    // const STARTERS_MODULE_DECL: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::Module,
    //     ],
    //     includes = []
    // };
    // fn parse_module_decl(&mut self, terminators: &TokenSet) {
    //     let m = self.open();
    //
    //     self.expect(TokenKind::Module);
    //     self.skip_unexpected_until(&[&Self::STARTERS_MODULE_PATH_SPEC, terminators]);
    //
    //     if self.at_set(Self::STARTERS_MODULE_PATH_SPEC) {
    //         self.parse_module_path_spec();
    //     }
    //
    //     self.close(m, CSTTreeKind::ModuleDecl);
    // }
    //
    // /// modulePathSpec
    // ///     : id ("::" id)*
    // ///     ;
    // const STARTERS_MODULE_PATH_SPEC: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::Identifier,
    //     ],
    //     includes = []
    // };
    // fn parse_module_path_spec(&mut self) {}
    //
    // /// useDecl
    // ///     : "use" usePathSpec ("as" id)?
    // ///     ;
    // const STARTERS_USE_DECL: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::Use,
    //     ],
    //     includes = []
    // };
    // fn parse_use_decl(&mut self) {}
    //
    // /// usePathSpec
    // ///     : (usePathSpecEntry "::")*
    // ///     ;
    // const STARTERS_USE_PATH_SPEC: TokenSet = token_set! {
    //     tokens = [],
    //     includes = [
    //         CSTParser::STARTERS_USE_PATH_SPEC_ENTRY,
    //     ]
    // };
    // fn parse_use_path_spec(&mut self) {}
    //
    // /// usePathSpecEntry
    // ///     : id
    // ///     : "{" (id ",")* "}"
    // ///     : "*"
    // ///     ;
    // const STARTERS_USE_PATH_SPEC_ENTRY: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::Identifier,
    //         TokenKind::LeftBrace,
    //         TokenKind::Mul,
    //     ],
    //     includes = []
    // };
    // fn parse_use_path_spec_entry(&mut self) {}
    //
    // /// structDecl
    // ///     : "struct" id structMemberList? structBody?
    // ///     ;
    // const STARTERS_STRUCT_DECL: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::Struct,
    //     ],
    //     includes = []
    // };
    // fn parse_struct_decl(&mut self) {}
    //
    // /// structMemberList
    // ///     : "(" (variableDecl ",")* ")"
    // ///     ;
    // const STARTERS_STRUCT_MEMBER_LIST: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::LeftParen,
    //     ],
    //     includes = []
    // };
    // fn parse_struct_member_list(&mut self) {}
    //
    // /// structBody
    // ///     : "{" structBodyStmt* "}"
    // ///     ;
    // const STARTERS_STRUCT_BODY: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::LeftBrace,
    //     ],
    //     includes = []
    // };
    // fn parse_struct_body(&mut self) {}
    //
    // /// structBodyStmt
    // ///     : variableDecl
    // ///     : fnDecl
    // ///     : "init" block
    // ///     ;
    // const STARTERS_STRUCT_BODY_STMT: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::Init,
    //     ],
    //     includes = [
    //         CSTParser::STARTERS_VARIABLE_DECL,
    //         CSTParser::STARTERS_FN_DECL,
    //     ]
    // };
    // fn parse_struct_body_stmt(&mut self) {}
    //
    // /// fnDecl
    // ///     : "fn" fnName fnParamList fnReturnDecl fnBody
    // ///     ;
    // const STARTERS_FN_DECL: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::Fn,
    //     ],
    //     includes = []
    // };
    // fn parse_fn_decl(&mut self) {}
    //
    // /// fnName
    // ///     : (id ".")? id
    // ///     ;
    // const STARTERS_FN_NAME: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::Identifier,
    //     ],
    //     includes = []
    // };
    // fn parse_fn_name(&mut self) {}
    //
    // /// fnParamList
    // ///     : "(" (fnParam ",")* ")"
    // ///     ;
    // const STARTERS_FN_PARAM_LIST: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::LeftParen,
    //     ],
    //     includes = []
    // };
    // fn parse_fn_param_list(&mut self) {}
    //
    // /// fnParam
    // ///     : id ":" typeExpr
    // ///     ;
    // const STARTERS_FN_PARAM: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::Identifier,
    //     ],
    //     includes = []
    // };
    // fn parse_fn_param(&mut self) {}
    //
    // /// fnReturnDecl
    // ///     : "->" typeExpr
    // ///     ;
    // const STARTERS_FN_RETURN_DECL: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::RightArrow,
    //     ],
    //     includes = []
    // };
    // fn parse_fn_return_decl(&mut self) {}
    //
    // /// fnBody
    // ///     : "=" expr
    // ///     : block
    // ///     ;
    // const STARTERS_FN_BODY: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::Assign,
    //     ],
    //     includes = [
    //         CSTParser::STARTERS_BLOCK,
    //     ]
    // };
    // fn parse_fn_body(&mut self) {}
    //
    // /// variableDecl
    // ///     : "let" "mut"? id ":" typeExpr "=" expr
    // ///     ;
    // const STARTERS_VARIABLE_DECL: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::Let,
    //     ],
    //     includes = []
    // };
    // fn parse_variable_decl(&mut self) {}
    //
    // /// controlStmt
    // ///     : "return" expr?
    // ///     : "break"
    // ///     : "continue"
    // ///     ;
    // const STARTERS_CONTROL_STMT: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::Return,
    //         TokenKind::Break,
    //         TokenKind::Continue,
    //     ],
    //     includes = []
    // };
    // fn parse_control_stmt(&mut self) {}
    //
    // /// ifStmt
    // ///     : "if" expr expr ("else" (expr | ifStmt))?
    // ///     ;
    // const STARTERS_IF_STMT: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::If
    //     ],
    //     includes = []
    // };
    // fn parse_if_stmt(&mut self) {}
    //
    // /// whileStmt
    // ///     : "while" expr expr
    // ///     ;
    // const STARTERS_WHILE_STMT: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::While
    //     ],
    //     includes = []
    // };
    // fn parse_while_stmt(&mut self) {}
    //
    // /// typeExpr
    // ///     : modulePathSpec "?"?
    // ///     ;
    // const STARTERS_TYPE_EXPR: TokenSet = token_set! {
    //     tokens = [],
    //     includes = [
    //         CSTParser::STARTERS_MODULE_PATH_SPEC
    //     ]
    // };
    // fn parse_type_expr(&mut self) {}
    //
    // /// expr
    // ///     : literal
    // ///     : expr operator expr
    // ///     : operator expr
    // ///     : "(" expr ")"
    // ///     : block
    // ///     ;
    // const STARTERS_UNARY_OPERATOR: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::Add,
    //         TokenKind::Sub,
    //         TokenKind::Not,
    //         TokenKind::BitNot,
    //     ],
    //     includes = []
    // };
    // const STARTERS_EXPR: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::LeftParen
    //     ],
    //     includes = [
    //         CSTParser::STARTERS_LITERAL
    //     ]
    // };
    // fn parse_expr(&mut self) {}
    //
    // /// block
    // ///     : "{" statement* "}"
    // ///     ;
    // const STARTERS_BLOCK: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::LeftBrace,
    //     ],
    //     includes = []
    // };
    // fn parse_block(&mut self) {}
    //
    // /// literal
    // ///     : integer
    // ///     : float
    // ///     : string
    // ///     : boolean
    // ///     : null
    // ///     : identifier
    // ///     ;
    // const STARTERS_LITERAL: TokenSet = token_set! {
    //     tokens = [
    //         TokenKind::Integer,
    //         TokenKind::Float,
    //         TokenKind::String,
    //         TokenKind::True,
    //         TokenKind::Null,
    //         TokenKind::Identifier,
    //     ],
    //     includes = []
    // };
    // fn parse_literal(&mut self) {}

    // fn parse_file(&mut self) {
    //     let m = self.open();
    //
    //     while !self.eof() {
    //         match self.current() {
    //             TokenKind::Module => self.parse_module_decl(),
    //             TokenKind::Let | TokenKind::Const => self.parse_variable_decl(),
    //             _ => self.parse_expression(),
    //         }
    //     }
    //
    //     self.close(m, CSTTreeKind::File);
    // }
    //
    // fn parse_module_decl(&mut self) {
    //     assert_eq!(self.current(), TokenKind::Module);
    //     let m = self.open();
    //     self.expect(TokenKind::Module);
    //     self.parse_name(true);
    //     self.close(m, CSTTreeKind::ModuleDeclaration);
    // }
    //
    // fn parse_variable_decl(&mut self) {
    //     let m = self.open();
    //     match self.current() {
    //         TokenKind::Let | TokenKind::Const => {}
    //         _ => panic!("unexpected token"),
    //     }
    //     self.advance();
    //
    //     self.parse_name(false);
    //
    //     if self.eat(TokenKind::Assign) {
    //         self.parse_expression();
    //     }
    //
    //     self.close(m, CSTTreeKind::Declaration);
    // }
    //
    // fn parse_expression(&mut self) {
    //     self.parse_expression_rec(TokenKind::Eof);
    // }
    //
    // fn parse_expression_rec(&mut self, left: TokenKind) {
    //     let mut lhs = self.parse_expression_delimited();
    //
    //     while self.at(TokenKind::LeftParen) {
    //         let m = self.open_before(&lhs);
    //         self.parse_argument_list();
    //         lhs = self.close(m, CSTTreeKind::CallOp)
    //     }
    //
    //     loop {
    //         let right = self.current();
    //         if Self::right_operator_is_tighter(left, right) {
    //             let m = self.open_before(&lhs);
    //             self.advance();
    //             self.parse_expression_rec(right);
    //             lhs = self.close(m, CSTTreeKind::BinaryOp);
    //         } else {
    //             break;
    //         }
    //     }
    // }
    //
    // fn parse_argument_list(&mut self) {
    //     assert!(self.at(TokenKind::LeftParen));
    //     let m = self.open();
    //
    //     self.expect(TokenKind::LeftParen);
    //     while !self.at(TokenKind::RightParen) && !self.eof() {
    //         self.parse_argument();
    //     }
    //     self.expect(TokenKind::RightParen);
    //
    //     self.close(m, CSTTreeKind::CallArgumentList);
    // }
    //
    // fn parse_argument(&mut self) {
    //     let m = self.open();
    //     self.parse_expression();
    //
    //     if !self.at(TokenKind::RightParen) {
    //         self.expect(TokenKind::Comma);
    //     }
    //
    //     self.close(m, CSTTreeKind::CallArgument);
    // }
    //
    // fn parse_expression_delimited(&mut self) -> MarkClosed {
    //     match self.current() {
    //         // literals
    //         TokenKind::Identifier
    //         | TokenKind::Integer
    //         | TokenKind::Float
    //         | TokenKind::String
    //         | TokenKind::True
    //         | TokenKind::False
    //         | TokenKind::Null
    //         | TokenKind::Super => {
    //             let m = self.open();
    //             self.advance();
    //             self.close(m, CSTTreeKind::Literal)
    //         }
    //
    //         // ( expr )
    //         TokenKind::LeftParen => {
    //             let m = self.open();
    //             self.advance();
    //             self.parse_expression();
    //             self.expect(TokenKind::RightParen);
    //             self.close(m, CSTTreeKind::Paren)
    //         }
    //
    //         // format strings
    //         TokenKind::FormatStringPart => todo!("format strings not implemented"),
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
    // fn parse_name(&mut self, allow_qualified: bool) {
    //     let m = self.open();
    //     self.expect(TokenKind::Identifier);
    //     if allow_qualified {
    //         while self.eat(TokenKind::Dot) {
    //             self.expect(TokenKind::Identifier);
    //         }
    //     }
    //
    //     self.close(m, CSTTreeKind::Name);
    // }

    fn right_operator_is_tighter(left: TokenKind, right: TokenKind) -> bool {
        /// the further down in the list an operator is,
        /// the tighter it binds
        ///
        /// operators on the same level have the same precedence
        fn tightness(kind: TokenKind) -> Option<usize> {
            use TokenKind::*;

            let list = [
                [Or].as_slice(),
                [And].as_slice(),
                [QuestionMarkColon].as_slice(),
                [Eq, Neq].as_slice(),
                [Lt, Gt, Lte, Gte].as_slice(),
                [BitLeftShift, BitRightShift, BitUnsignedRightShift].as_slice(),
                [BitOr].as_slice(),
                [BitXor].as_slice(),
                [BitAnd].as_slice(),
                [Add, Sub].as_slice(),
                [Mul, Div, Mod].as_slice(),
                [Pow].as_slice(),
            ];
            list.iter().position(|level| level.contains(&kind))
        }

        let Some(right) = tightness(right) else {
            return false;
        };
        let Some(left) = tightness(left) else {
            assert_eq!(left, TokenKind::Eof);
            return true;
        };
        right > left
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

    fn close(&mut self, mark: MarkOpened, kind: CSTTreeKind) -> MarkClosed {
        self.events[mark.index] = Event::Open { kind };
        self.events.push(Event::Close);
        MarkClosed { index: mark.index }
    }

    fn open_before(&mut self, mark: &MarkClosed) -> MarkOpened {
        let mark = MarkOpened { index: mark.index };
        let event = Event::Open {
            kind: CSTTreeKind::Error,
        };
        self.events.insert(mark.index, event);
        mark
    }

    fn advance(&mut self) {
        assert!(!self.eof());
        self.fuel = 256;
        self.events.push(Event::Advance);
        self.pos += 1;
    }

    fn eof(&mut self) -> bool {
        self.current() == TokenKind::Eof
    }

    fn nth_token(&self, lookahead: usize) -> &Token {
        let result = self.tokens.get(self.pos + lookahead);
        match result {
            Some(token) => token,
            None => self.tokens.last().unwrap(),
        }
    }

    fn nth(&mut self, lookahead: usize) -> TokenKind {
        if self.fuel == 0 {
            panic!("parser ran out of fuel");
        }
        self.fuel = self.fuel - 1;
        self.nth_token(lookahead).kind
    }

    fn current(&mut self) -> TokenKind {
        self.nth(0)
    }

    fn current_token(&self) -> &Token {
        self.nth_token(0)
    }

    fn at(&mut self, kind: TokenKind) -> bool {
        self.current() == kind
    }

    fn at_set(&mut self, types: TokenSet) -> bool {
        types.contains(&self.current())
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokenKind) {
        if self.eat(kind) {
            return;
        }
        self.unexpected_token(&[kind]);
    }

    fn advance_with_error(&mut self, error: &str) {
        let m = self.open();
        let location = self.nth_token(0).location.clone();
        self.diagnostic_context
            .error(error, &location, vec![], vec![]);
        self.advance();
        self.close(m, CSTTreeKind::Error);
    }

    // consume unexpected tokens until one is found that is contained within
    // the terminator set
    // if at least one unexpected token was consumed, a diagnostic error is reported
    fn skip_unexpected_until(&mut self, terminators: &[&TokenSet]) {
        // merge the tokensets
        let terminators = {
            let mut result = Vec::new();
            for set in terminators {
                for token in set.iter() {
                    result.push(*token);
                }
            }
            result
        };

        let mut skipped_unexpected_tokens = 0;
        let start_loc = self.current_token().location.clone();
        let mut end_loc = start_loc.clone();
        while !self.eof() {
            // found an expected token, stop skipping
            if terminators.contains(&self.current()) {
                break;
            }

            end_loc = self.current_token().location.clone();
            skipped_unexpected_tokens += 1;
            self.advance();
        }

        if skipped_unexpected_tokens > 0 {
            let merged_location = start_loc.merge(&end_loc);
            self.diagnostic_context.error(
                format!("unexpected tokens, expected {:?}", terminators).as_str(),
                &merged_location,
                vec![],
                vec![],
            );
            return;
        }

        if self.eof() {
            let current_loc = self.current_token().location.clone();
            self.diagnostic_context.error(
                format!("unexpected end of file, expected {:?}", terminators).as_str(),
                &current_loc,
                vec![],
                vec![],
            );
            return;
        }
    }

    fn unexpected_token(&mut self, expected: &[TokenKind]) {
        let (kind, location) = {
            let current = self.nth_token(0);
            (current.kind, current.location.clone())
        };

        self.diagnostic_context.error(
            format!("expected token(s) {:?}, got {:?}", expected, kind).as_str(),
            &location,
            vec![],
            vec![],
        );
    }

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
        assert_eq!(tokens.next().unwrap().kind, TokenKind::Eof);
        assert!(tokens.next().is_none());
        stack.pop().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use crate::charly::compiler::cst_parser::CSTParser;
    use crate::charly::compiler::tokenizer::{TokenDetailLevel, Tokenizer};
    use crate::charly::test_utils::validate_expected_diagnostics;
    use crate::charly::utils::ascii_tree::IndentStyle;
    use crate::charly::utils::cst_printer::CstPrinter;
    use crate::charly::utils::diagnostics::DiagnosticController;
    use clap::builder::TypedValueParser;
    use pretty_assertions::assert_eq;
    use std::path::PathBuf;

    #[track_caller]
    fn assert_tree(source: &str, expected_tree: &str, expected_diagnostics: &[&str]) {
        let mut controller = DiagnosticController::new();
        let path = PathBuf::from("test");
        let file_id = controller.register_file(&path, source);
        let mut context = controller.get_or_create_context(file_id);

        let mut tokenizer = Tokenizer::new(source, file_id, &mut context);
        let tokens = tokenizer.tokenize(TokenDetailLevel::NoWhitespaceAndComments);

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
