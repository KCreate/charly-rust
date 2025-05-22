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

use std::cell::Cell;

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

/// reference: https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html
pub struct CSTParser<'a> {
    tokens: &'a Vec<Token>,
    diagnostic_context: &'a mut DiagnosticContext,
    pos: usize,
    fuel: Cell<u32>,
    events: Vec<Event>,
}

impl<'a> CSTParser<'a> {
    pub fn new(tokens: &'a Vec<Token>, diagnostic_context: &'a mut DiagnosticContext) -> Self {
        Self {
            tokens,
            diagnostic_context,
            pos: 0,
            fuel: Cell::new(256),
            events: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> CSTTree {
        self.parse_file();
        self.build_tree()
    }

    fn parse_file(&mut self) {
        let m = self.open();

        while !self.eof() {
            match self.current() {
                TokenKind::Module => self.parse_module_decl(),
                TokenKind::Let | TokenKind::Const => self.parse_variable_decl(),
                _ => self.parse_expression(),
            }
        }

        self.close(m, CSTTreeKind::File);
    }

    fn parse_module_decl(&mut self) {
        assert_eq!(self.current(), TokenKind::Module);
        let m = self.open();
        self.expect(TokenKind::Module);
        self.parse_name(true);
        self.close(m, CSTTreeKind::ModuleDeclaration);
    }

    fn parse_variable_decl(&mut self) {
        let m = self.open();
        match self.current() {
            TokenKind::Let | TokenKind::Const => {}
            _ => panic!("unexpected token"),
        }
        self.advance();

        self.parse_name(false);

        if self.eat(TokenKind::Assign) {
            self.parse_expression();
        }

        self.close(m, CSTTreeKind::Declaration);
    }

    fn parse_expression(&mut self) {
        self.parse_expression_rec(TokenKind::Eof);
    }

    fn parse_expression_rec(&mut self, left: TokenKind) {
        let mut lhs = self.parse_expression_delimited();

        while self.at(TokenKind::LeftParen) {
            let m = self.open_before(&lhs);
            self.parse_argument_list();
            lhs = self.close(m, CSTTreeKind::CallOp)
        }

        loop {
            let right = self.current();
            if Self::right_binds_tighter(left, right) {
                let m = self.open_before(&lhs);
                self.advance();
                self.parse_expression_rec(right);
                lhs = self.close(m, CSTTreeKind::BinaryOp);
            } else {
                break;
            }
        }
    }

    fn right_binds_tighter(left: TokenKind, right: TokenKind) -> bool {
        /// the further down in the list an operator is,
        /// the tighter it binds
        ///
        /// operators on the same level have the same precedence
        fn tightness(kind: TokenKind) -> Option<usize> {
            use TokenKind::*;

            let list = [
                [Or].as_slice(),
                [And].as_slice(),
                [Eq, Neq].as_slice(),
                [Instanceof].as_slice(),
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

    fn parse_argument_list(&mut self) {
        assert!(self.at(TokenKind::LeftParen));
        let m = self.open();

        self.expect(TokenKind::LeftParen);
        while !self.at(TokenKind::RightParen) && !self.eof() {
            self.parse_argument();
        }
        self.expect(TokenKind::RightParen);

        self.close(m, CSTTreeKind::CallArgumentList);
    }

    fn parse_argument(&mut self) {
        let m = self.open();
        self.parse_expression();

        if !self.at(TokenKind::RightParen) {
            self.expect(TokenKind::Comma);
        }

        self.close(m, CSTTreeKind::CallArgument);
    }

    fn parse_expression_delimited(&mut self) -> MarkClosed {
        match self.current() {
            // literals
            TokenKind::Identifier
            | TokenKind::Integer
            | TokenKind::Float
            | TokenKind::String
            | TokenKind::True
            | TokenKind::False
            | TokenKind::Null
            | TokenKind::Super => {
                let m = self.open();
                self.advance();
                self.close(m, CSTTreeKind::Literal)
            }

            // ( expr )
            TokenKind::LeftParen => {
                let m = self.open();
                self.advance();
                self.parse_expression();
                self.expect(TokenKind::RightParen);
                self.close(m, CSTTreeKind::Paren)
            }

            // format strings
            TokenKind::FormatStringPart => todo!("format strings not implemented"),

            _ => {
                let m = self.open();
                if !self.eof() {
                    self.advance_with_error("unexpected token");
                }
                self.close(m, CSTTreeKind::Error)
            }
        }
    }

    fn parse_name(&mut self, allow_qualified: bool) {
        let m = self.open();
        self.expect(TokenKind::Identifier);
        if allow_qualified {
            while self.eat(TokenKind::Dot) {
                self.expect(TokenKind::Identifier);
            }
        }

        self.close(m, CSTTreeKind::Name);
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
        self.fuel.set(256);
        self.events.push(Event::Advance);
        self.pos += 1;
    }

    fn eof(&self) -> bool {
        self.current() == TokenKind::Eof
    }

    fn nth_token(&self, lookahead: usize) -> &Token {
        let result = self.tokens.get(self.pos + lookahead);
        match result {
            Some(token) => token,
            None => self.tokens.last().unwrap(),
        }
    }

    fn nth(&self, lookahead: usize) -> TokenKind {
        if self.fuel.get() == 0 {
            panic!("parser ran out of fuel");
        }
        self.fuel.set(self.fuel.get() - 1);
        self.nth_token(lookahead).kind
    }

    fn current(&self) -> TokenKind {
        self.nth(0)
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.current() == kind
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
        self.unexpected_token(kind);
    }

    fn advance_with_error(&mut self, error: &str) {
        let m = self.open();
        let location = self.nth_token(0).location.clone();
        self.diagnostic_context
            .error(error, &location, vec![], vec![]);
        self.advance();
        self.close(m, CSTTreeKind::Error);
    }

    fn unexpected_token(&mut self, expected: TokenKind) {
        let (kind, location) = {
            let current = self.nth_token(0);
            (current.kind, current.location.clone())
        };

        self.diagnostic_context.error(
            format!("expected token {:?}, got {:?}", expected, kind).as_str(),
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
    use indoc::indoc;
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

    #[test]
    fn test_parse_module_decl() {
        let source = r#"
            module foo
            module foo.bar
            module foo.bar.baz
        "#;

        assert_tree(
            source,
            indoc! {"
                File
                  ModuleDeclaration
                    Module
                    Name
                      Identifier(foo)
                  ModuleDeclaration
                    Module
                    Name
                      Identifier(foo)
                      Dot
                      Identifier(bar)
                  ModuleDeclaration
                    Module
                    Name
                      Identifier(foo)
                      Dot
                      Identifier(bar)
                      Dot
                      Identifier(baz)
            "},
            &[],
        );
    }

    #[test]
    fn test_parse_declarations() {
        let source = r#"
            let a
            let b = 100
            const c = 200
        "#;

        assert_tree(
            source,
            indoc! {"
                File
                  Declaration
                    Let
                    Name
                      Identifier(a)
                  Declaration
                    Let
                    Name
                      Identifier(b)
                    Assign
                    Literal
                      Integer(100)
                  Declaration
                    Const
                    Name
                      Identifier(c)
                    Assign
                    Literal
                      Integer(200)
            "},
            &[],
        );
    }

    #[test]
    fn test_parse_binop() {
        let source = r#"
            let a = 10 + 20 * 30 > 100 == false && true || false
        "#;

        assert_tree(
            source,
            indoc! {"
                File
                  Declaration
                    Let
                    Name
                      Identifier(a)
                    Assign
                    BinaryOp
                      BinaryOp
                        BinaryOp
                          BinaryOp
                            BinaryOp
                              Literal
                                Integer(10)
                              Add
                              BinaryOp
                                Literal
                                  Integer(20)
                                Mul
                                Literal
                                  Integer(30)
                            Gt
                            Literal
                              Integer(100)
                          Eq
                          Literal
                            False
                        And
                        Literal
                          True
                      Or
                      Literal
                        False
            "},
            &[],
        );
    }

    #[test]
    fn test_parse_call_expr() {
        let source = r#"
            let a = foo(10, 20, bar(30) * 2) * 2
        "#;

        assert_tree(
            source,
            indoc! {"
                File
                  Declaration
                    Let
                    Name
                      Identifier(a)
                    Assign
                    BinaryOp
                      CallOp
                        Literal
                          Identifier(foo)
                        CallArgumentList
                          LeftParen
                          CallArgument
                            Literal
                              Integer(10)
                            Comma
                          CallArgument
                            Literal
                              Integer(20)
                            Comma
                          CallArgument
                            BinaryOp
                              CallOp
                                Literal
                                  Identifier(bar)
                                CallArgumentList
                                  LeftParen
                                  CallArgument
                                    Literal
                                      Integer(30)
                                  RightParen
                              Mul
                              Literal
                                Integer(2)
                          RightParen
                      Mul
                      Literal
                        Integer(2)
            "},
            &[],
        );
    }

    #[test]
    fn test_parse_expressions() {
        let source = r#"
            let a = 100
            let b = foo
            let c = 10 + 20 * 30 + 40
            let d = c + 1 == (200 / 20)
        "#;

        assert_tree(
            source,
            indoc! {"
                File
                  Declaration
                    Let
                    Name
                      Identifier(a)
                    Assign
                    Literal
                      Integer(100)
                  Declaration
                    Let
                    Name
                      Identifier(b)
                    Assign
                    Literal
                      Identifier(foo)
                  Declaration
                    Let
                    Name
                      Identifier(c)
                    Assign
                    BinaryOp
                      BinaryOp
                        Literal
                          Integer(10)
                        Add
                        BinaryOp
                          Literal
                            Integer(20)
                          Mul
                          Literal
                            Integer(30)
                      Add
                      Literal
                        Integer(40)
                  Declaration
                    Let
                    Name
                      Identifier(d)
                    Assign
                    BinaryOp
                      BinaryOp
                        Literal
                          Identifier(c)
                        Add
                        Literal
                          Integer(1)
                      Eq
                      Paren
                        LeftParen
                        BinaryOp
                          Literal
                            Integer(200)
                          Div
                          Literal
                            Integer(20)
                        RightParen
            "},
            &[],
        );
    }
}
