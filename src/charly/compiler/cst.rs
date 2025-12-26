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

use crate::charly::compiler::token::{Token, TokenKind};
use crate::charly::utils::ascii_tree::IndentStyle;
use crate::charly::utils::cst_printer::CstPrinter;
use crate::charly::utils::diagnostics::DiagnosticLocation;
use std::fmt::{Display, Formatter};

#[allow(unused)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CSTTreeKind {
    Error,
    Program,
    TopLevelItem,

    // declaration misc.
    NodeWithDeclModifiers,
    DeclModifiers,
    NameDecl,
    UnpackTargetSequence,
    UnpackTargetAccess,
    UnpackTargetItem,

    // variable declarations
    VarDecl,
    VarDeclTarget,
    VarDeclValue,

    // import declarations
    ImportDecl,
    ImportTarget,
    ImportPath,
    ImportAsItem,

    // expressions
    TypeExpr,
    Expr,
    PrefixOpExpr,
    InfixOpExpr,
    PostfixOpExpr,
    MemberExpr,
    NullableMemberExpr,

    // literals
    Atom,
    Identifier,
    String,
    StringInterpolatedExpr,

    // attributes
    NodeWithAttributes,
    AttributeList,
    Attribute,
    AttributeArguments,
    AttributeItem,
    AttributeKey,
    AttributeValue,

    // several kinds of brackets
    ParenGroup,
    BracketGroup,
    BraceGroup,
    AngleBracketGroup,
}

impl CSTTreeKind {
    pub fn matching_bracket_token_kinds(&self) -> Option<(TokenKind, TokenKind)> {
        match self {
            CSTTreeKind::ParenGroup => {
                Some((TokenKind::LeftParen, TokenKind::RightParen))
            }
            CSTTreeKind::BracketGroup => {
                Some((TokenKind::LeftBracket, TokenKind::RightBracket))
            }
            CSTTreeKind::BraceGroup => {
                Some((TokenKind::LeftBrace, TokenKind::RightBrace))
            }
            CSTTreeKind::AngleBracketGroup => Some((TokenKind::Lt, TokenKind::Gt)),
            _ => None,
        }
    }
}

impl Display for CSTTreeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub struct CSTTree {
    pub kind: CSTTreeKind,
    pub children: Vec<CSTNode>,
}

impl CSTTree {
    pub fn location(&self) -> DiagnosticLocation {
        assert!(
            self.children.len() > 0,
            "CSTTree of kind {} has no children",
            self.kind
        );
        let start = self.children.first().unwrap().location();
        let end = self.children.last().unwrap().location();
        start.merge(&end)
    }
}

pub enum CSTNode {
    Token(Token),
    Tree(CSTTree),
}

impl CSTNode {
    pub fn location(&self) -> DiagnosticLocation {
        match self {
            CSTNode::Token(token) => token.location.clone(),
            CSTNode::Tree(tree) => tree.location(),
        }
    }
}

impl Display for CSTTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut printer = CstPrinter::new();
        printer.set_indent_style(IndentStyle::Unicode);
        let text = printer.format(self);
        write!(f, "{}", text)
    }
}
