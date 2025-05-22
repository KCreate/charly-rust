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

use std::fmt::{Display, Formatter};

use crate::charly::compiler::token::Token;
use crate::charly::utils::ascii_tree::IndentStyle;
use crate::charly::utils::cst_printer::CstPrinter;
use crate::charly::utils::diagnostics::DiagnosticLocation;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CSTTreeKind {
    Error,
    File,

    // statements
    ModuleDeclaration,
    Declaration,
    UnpackDeclaration,
    Block,
    If,
    While,
    Loop,
    For,
    Match,
    MatchArm,
    TryCatch,
    TryFinally,

    // control statements
    Return,
    Break,
    Continue,
    Throw,
    Assert,
    Export,
    Import,
    Yield,
    Spawn,
    Await,
    Typeof,

    // expressions
    Assignment,
    MemberOp,
    IndexOp,
    BinaryOp,
    UnaryOp,
    CallOp,
    CallArgumentList,
    CallArgument,
    UnpackTargetElement,
    UnpackTarget,
    OptionalUnpack,
    Spread,

    // literals
    Literal,
    Name,
    Paren,
    FormatString,
    Tuple,
    List,
    Dict,
    DictEntry,
    Fn,
    FnParamList,
    FnParam,
    Class,
    ClassBody,
    ClassStaticDecl,
    ClassStaticBlock,
    ClassExtends,
    ClassProperty,
}

pub struct CSTTree {
    pub kind: CSTTreeKind,
    pub children: Vec<CSTNode>,
}

impl CSTTree {
    pub fn location(&self) -> DiagnosticLocation {
        assert!(self.children.len() > 0);
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
        printer.set_indent_style(IndentStyle::Plain);
        let text = printer.format(self);
        write!(f, "{}", text)
    }
}
