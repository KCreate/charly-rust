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
    Stmt,
    DeclStmt,
    FlowStmt,
    ExprStmt,
    Block,

    // module decl
    ModuleDecl,
    ModulePathSpec,

    // use decl
    UseDecl,
    UsePathSpec,
    UsePathSpecWildcard,

    // struct decl
    StructDecl,
    StructMemberList,
    StructMemberPart,
    StructBody,
    StructBodyStmt,
    StructInitBlock,
    StructDeclModifierBlock,
    StructCompanionObjectDecl,

    // interface decl
    InterfaceDecl,
    InterfaceBody,

    // typealias decl
    TypealiasDecl,

    // fn decl
    FnDecl,
    FnName,
    FnParamList,
    FnParam,
    FnReturnDecl,
    FnBody,
    FnExprBody,
    FnBlockBody,

    // variable decl
    VariableDecl,

    // control flow stmt
    IfStmt,
    WhileStmt,
    ForStmt,
    LoopStmt,

    // when stmt
    WhenStmt,
    WhenBody,
    WhenCondition,
    WhenExprCondition,
    WhenIsCondition,
    WhenIsUnpackPart,
    WhenElseCondition,
    WhenSubCondition,

    // throw stmt
    ThrowStmt,

    // try catch finally stmt
    TryStmt,
    CatchStmt,
    FinallyStmt,

    // select stmt
    SelectStmt,
    SelectExprCase,
    SelectElseCase,

    // defer stmt
    DeferStmt,

    // expressions
    Literal,
    AwaitExpr,
    BinOpExpr,
    UnaryOpExpr,
    CallExpr,
    IndexExpr,
    MemberExpr,
    RangeExpr,

    // literals
    FStringLiteral,
    TupleLiteral,
    ListLiteral,

    // lambda literal
    LambdaLiteral,
    LambdaParameterDecl,

    // map literal
    MapLiteral,
    MapPart,
    MapPartKey,

    // type annotations
    TypeAnnotation,
    TypeExpr,
    TypeExprPart,
    TypeExprGenericPart,

    // misc
    DeclModifier,
    BlockOrFlowStmt,
    BlockOrExprStmt,
    BlockOrExpr,
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
