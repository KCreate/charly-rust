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
use crate::charly::utils::ascii_tree::{AsciiTree, IndentStyle};
use ariadne::{Color, Fmt};

pub struct CstPrinter {
    indent_style: IndentStyle,
    enable_colors: bool,
}

impl CstPrinter {
    pub fn new() -> Self {
        Self {
            indent_style: IndentStyle::default(),
            enable_colors: true,
        }
    }

    pub fn set_indent_style(&mut self, indent_style: IndentStyle) {
        self.indent_style = indent_style;
    }

    pub fn set_enable_colors(&mut self, enable_colors: bool) {
        self.enable_colors = enable_colors;
    }

    pub fn format(&self, tree: &CSTTree) -> String {
        self.build_tree_from_csttree(tree).format()
    }

    fn build_tree_from_cstnode(&self, node: &CSTNode) -> AsciiTree {
        match node {
            CSTNode::Token(token) => self.build_tree_from_token(token),
            CSTNode::Tree(tree) => self.build_tree_from_csttree(tree),
        }
    }

    fn build_tree_from_csttree(&self, tree: &CSTTree) -> AsciiTree {
        let mut node_name = format!("{:?}", tree.kind);

        if self.enable_colors {
            let name_color = match tree.kind {
                CSTTreeKind::Error => Color::Red,
                _ => Color::Yellow,
            };
            node_name = node_name.fg(name_color).to_string();
        }

        node_name.push_str(format!(" {}", tree.location().span.start).as_str());

        let mut root = AsciiTree::new_with_style(node_name.as_str(), self.indent_style);

        for child in &tree.children {
            root.attach(self.build_tree_from_cstnode(child));
        }

        root
    }

    fn build_tree_from_token(&self, token: &Token) -> AsciiTree {
        let mut token_name = token.to_string();

        if self.enable_colors {
            let token_color = match token.kind {
                TokenKind::Error => Color::Red,
                _ => Color::Cyan,
            };
            token_name = token_name.fg(token_color).to_string();
        }

        AsciiTree::new_with_style(token_name.as_str(), self.indent_style)
    }
}
