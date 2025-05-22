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

pub struct AsciiTree {
    text: String,
    children: Vec<AsciiTree>,
    indent_style: IndentStyle,
}

#[derive(Debug, Clone, Copy)]
pub enum IndentStyle {
    Unicode,
    Ascii,
    Plain,
}

impl IndentStyle {
    fn branch_last(&self) -> &str {
        match self {
            IndentStyle::Unicode => "└─",
            IndentStyle::Ascii => "`-",
            IndentStyle::Plain => "  ",
        }
    }

    fn branch_middle(&self) -> &str {
        match self {
            IndentStyle::Unicode => "├─",
            IndentStyle::Ascii => "|-",
            IndentStyle::Plain => "  ",
        }
    }

    fn vertical(&self) -> &str {
        match self {
            IndentStyle::Unicode => "│ ",
            IndentStyle::Ascii => "| ",
            IndentStyle::Plain => "  ",
        }
    }

    fn empty(&self) -> &str {
        "  "
    }
}

impl Default for IndentStyle {
    fn default() -> Self {
        IndentStyle::Unicode
    }
}

impl AsciiTree {
    pub fn new(text: &str) -> Self {
        Self {
            text: text.to_string(),
            children: Vec::new(),
            indent_style: IndentStyle::default(),
        }
    }

    pub fn new_with_style(text: &str, indent_style: IndentStyle) -> Self {
        Self {
            text: text.to_string(),
            children: Vec::new(),
            indent_style,
        }
    }

    pub fn set_indent_style(&mut self, style: IndentStyle) {
        self.indent_style = style;
    }

    pub fn attach(&mut self, child: AsciiTree) -> &mut Self {
        self.children.push(child);
        self.children.last_mut().unwrap()
    }

    pub fn node(&mut self, text: &str) -> &mut Self {
        self.attach(AsciiTree::new_with_style(text, self.indent_style))
    }

    pub fn format(&self) -> String {
        let mut buffer = String::new();

        buffer.push_str(&self.text);
        buffer.push('\n');

        let child_count = self.children.len();
        for (child_index, child) in self.children.iter().enumerate() {
            let child_text = child.format();
            let lines: Vec<&str> = child_text.lines().collect();
            let line_count = lines.len();
            for (line_index, line_text) in lines.iter().enumerate() {
                let is_last_child = child_index == child_count - 1;
                let is_first_line = line_index == 0;

                let indent = match (is_last_child, is_first_line) {
                    (true, true) => self.indent_style.branch_last(),
                    (true, false) => self.indent_style.empty(),
                    (false, true) => self.indent_style.branch_middle(),
                    (false, false) => self.indent_style.vertical(),
                };

                buffer.push_str(indent);
                buffer.push_str(line_text);

                let is_last_line = line_index == line_count - 1;
                if !is_last_line {
                    buffer.push('\n');
                }
            }

            buffer.push('\n');
        }

        buffer
    }
}

impl Display for AsciiTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format())
    }
}

#[cfg(test)]
mod tests {
    use crate::charly::utils::ascii_tree::{AsciiTree, IndentStyle};
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_ascii_tree_plain() {
        let mut root = AsciiTree::new("root");
        root.set_indent_style(IndentStyle::Plain);

        {
            let foo = root.node("foo");
            foo.node("1");
            foo.node("2");
            foo.node("3");
        }

        root.node("bar");

        {
            let baz = root.node("baz");
            baz.node("1");
            baz.node("2");
            baz.node("3");
        }

        let format = root.format();

        assert_eq!(
            indoc! {"
                root
                  foo
                    1
                    2
                    3
                  bar
                  baz
                    1
                    2
                    3
            "},
            format
        )
    }

    #[test]
    fn test_ascii_tree_ascii() {
        let mut root = AsciiTree::new("root");
        root.set_indent_style(IndentStyle::Ascii);

        {
            let foo = root.node("foo");
            foo.node("1");
            foo.node("2");
            foo.node("3");
        }

        root.node("bar");

        {
            let baz = root.node("baz");
            baz.node("1");
            baz.node("2");
            baz.node("3");
        }

        let format = root.format();

        assert_eq!(
            indoc! {"
                root
                |-foo
                | |-1
                | |-2
                | `-3
                |-bar
                `-baz
                  |-1
                  |-2
                  `-3
            "},
            format
        )
    }

    #[test]
    fn test_ascii_tree_unicode() {
        let mut root = AsciiTree::new("root");
        root.set_indent_style(IndentStyle::Ascii);

        {
            let foo = root.node("foo");
            foo.node("1");
            foo.node("2");
            foo.node("3");
        }

        root.node("bar");

        {
            let baz = root.node("baz");
            baz.node("1");
            baz.node("2");
            baz.node("3");
        }

        let format = root.format();

        assert_eq!(
            indoc! {"
                root
                |-foo
                | |-1
                | |-2
                | `-3
                |-bar
                `-baz
                  |-1
                  |-2
                  `-3
            "},
            format
        )
    }
}
