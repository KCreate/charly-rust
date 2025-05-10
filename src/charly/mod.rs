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

mod diagnostics;
mod token;
mod tokenizer;
mod window_buffer;

use crate::charly::diagnostics::DiagnosticController;
use crate::charly::token::{Token, TokenKind};
use crate::charly::tokenizer::{TokenDetailLevel, Tokenizer};
use crate::{Args, Commands};
use std::path::PathBuf;
use std::process::ExitCode;

pub fn run(cli: Args) -> ExitCode {
    match cli.command {
        Commands::Run {
            filename,
            runtime_args: _,
            debug_args,
        } => {
            let path = PathBuf::try_from(&filename).unwrap();
            let content = std::fs::read_to_string(&path).unwrap();

            let mut controller = DiagnosticController::new();
            let file_id = controller.register_file(&path, content.as_str());
            let context = controller.get_or_create_context(file_id);

            let mut tokenizer = Tokenizer::new(content.as_str(), file_id, context);
            let tokens: Vec<Token> = tokenizer
                .iter(TokenDetailLevel::NoWhitespaceAndComments)
                .collect();

            if debug_args.dump_tokens {
                println!("Got {} tokens", tokens.len());
                for token in &tokens {
                    match &token.kind {
                        TokenKind::Newline => continue,
                        TokenKind::Whitespace => continue,
                        _ => {
                            let kind_str = format!("{:?}", token.kind);
                            let kind_str = format!("{:<32}", kind_str);
                            let text_fmt = format!("{:<16}", token.raw);
                            println!("{}: {} {}", kind_str, text_fmt, token.location.span);
                        }
                    }
                }
            }

            controller.print_diagnostics();
        }
    }

    ExitCode::SUCCESS
}
