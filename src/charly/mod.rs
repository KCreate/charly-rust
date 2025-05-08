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

mod token;
mod tokenizer;
mod window_buffer;

use crate::charly::token::{Token, TokenType};
use crate::charly::tokenizer::{TokenDetailLevel, Tokenizer};
use crate::{Args, Commands};
use std::path::PathBuf;
use std::process::ExitCode;

pub fn run(cli: Args) -> ExitCode {
    match cli.command {
        Commands::Run {
            filename,
            runtime_args: _,
            debug_args: _,
        } => {
            let path = PathBuf::try_from(filename).expect("Failed to convert filename to path");
            let content = std::fs::read_to_string(&path).expect("Failed to read file");

            let mut tokenizer = Tokenizer::new(&content);
            let tokens: Vec<Token> = tokenizer
                .iter(TokenDetailLevel::NoWhitespaceAndComments)
                .collect();

            println!("Got {} tokens", tokens.len());

            for token in tokens {
                match token.token_type {
                    TokenType::Newline => continue,
                    TokenType::Whitespace => continue,
                    _ => {
                        let token_type_str = format!("{:?}", token.token_type);
                        let type_fmt = format!("{:<32}", token_type_str);
                        let text_fmt = format!("{:<16}", token.raw);
                        println!("{}: {} {}", type_fmt, text_fmt, token.span);
                    }
                }
            }
        }
    }

    ExitCode::SUCCESS
}
