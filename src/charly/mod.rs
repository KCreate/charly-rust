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

mod compiler;
mod test_utils;
mod utils;

use crate::charly::compiler::token::{Token, TokenKind};
use crate::charly::compiler::tokenizer::Tokenizer;
use crate::charly::utils::ascii_tree::AsciiTree;
use crate::charly::utils::diagnostics::DiagnosticController;
use crate::{Args, Commands, DebugArgs};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::path::PathBuf;
use std::process::ExitCode;

pub fn run(cli: Args) -> ExitCode {
    use std::path::PathBuf;
    use std::process::ExitCode;

    match cli.command {
        Commands::Run {
            filename,
            debug_args,
        } => {
            let path = PathBuf::try_from(&filename).unwrap();
            let content = std::fs::read_to_string(&path).unwrap();
            compile(&debug_args, &path, &content);
        }

        Commands::Repl { debug_args } => {
            let path = PathBuf::try_from("repl").unwrap();
            let mut rl = DefaultEditor::new().unwrap();
            loop {
                match rl.readline("> ") {
                    Ok(line) => match line.trim() {
                        ".exit" => break,
                        ".clear" => {
                            rl.clear_history().expect("Failed to clear history");
                            continue;
                        }
                        ".help" => {
                            println!("Help:");
                            println!("  .exit       Exit the REPL");
                            println!("  .clear      Clear REPL history");
                            println!("  .help       Show this message");
                        }
                        _ => {
                            compile(&debug_args, &path, &line);
                            rl.add_history_entry(&line).unwrap();
                        }
                    },
                    Err(ReadlineError::Interrupted) => {
                        break;
                    }
                    Err(ReadlineError::Eof) => continue,
                    Err(err) => {
                        println!("Error: {:?}", err);
                        break;
                    }
                }
            }
        }

        Commands::Debug {} => {
            let mut root = AsciiTree::new("root");

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

            println!("{}", format);
        }
    }

    ExitCode::SUCCESS
}

fn compile(debug_args: &DebugArgs, path: &PathBuf, content: &str) {
    let mut controller = DiagnosticController::new();
    let file_id = controller.register_file(&path, content);
    let context = controller.get_or_create_context(file_id);

    let tokenizer = Tokenizer::new(content, file_id, context);
    let tokens: Vec<Token> = tokenizer.collect();

    if debug_args.dump_source {
        println!("{}", content);
    }

    if debug_args.dump_tokens {
        for token in &tokens {
            match &token.kind {
                TokenKind::Newline => continue,
                TokenKind::Whitespace => continue,
                _ => println!(
                    "{:<32}: {}",
                    format!("{}", token),
                    token.location.span.start
                ),
            }
        }
    }

    // let mut parser = CSTParser::new(&tokens, context);
    // let tree = parser.parse();
    //
    // if debug_args.dump_cst {
    //     println!("{}", tree);
    // }

    controller.print_diagnostics();
}
