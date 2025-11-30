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

use crate::charly::compiler::cst_parser::CSTParser;
use crate::charly::compiler::token::TokenKind;
use crate::charly::compiler::tokenizer::Tokenizer;
use crate::charly::utils::ascii_tree::AsciiTree;
use crate::charly::utils::diagnostics::DiagnosticController;
use crate::{Args, Commands, DebugArgs};
use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;
use std::path::PathBuf;
use std::process::ExitCode;

pub fn run(cli: Args) -> ExitCode {
    use std::process::ExitCode;

    match cli.command {
        Some(Commands::Debug) => {
            command_debug();
            ExitCode::SUCCESS
        }
        None => match cli.filename {
            Some(filename) => command_run(filename, cli.debug_args),
            None => {
                command_repl(cli.debug_args);
                ExitCode::SUCCESS
            }
        },
    }
}

fn command_run(filename: String, debug_args: DebugArgs) -> ExitCode {
    let Some(path) = PathBuf::try_from(&filename).ok() else {
        eprintln!("Error: Invalid filename '{}'", filename);
        return ExitCode::FAILURE;
    };

    let Some(content) = std::fs::read_to_string(&path).ok() else {
        eprintln!("Error: Could not read file '{}'", filename);
        return ExitCode::FAILURE;
    };

    compile(&debug_args, &path, &content)
}

fn command_repl(debug_args: DebugArgs) {
    let mut debug_args = debug_args;

    let path = PathBuf::try_from("repl").unwrap();
    let mut rl = DefaultEditor::new().unwrap();
    loop {
        match rl.readline("> ") {
            Ok(line) => match line.trim() {
                ".exit" => break,

                ".help" => {
                    println!("Help:");
                    println!("  .exit           Exit the REPL");
                    println!("  .clear          Clear REPL history");
                    println!("  .help           Show this message");
                    println!("  .dump-tokens    Toggle token dumping");
                    println!("  .dump-cst       Toggle CST dumping");
                }

                ".clear" => {
                    rl.clear_history().expect("Failed to clear history");
                }

                ".dump-tokens" => {
                    debug_args.dump_tokens = !debug_args.dump_tokens;
                    println!(
                        "Dump tokens: {}",
                        if debug_args.dump_tokens {
                            "enabled"
                        } else {
                            "disabled"
                        }
                    );
                }

                ".dump-cst" => {
                    debug_args.dump_cst = !debug_args.dump_cst;
                    println!(
                        "Dump CST: {}",
                        if debug_args.dump_cst {
                            "enabled"
                        } else {
                            "disabled"
                        }
                    );
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

fn command_debug() {
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

fn compile(debug_args: &DebugArgs, path: &PathBuf, content: &str) -> ExitCode {
    let mut controller = DiagnosticController::new();
    let file_id = controller.register_file(&path, content);
    let context = controller.get_or_create_context(file_id);

    let tokens = Tokenizer::tokenize(content, context);

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

    let mut parser = CSTParser::new(&tokens, context);
    let tree = parser.parse();

    if debug_args.dump_cst {
        println!("{}", tree);
    }

    controller.print_diagnostics();

    match controller.has_errors() {
        true => ExitCode::FAILURE,
        false => ExitCode::SUCCESS,
    }
}
