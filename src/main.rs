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

extern crate core;

use clap::{Parser, Subcommand};
use std::process::ExitCode;

mod charly;
use charly::run;

#[derive(Parser, Debug)]
pub struct Args {
    #[command(subcommand)]
    command: Commands,
    // TODO: implement license printing
    // #[arg(long, global = true, help = "Show license")]
    // license: bool,
}

#[derive(clap::Args, Debug)]
struct DebugArgs {
    #[arg(help_heading = "Debug", long, help = "Disable AST optimizations")]
    disable_ast_opt: bool,

    #[arg(help_heading = "Debug", long, help = "Disable IR optimizations")]
    disable_ir_opt: bool,

    #[arg(help_heading = "Debug", long, help = "Dump source code to stdout")]
    dump_source: bool,

    #[arg(help_heading = "Debug", long, help = "Dump tokens to stdout")]
    dump_tokens: bool,

    #[arg(help_heading = "Debug", long, help = "Dump CST to stdout")]
    dump_cst: bool,

    #[arg(help_heading = "Debug", long, help = "Dump AST to stdout")]
    dump_ast: bool,

    #[arg(help_heading = "Debug", long, help = "Dump IR to stdout")]
    dump_ir: bool,

    #[arg(
        help_heading = "Debug",
        long,
        help = "Dump runtime constants to stdout"
    )]
    dump_constants: bool,

    #[arg(
        help_heading = "Debug",
        long = "dump-include",
        help = "Include files matching the given glob pattern in any dumps",
        value_name = "glob"
    )]
    dump_include_list: Vec<String>,
}

#[derive(Subcommand, Debug)]
enum Commands {
    #[clap(about = "Run a program")]
    Run {
        #[arg(required = true)]
        filename: String,

        #[clap(flatten)]
        debug_args: DebugArgs,
    },

    #[clap(about = "Run the REPL")]
    Repl {
        #[clap(flatten)]
        debug_args: DebugArgs,
    },

    #[clap(about = "Run the code in the main debug section")]
    Debug {},
}

fn main() -> ExitCode {
    let args = Args::parse();
    run(args)
}
