use clap::{Parser, Subcommand};
use std::process::ExitCode;

mod charly;
use charly::run;

#[derive(Parser, Debug)]
pub struct Args {
    #[command(subcommand)]
    command: Commands,

    #[arg(
        long,
        global = true,
        help = "Show license",
    )]
    license: bool,
}

#[derive(clap::Args, Debug)]
struct RuntimeArgs {

    #[arg(
        help_heading = "Runtime",
        long,
        help = "Set maximum number of concurrent threads",
        default_value = "16",
        value_name = "N",
    )]
    limit_concurrency: Option<usize>,

    #[arg(
        help_heading = "Runtime",
        long,
        help = "Set initial number of heap regions",
        default_value = "16",
        value_name = "N",
    )]
    initial_heap_region_count: Option<usize>,

    #[arg(
        help_heading = "Runtime",
        long,
        help = "Skip execution",
    )]
    skip_execution: bool,

    #[arg(
        help_heading = "Runtime",
        long,
        env = "CHARLY_RUNTIME_PATH",
    )]
    runtime_path: Option<String>,
}

#[derive(clap::Args, Debug)]
struct DebugArgs {
    #[arg(
        help_heading = "Debug",
        long,
        help = "Disable AST optimizations",
    )]
    disable_ast_opt: bool,

    #[arg(
        help_heading = "Debug",
        long,
        help = "Disable IR optimizations",
    )]
    disable_ir_opt: bool,

    #[arg(
        help_heading = "Debug",
        long,
        help = "Dump AST to stdout",
    )]
    dump_ast: bool,

    #[arg(
        help_heading = "Debug",
        long,
        help = "Dump CST to stdout",
    )]
    dump_cst: bool,

    #[arg(
        help_heading = "Debug",
        long,
        help = "Dump IR to stdout",
    )]
    dump_ir: bool,

    #[arg(
        help_heading = "Debug",
        long,
        help = "Dump runtime constants to stdout",
    )]
    dump_constants: bool,

    #[arg(
        help_heading = "Debug",
        long = "dump-include",
        help = "Include files matching the given glob pattern in any dumps",
    )]
    dump_include_list: Vec<String>,
}

#[derive(Subcommand, Debug)]
enum Commands {

    #[clap(about = "Run a program")]
    Run {
        #[arg(
            required = true,
        )]
        filename: String,

        #[clap(flatten)]
        runtime_args: RuntimeArgs,

        #[clap(flatten)]
        debug_args: DebugArgs,
    },
}

fn main() -> ExitCode {
    let args = Args::parse();
    run(args)
}
