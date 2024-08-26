use clap::{Parser, Subcommand};
use piratelang_core::interpreter::Interpreter;
use std::fs::read_to_string;
use std::path::PathBuf;

#[derive(Parser)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    cmd: Commands,
}

#[derive(Subcommand, Clone)]
enum Commands {
    #[command(about = "Run a file.")]
    Run {
        #[arg(value_parser, help = "The file to be run.")]
        path: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.cmd {
        Commands::Run { path } => {
            let mut interpreter = Interpreter::new();

            let file_contents = read_to_string(path).unwrap();
            let file_contents = file_contents.trim_end();

            if let Err(e) = interpreter.interpret(file_contents) {
                println!("RUNTIME ERROR: {e:?}")
            };
        }
    }
}
