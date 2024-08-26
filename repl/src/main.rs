use piratelang_core::interpreter::Interpreter;
use std::io::{self, stdout};

use std::io::Write;

fn main() {
    println!("Piratelang v0.0.0 REPL (Press Ctrl+c to exit)");
    println!("---");
    let mut parser = Interpreter::new();
    parser.load_modules();

    loop {
        let mut buf = String::new();
        print!("> ");
        stdout().flush().unwrap();

        io::stdin().read_line(&mut buf).unwrap();

        if let Err(e) = parser.interpret(buf.trim()) {
            println!("Error while parsing: {e:?}")
        };
    }
}
