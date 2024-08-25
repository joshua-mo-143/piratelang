use piratelang_core::{
    interpreter::Interpreter,
    parser::{parse_statement, Expr, Primitive, Span},
    stdlib::logging::Logging,
    symbols::SymbolTable,
};
use std::cell::{RefCell, RefMut};
use std::io::{self, Read};
use std::rc::Rc;

fn main() {
    println!("Piratelang v0.0.0 REPL (Press Ctrl+c to exit)");
    println!("---");
    let mut parser = Interpreter::new();
    parser.load_modules();

    loop {
        let mut buf = String::new();
        print!("> ");

        io::stdin().read_line(&mut buf).unwrap();

        parser.interpret(buf.trim());
    }
}
