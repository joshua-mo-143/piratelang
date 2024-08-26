use piratelang_core::{
    interpreter::Interpreter,
    parser::{parse_statement, Expr, Primitive, Span},
    symbols::SymbolTable,
};
use std::io::{self, stdout, Read};
use std::rc::Rc;
use std::{
    cell::{RefCell, RefMut},
    io::Write,
};

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

        parser.interpret(buf.trim());
    }
}
