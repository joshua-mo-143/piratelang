use piratelang_core::{
    interpreter::Interpreter,
    parser::{parse_statement, Span},
    symbols::SymbolTable,
};
use std::io::{self, Read};

fn process_input(buf: String, symbols: &mut SymbolTable) {
    let span = Span::new(&buf.trim());

    match parse_statement(span) {
        Ok((s, val)) => println!("{val:?}"),
        Err(e) => println!("Error while parsing: {e}"),
    };
}

fn main() {
    println!("Piratelang v0.0.0 REPL (Press Ctrl+c to exit)");
    println!("---");
    let mut symbols = SymbolTable::new();
    loop {
        let mut buf = String::new();
        print!("> ");

        io::stdin().read_line(&mut buf).unwrap();

        process_input(buf, &mut symbols)
    }
}
