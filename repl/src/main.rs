use piratelang_core::{
    interpreter::Interpreter,
    parser::{parse_statement, Span},
    stdlib::logging::Logging,
    symbols::SymbolTable,
};
use std::io::{self, Read};

fn process_input(buf: String, symbols: &mut SymbolTable) {
    let span = Span::new(&buf.trim());

    match parse_statement(span) {
        Ok((s, expr)) => {
            let ret_val = expr.evaluate(symbols);

            println!("{ret_val:?}");
        }
        Err(e) => println!("Error while parsing: {e}"),
    };
}

fn main() {
    println!("Piratelang v0.0.0 REPL (Press Ctrl+c to exit)");
    println!("---");
    let mut symbols = SymbolTable::new();
    symbols.load_module(Logging);
    loop {
        let mut buf = String::new();
        print!("> ");

        io::stdin().read_line(&mut buf).unwrap();

        process_input(buf, &mut symbols)
    }
}
