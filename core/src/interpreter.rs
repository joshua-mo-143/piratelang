use crate::parser::{parse_statement, Expr, Span};
use crate::symbols::SymbolTable;

pub struct Interpreter {
    symbol_table: SymbolTable,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn interpret(&mut self, s: &str) {
        let mut input = Span::new(s);

        let output: Option<Expr> = None;
        while !input.is_empty() || input.fragment() != &"\n" {
            let (remaining_input, expr) = parse_statement(input).unwrap();
            let val = expr.evaluate(&mut self.symbol_table).unwrap();

            if val == Expr::Eof {
                break;
            }
            input = remaining_input;
        }
    }

    pub fn interpret_print(&mut self, s: &str) {
        let mut input = Span::new(s);

        while !input.is_empty() || input.fragment() != &"\n" {
            let (remaining_input, expr) = parse_statement(input).unwrap();
            let val = expr.evaluate(&mut self.symbol_table).unwrap();
            print!("{val:?}");

            if val == Expr::Eof {
                break;
            }
            input = remaining_input;
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs::read_to_string;

    use super::*;

    #[test]
    fn test_assignment_works() {
        let mut parser = Interpreter::new();
        let input = read_to_string("../_examples/hello.pirate").unwrap();
        let input = input.trim_end();

        parser.interpret(input);

        let str = parser.symbol_table.get("hello".to_string());

        assert_eq!(*str, Expr::Primitive("World!".to_string().into()));
    }
}
