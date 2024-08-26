use std::cell::RefCell;
use std::rc::Rc;

use crate::chained_methods::string::StringMethods;
use crate::errors::CustomError;
use crate::parser::{parse_statement, Expr, Span};
use crate::stdlib::logging::Log;
use crate::symbols::SymbolTable;

pub struct Interpreter {
    symbol_table: Rc<RefCell<SymbolTable>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let symbol_table = Rc::new(RefCell::new(SymbolTable::new()));
        Self { symbol_table }
    }

    pub fn load_modules(&mut self) {
        self.symbol_table.borrow_mut().load_module(StringMethods);
        self.symbol_table.borrow_mut().load_module(Log);
    }

    pub fn interpret(&mut self, s: &str) {
        let mut input = Span::new(s);

        while !input.is_empty() || input.fragment() != &"\n" {
            let (remaining_input, expr) = parse_statement(input).unwrap();
            let val = expr.evaluate(self.symbol_table.clone()).unwrap();

            if val == Expr::Eof {
                break;
            }
            input = remaining_input;
        }
    }

    pub fn interpret_print(&self, s: &str) {
        let mut input = Span::new(s);

        while !input.is_empty() || input.fragment() != &"\n" {
            let (remaining_input, expr) = parse_statement(input).unwrap();
            println!("Statement found: {expr:?}");
            println!("Input remaining: {remaining_input:?}");
            let table = Rc::clone(&self.symbol_table);
            let val = expr.evaluate(table).unwrap();
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
        parser.load_modules();
        let input = read_to_string("../_examples/assignment.pirate").unwrap();
        let input = input.trim_end();

        parser.interpret(input);
        assert_eq!(parser.symbol_table.borrow().len(), 2);
    }

    #[test]
    fn test_using_variables_works() {
        let mut parser = Interpreter::new();
        let input = read_to_string("../_examples/imports.pirate").unwrap();
        let input = input.trim_end();

        parser.interpret(input);

        assert_eq!(parser.symbol_table.borrow().len(), 1);
    }

    #[test]
    fn test_using_functions_works() {
        let mut parser = Interpreter::new();
        let input = read_to_string("../_examples/functions.pirate").unwrap();
        let input = input.trim_end();

        parser.interpret(input);

        assert_eq!(parser.symbol_table.borrow().len(), 1);
    }

    #[test]
    fn test_using_structs_works() {
        let mut parser = Interpreter::new();
        parser.load_modules();
        let input = read_to_string("../_examples/structs.pirate").unwrap();
        let input = input.trim_end();

        parser.interpret(input);

        assert_eq!(parser.symbol_table.borrow().len(), 1);
    }
}
