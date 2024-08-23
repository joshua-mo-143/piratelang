use crate::{
    parser::{Expr, Primitive},
    symbols::{self, Module, SymbolTable},
};

pub struct Logging;

impl Logging {
    fn print(s: Vec<Expr>) -> Box<Expr> {
        if s.len() != 1 {
            println!("Expected 1 argument, got {}", s.len());
        }

        if let Expr::Primitive(prim) = s.first().unwrap() {
            println!("{prim}");
        };

        Box::new(Expr::Primitive(Primitive::None))
    }
}

impl Module for Logging {
    fn load(self, symbols: &mut SymbolTable) {
        symbols.register_fn(
            "print".into(),
            Box::new(Expr::RustFn(Box::new(Self::print))),
        )
    }
}
