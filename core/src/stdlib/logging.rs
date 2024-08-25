use crate::check_args_num;
use crate::{
    parser::{Expr, Primitive},
    symbols::{self, Module, SymbolTable},
};

pub struct Logging;

#[piratelang_macros::load_module]
impl Logging {
    fn print(s: Vec<Expr>) -> Box<Expr> {
        check_args_num!(1, s.len());

        if let Expr::Primitive(prim) = s.first().unwrap() {
            println!("{prim}");
        };

        Box::new(Expr::Primitive(Primitive::None))
    }
}
