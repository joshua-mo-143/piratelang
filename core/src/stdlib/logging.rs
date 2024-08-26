use crate::check_args_num;
use crate::{
    parser::{Expr, Primitive},
    symbols::{Module, SymbolTable},
};

pub struct Log;

#[piratelang_macros::load_module]
impl Log {
    fn print(s: Vec<Expr>) -> Box<Expr> {
        check_args_num!(1, s.len());

        println!("{}", s.first().unwrap());

        Box::new(Expr::Primitive(Primitive::None))
    }
}
