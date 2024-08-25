use crate::check_args_num;
use crate::parser::{Expr, Primitive};
use crate::symbols::{Module, SymbolTable};

pub struct StringMethods;

#[piratelang_macros::load_module(name = "string")]
impl StringMethods {
    fn contains(extra_args: Vec<Expr>) -> Box<Expr> {
        check_args_num!(1, extra_args.len());

        let mut extra_args = extra_args.iter();
        let Expr::Primitive(Primitive::String(str)) = extra_args.next().unwrap() else {
            panic!("Chained variable is not a string.");
        };

        let Expr::Primitive(Primitive::String(pred)) = extra_args.next().unwrap() else {
            panic!("Provided function argument is required to be a String");
        };

        if str.contains(pred) {
            Box::new(Expr::Primitive(Primitive::Bool(true)))
        } else {
            Box::new(Expr::Primitive(Primitive::Bool(false)))
        }
    }

    fn pushs(extra_args: Vec<Expr>) -> Box<Expr> {
        check_args_num!(2, extra_args.len());
        let mut extra_args = extra_args.into_iter();
        let Expr::Primitive(Primitive::String(str)) = extra_args.next().unwrap() else {
            panic!("Chained variable is not a string.");
        };

        let Expr::Primitive(Primitive::String(to_add)) = extra_args.next().unwrap() else {
            panic!("Provided function argument is required to be a String");
        };

        let mut str = str.to_owned();
        str.push_str(&to_add);

        Box::new(Expr::Primitive(Primitive::String(str.to_owned())))
    }
}
