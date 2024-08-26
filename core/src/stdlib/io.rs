use std::{
    fs::read_to_string,
    path::{Path, PathBuf},
};

use crate::{
    check_args_num,
    parser::{Expr, Primitive},
    symbols::{Module, SymbolTable},
};

pub struct Io;

#[piratelang_macros::load_module]
impl Io {
    fn read_file_str(s: Vec<Expr>) -> Box<Expr> {
        check_args_num!(1, s.len());
        let Expr::Primitive(Primitive::String(filepath)) = s.first().unwrap() else {
            panic!("Filepath is required to be a string");
        };

        let filepath = PathBuf::from(filepath);

        let file_contents = read_to_string(filepath).unwrap();

        Box::new(Expr::string(file_contents))
    }
}
