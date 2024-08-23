use std::collections::HashMap;

use crate::parser::Expr;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    vars: HashMap<String, Box<Expr>>,
    fns: HashMap<String, Box<Expr>>,
}

#[derive(Clone, Debug)]
pub struct Function {
    module_name: String,
    fun: Box<Expr>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            fns: HashMap::new(),
        }
    }

    pub fn set_var(&mut self, name: String, var: Box<Expr>) {
        self.vars.insert(name, var);
    }

    pub fn get_var(&mut self, name: String) -> Box<Expr> {
        let Some(expr) = self.vars.remove(&name) else {
            todo!("Handle if a variable doesn't exist")
        };
        expr
    }

    pub fn register_fn(&mut self, name: String, fun: Box<Expr>) {
        self.fns.insert(name, fun);
    }

    pub fn call_fn(&self, name: String, params: Vec<Expr>) -> Box<Expr> {
        let Some(function_as_expr) = self.fns.get(&name) else {
            panic!("Could not find function {name}");
        };

        match *function_as_expr.to_owned() {
            Expr::RustFn(fun) => fun(params),
            Expr::FnDecl { name, params, body } => todo!(),
            _ => todo!(),
        }
    }

    pub fn load_module<M: Module>(&mut self, module: M) {
        module.load(self);
    }

    pub fn len(&self) -> usize {
        self.vars.len()
    }
}

pub trait Module {
    fn load(self, symbols: &mut SymbolTable);
}
