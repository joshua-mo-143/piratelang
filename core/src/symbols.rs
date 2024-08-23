use std::collections::HashMap;

use crate::parser::Expr;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    inner: HashMap<String, Box<Expr>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            inner: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: String, var: Box<Expr>) {
        self.inner.insert(name, var);
    }

    pub fn get(&mut self, name: String) -> Box<Expr> {
        let Some(expr) = self.inner.remove(&name) else {
            todo!("Handle if a variable doesn't exist")
        };

        expr
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }
}
