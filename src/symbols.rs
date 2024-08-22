use std::collections::HashMap;

use crate::parser::Expr;

#[derive(Debug, Eq, PartialEq, Clone)]
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_table_should_work() {
        let mut map = SymbolTable::new();

        let span = Box::new(Expr::Assign {
            name: "MyVariable".to_string(),
            expr: Box::new(Expr::Primitive(1234.into())),
        });
        span.evaluate(&mut map).unwrap();

        assert_eq!(
            (*map.get("MyVariable".to_string())),
            Expr::Primitive(1234.into())
        );
    }
}
