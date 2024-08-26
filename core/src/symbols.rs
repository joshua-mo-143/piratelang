use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::{parse_statement, Expr, FnParameter, Primitive, Struct, StructParam};

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub vars: HashMap<String, Box<Expr>>,
    fns: HashMap<String, Box<Expr>>,
    structs: HashMap<String, Box<Expr>>,
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
            structs: HashMap::new(),
        }
    }

    pub fn get_vars_table<'a>(&'a self) -> &'a HashMap<String, Box<Expr>> {
        &self.vars
    }

    pub fn set_var(&mut self, name: String, var: Box<Expr>) {
        self.vars.insert(name, var);
    }

    pub fn get_var<'a>(&'a self, name: String) -> &'a Expr {
        let Some(expr) = self.vars.get(&name) else {
            panic!("Variable not found: {name}")
        };

        expr
    }

    pub fn register_fn(&mut self, name: String, fun: Box<Expr>) {
        if check_reserved_keyword(&name) {
            panic!("Can't use reserved keyword: {name}");
        }

        if self.fns.contains_key(&name) {
            panic!("Can't register a function that already exists!");
        }

        self.fns.insert(name, fun);
    }

    pub fn register_struct(&mut self, name: String, fields: Vec<FnParameter>) {
        if self.structs.contains_key(&name) {
            panic!("ERROR: Struct has been re-assigned multiple times: {name}");
        }

        let expr = Expr::StructDecl {
            name: name.to_owned(),
            fields,
        };

        self.structs.insert(name, Box::new(expr));
    }

    pub fn get_struct_decl<'a>(&'a self, name: String) -> &'a Expr {
        let Some(expr) = self.structs.get(&name) else {
            panic!("Variable not found: {name}")
        };

        expr
    }

    pub fn init_struct(&self, name: String, args: Vec<Expr>) -> Expr {
        let struct_as_expr = self.get_struct_decl(name.clone());

        let Expr::StructDecl { fields, .. } = struct_as_expr else {
            panic!("{name} incorrectly declared as struct declaration");
        };

        if fields.len() != args.len() {
            panic!(
                "Expected {} args for struct {name}, received {} args",
                fields.len(),
                args.len(),
            )
        }

        let field_args: Vec<StructParam> = args
            .into_iter()
            .zip(fields)
            .map(|(arg, field)| StructParam {
                name: field.name.clone(),
                value: arg,
            })
            .collect();

        let struct_inner = Struct {
            name: name.to_owned(),
            field_args,
        };

        Expr::Primitive(Primitive::Struct(struct_inner))
    }

    pub fn call_fn(&self, name: String, fn_args: Vec<Expr>) -> Box<Expr> {
        let Some(function_as_expr) = self.fns.get(&name) else {
            panic!("Could not find function {name}(). Is it imported correctly?");
        };

        match *function_as_expr.to_owned() {
            Expr::RustFn(fun) => fun(fn_args),
            Expr::FnDecl {
                name,
                body,
                fn_params,
            } => {
                if fn_params.len() != fn_args.len() {
                    panic!(
                        "Expected {} args, found {} args",
                        fn_params.len(),
                        fn_args.len()
                    )
                }

                let local_symbol_table = Rc::new(RefCell::new(SymbolTable::new()));

                fn_args.iter().zip(fn_params).for_each(|(param, arg)| {
                    local_symbol_table
                        .borrow_mut()
                        .set_var(arg.name.to_owned(), Box::new(param.to_owned()));
                });

                local_symbol_table
                    .borrow_mut()
                    .inherit_functions_from(&self.fns);

                let Expr::FnBody(fn_body) = *body else {
                    panic!("Function body is not a body!");
                };

                for expr in fn_body {
                    let _ = expr.evaluate(local_symbol_table.clone());
                }

                return Box::new(Expr::Primitive(Primitive::None));
            }
            _ => todo!(),
        }
    }

    pub fn load_module<M: Module>(&mut self, module: M) {
        module.load(self);
    }

    pub fn inherit_functions_from(&mut self, from: &HashMap<String, Box<Expr>>) {
        self.fns = from.to_owned()
    }

    pub fn len(&self) -> usize {
        self.vars.len()
    }
}

pub trait Module {
    fn load(self, symbols: &mut SymbolTable);
}

fn check_reserved_keyword(keyword: &str) -> bool {
    let reserved_keywords = ["yarr"];

    if reserved_keywords.contains(&keyword) {
        true
    } else {
        false
    }
}
