use std::cell::RefCell;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fs::File;
use std::rc::Rc;
use std::str::FromStr;

use nom::branch::alt;
use nom::bytes::complete::{escaped, tag, take_until, take_while1};
use nom::character::complete::satisfy;
use nom::character::complete::{char, multispace0, multispace1, none_of, one_of};

use nom::combinator::{eof, opt};

use nom::multi::separated_list0;
use nom::sequence::{delimited, pair, preceded, terminated};
use nom::IResult;
use nom_locate::LocatedSpan;

pub type Span<'a> = LocatedSpan<&'a str>;
pub type NomResult<I, O> = IResult<I, O, CustomError<I>>;

use crate::errors::CustomError;
use crate::stdlib::io::Io;
use crate::stdlib::logging::Log;
use crate::symbols::Module;
use crate::symbols::SymbolTable;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct FnParameter {
    kind: Typename,
    pub name: String,
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum MathOps {
    Add,
    Minus,
    Divide,
    Multiply,
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum EqOps {
    Gt,
    Lt,
    Eq,
    Neq,
}

impl EqOps {
    fn compare(self, expr1: Expr, expr2: Expr) -> Expr {
        match self {
            Self::Gt => self.compare_gt(expr1, expr2),
            Self::Lt => self.compare_lt(expr1, expr2),
            Self::Eq => self.compare_eq(expr1, expr2),
            Self::Neq => self.compare_neq(expr1, expr2),
        }
    }

    fn compare_gt(self, expr1: Expr, expr2: Expr) -> Expr {
        let Expr::Primitive(Primitive::Number(num1)) = expr1 else {
            panic!("Not a valid number")
        };
        let Expr::Primitive(Primitive::Number(num2)) = expr2 else {
            panic!("Not a valid number")
        };

        if num1 > num2 {
            Expr::Primitive(Primitive::Bool(true))
        } else {
            Expr::Primitive(Primitive::Bool(false))
        }
    }
    fn compare_lt(self, expr1: Expr, expr2: Expr) -> Expr {
        let Expr::Primitive(Primitive::Number(num1)) = expr1 else {
            panic!("Not a valid number")
        };
        let Expr::Primitive(Primitive::Number(num2)) = expr2 else {
            panic!("Not a valid number")
        };

        if num1 < num2 {
            Expr::Primitive(Primitive::Bool(true))
        } else {
            Expr::Primitive(Primitive::Bool(false))
        }
    }
    fn compare_eq(self, expr1: Expr, expr2: Expr) -> Expr {
        if expr1 == expr2 {
            Expr::Primitive(Primitive::Bool(true))
        } else {
            Expr::Primitive(Primitive::Bool(false))
        }
    }
    fn compare_neq(self, expr1: Expr, expr2: Expr) -> Expr {
        if expr1 != expr2 {
            return Expr::Primitive(Primitive::Bool(true));
        }
        Expr::Primitive(Primitive::Bool(false))
    }
}

fn parse_eq_ops(s: Span) -> NomResult<Span, EqOps> {
    let mut parser = alt((tag(">"), tag("<"), tag("=="), tag("!=")));

    let (s, op) = parser(s)?;

    match *op.fragment() {
        ">" => Ok((s, EqOps::Gt)),
        "<" => Ok((s, EqOps::Lt)),
        "==" => Ok((s, EqOps::Eq)),
        "!=" => Ok((s, EqOps::Neq)),
        _ => panic!("Operator is not a valid equality operator"),
    }
}

impl Display for EqOps {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Gt => write!(f, ">"),
            Self::Lt => write!(f, "<"),
            Self::Eq => write!(f, "=="),
            Self::Neq => write!(f, "!="),
        }
    }
}

fn parse_compare(s: Span) -> NomResult<Span, Expr> {
    let (s, expr1) = parse_expr(s)?;
    let (s, _) = multispace0(s)?;
    let (s, eq_op) = parse_eq_ops(s)?;
    let (s, _) = multispace0(s)?;
    let (s, expr2) = parse_expr(s)?;

    Ok((
        s,
        Expr::IfCond {
            expr1: Box::new(expr1),
            eq_op,
            expr2: Box::new(expr2),
        },
    ))
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Primitive {
    None,
    String(String),
    Number(i64),
    Bool(bool),
    List(Vec<Expr>),
    Struct(Struct),
}

impl Primitive {
    fn to_module_name(&self) -> String {
        match self {
            Self::None => panic!("None cannot be a module!"),
            Self::String(..) => "string-".into(),
            Self::Number(..) => "number-".into(),
            Self::Bool(..) => "bool-".into(),
            Self::List(..) => "list-".into(),
            Self::Struct(Struct { name, .. }) => format!("{name}-"),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructParam {
    pub name: String,
    pub value: Expr,
}

impl Display for StructParam {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.value)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Struct {
    pub name: String,
    pub field_args: Vec<StructParam>,
}

impl Display for Primitive {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::None => write!(f, "None"),
            Self::String(string) => write!(f, "{string}"),
            Self::Number(num) => write!(f, "{num}"),
            Self::Bool(bool) => write!(f, "{bool}"),
            Self::List(list) => {
                let list_as_str = list
                    .to_owned()
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "[{list_as_str}]")
            }
            Self::Struct(inner) => {
                let args_as_str = inner
                    .field_args
                    .to_owned()
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{} {{ {args_as_str} }}", inner.name)
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Typename {
    String,
    Number,
    Float,
    Bool,
    List,
    Struct(String),
}

impl From<String> for Primitive {
    fn from(s: String) -> Self {
        Self::String(s)
    }
}

impl From<i64> for Primitive {
    fn from(i: i64) -> Self {
        Self::Number(i)
    }
}

impl From<bool> for Primitive {
    fn from(bool: bool) -> Self {
        Self::Bool(bool)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expr {
    Eof,
    Empty,
    Import(String),
    MathOps(MathOps),
    Primitive(Primitive),
    Variable(String),
    ModuleName(String),
    Typename(Typename),
    FnParameter(FnParameter),
    RustFn(Box<fn(Vec<Expr>) -> Box<Expr>>),
    FnBody(Vec<Expr>),
    Return(Box<Expr>),
    Extends {
        name: String,
        functions: Vec<Expr>,
    },
    StructDecl {
        name: String,
        fields: Vec<FnParameter>,
    },
    StructInit {
        name: String,
        args: Vec<Expr>,
    },
    ChainedFnCall {
        caller: Box<Expr>,
        fn_calls: Vec<Expr>,
    },
    FnCall {
        name: String,
        params: Vec<Expr>,
    },
    Assign {
        name: String,
        expr: Box<Expr>,
    },
    If(Box<Expr>, Box<Expr>),
    IfCond {
        expr1: Box<Expr>,
        eq_op: EqOps,
        expr2: Box<Expr>,
    },
    FnDecl {
        fn_name: String,
        fn_params: Vec<FnParameter>,
        body: Box<Expr>,
    },
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Eof => write!(f, "eof"),
            Self::Primitive(p) => write!(f, "{p}"),
            Self::StructInit { name, args } => {
                let args_as_str = args
                    .to_owned()
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{} {{ {args_as_str} }}", name)
            }
            _ => {
                panic!("You can't print that!")
            }
        }
    }
}

impl From<Typename> for Expr {
    fn from(t: Typename) -> Self {
        Self::Typename(t)
    }
}

impl From<Primitive> for Expr {
    fn from(p: Primitive) -> Self {
        Self::Primitive(p)
    }
}

impl Expr {
    pub fn string(str: String) -> Self {
        Self::Primitive(Primitive::String(str))
    }

    pub fn module_name(str: String) -> Self {
        Self::ModuleName(str)
    }

    pub fn number(num: i64) -> Self {
        Self::Primitive(Primitive::Number(num))
    }

    pub fn bool(bool: bool) -> Self {
        Self::Primitive(Primitive::Bool(bool))
    }

    pub fn list(list: Vec<Expr>) -> Self {
        Self::Primitive(Primitive::List(list))
    }

    pub fn evaluate<'a>(
        self,
        symbols: Rc<RefCell<SymbolTable>>,
    ) -> Result<Expr, CustomError<Span<'a>>> {
        match self {
            Expr::Extends { name, functions } => {
                functions.iter().for_each(|x| {
                    let Expr::FnDecl { fn_name, .. } = x else {
                        panic!("Only functions are allowed in `extends` syntax!");
                    };

                    symbols
                        .borrow_mut()
                        .register_fn(format!("{name}-{fn_name}"), Box::new(x.to_owned()));
                });

                Ok(Expr::Extends { name, functions })
            }
            Expr::ModuleName(name) => Ok(Expr::module_name(name.to_owned())),
            Expr::Return(expr) => Ok(Expr::Return(expr.to_owned())),
            Expr::FnCall { name, params } => {
                let fn_args: Vec<Expr> = params
                    .into_iter()
                    .map(|x| match x {
                        Expr::Variable(var_name) => symbols
                            .borrow()
                            .get_var(var_name.to_owned())
                            .unwrap()
                            .to_owned(),
                        Expr::IfCond {
                            expr1,
                            expr2,
                            eq_op,
                        } => eq_op.compare(*expr1, *expr2),
                        _ => x.to_owned(),
                    })
                    .collect();

                Ok(*(symbols
                    .borrow()
                    .call_fn(name.to_owned(), fn_args.to_owned())))
            }
            Expr::Eof => Ok(Expr::Eof),
            Expr::StructDecl { name, fields } => {
                let expr = Expr::StructDecl {
                    name: name.clone(),
                    fields: fields.clone(),
                };
                symbols.borrow_mut().register_struct(name.clone(), fields);

                Ok(expr)
            }
            Expr::StructInit { name, args } => Ok(Expr::StructInit { name, args }),
            Expr::IfCond {
                expr1,
                eq_op,
                expr2,
            } => Ok(eq_op.compare(*expr1, *expr2)),
            Expr::Empty => Ok(Expr::Empty),
            Expr::Import(name) => {
                match name.trim() {
                    "log" => {
                        symbols.borrow_mut().load_module(Log);
                    }
                    "io" => {
                        symbols.borrow_mut().load_module(Io);
                    }
                    _ => return Err("Invalid module.".into()),
                }
                Ok(Expr::Empty)
            }
            Expr::RustFn(fun) => Ok(Expr::RustFn(fun.to_owned())),
            Expr::Primitive(primitive) => Ok(primitive.to_owned().into()),
            Expr::Typename(kind) => Ok(kind.to_owned().into()),
            Expr::Variable(name) => Ok(symbols
                .borrow_mut()
                .get_var(name.to_string())
                .unwrap()
                .to_owned()),
            Expr::FnBody(body) => Ok(Expr::FnBody(body.to_owned())),
            Expr::FnParameter(param) => Ok(Expr::FnParameter(param.to_owned())),
            Expr::ChainedFnCall { caller, fn_calls } => {
                let mut caller = *caller;

                for (i, fn_call) in fn_calls.iter().enumerate() {
                    let Expr::FnCall { name, mut params } = fn_call.clone() else {
                        return Err("Chained expression is not a function call.".into());
                    };

                    params.insert(0, caller.clone());

                    let fn_args: Vec<Expr> = params
                        .into_iter()
                        .map(|x| match x {
                            Expr::Variable(var_name) => symbols
                                .borrow()
                                .get_var(var_name.to_owned())
                                .unwrap()
                                .to_owned(),
                            Expr::IfCond {
                                expr1,
                                expr2,
                                eq_op,
                            } => eq_op.compare(*expr1, *expr2),
                            _ => x.to_owned(),
                        })
                        .collect();

                    let method_name = if let Expr::Primitive(ref prim) = caller {
                        format!("{}{name}", prim.to_module_name())
                    } else {
                        let val = caller.clone().evaluate(symbols.clone()).unwrap();
                        match val {
                            Expr::Primitive(prim) => {
                                format!("{}{name}", prim.to_module_name())
                            }
                            _ => {
                                panic!("Caller does not evaluate to an accepted value.")
                            }
                        }
                    };

                    if i == fn_calls.len() - 1 {
                        return Ok(*(symbols
                            .borrow()
                            .call_fn(method_name.to_owned(), fn_args.to_owned())));
                    } else {
                        let val = *(symbols
                            .borrow()
                            .call_fn(method_name.to_owned(), fn_args.to_owned()));

                        caller = val
                    }
                }

                Ok(Expr::Primitive(Primitive::None))
            }
            Expr::Assign { name, expr } => {
                let value = expr.evaluate(symbols.clone())?;

                symbols
                    .borrow_mut()
                    .set_var(name.clone(), Box::new(value.clone()));
                Ok(value)
            }
            Expr::If(pred, true_branch) => {
                if let Expr::IfCond {
                    eq_op,
                    expr1,
                    expr2,
                } = *pred
                {
                    if eq_op.compare(*expr1, *expr2) == Expr::Primitive(Primitive::Bool(true)) {
                        let value = true_branch.evaluate(symbols)?;
                        Ok(value)
                    } else {
                        Ok(Expr::Primitive(Primitive::Bool(false)))
                    }
                } else if Expr::Primitive(Primitive::Bool(true)) == *pred {
                    let value = true_branch.evaluate(symbols)?;
                    Ok(value)
                } else {
                    Ok(Expr::Primitive(Primitive::Bool(false)))
                }
            }
            Expr::MathOps(op) => Ok(Expr::MathOps(op.to_owned())),
            Expr::FnDecl {
                fn_name,
                fn_params,
                body,
            } => {
                let expr = Expr::FnDecl {
                    fn_name: fn_name.to_owned(),
                    fn_params: fn_params.to_owned(),
                    body: body.to_owned(),
                };
                symbols
                    .borrow_mut()
                    .register_fn(fn_name.clone(), Box::new(expr.to_owned()));

                Ok(Expr::Primitive(Primitive::None))
            }
        }
    }
}

impl From<String> for Expr {
    fn from(s: String) -> Self {
        Expr::Primitive(s.into())
    }
}

impl From<i64> for Expr {
    fn from(i: i64) -> Self {
        Expr::Primitive(i.into())
    }
}

impl From<bool> for Expr {
    fn from(bool: bool) -> Self {
        Expr::Primitive(bool.into())
    }
}

fn parse_if(s: Span) -> NomResult<Span, Expr> {
    let (s, _) = terminated(tag("if"), char(' '))(s)?;

    let (s, cond) = parse_compare(s)?;

    let (s, _) = multispace0(s)?;
    let (s, _) = char('{')(s)?;
    let (s, _) = multispace0(s)?;
    let (s, then) = parse_expr(s)?;
    let (s, _) = multispace0(s)?;
    let (s, _) = char('}')(s)?;

    Ok((s, Expr::If(Box::new(cond), Box::new(then))))
}

fn parse_var(s: Span) -> NomResult<Span, String> {
    let (s, var_name) = take_while1(|c: char| c.is_alphanumeric() || c == '_')(s)?;
    Ok((s, var_name.fragment().to_string()))
}

fn parse_chained_fn_call(s: Span) -> NomResult<Span, Expr> {
    let (s, caller) = alt((parse_var_expr, parse_primitive))(s)?;
    let (s, _) = tag(".")(s)?;
    let (s, fn_calls) = separated_list0(char('.'), parse_fn_call)(s)?;

    Ok((
        s,
        Expr::ChainedFnCall {
            caller: Box::new(caller),
            fn_calls,
        },
    ))
}

fn parse_struct_ident(s: Span) -> NomResult<Span, String> {
    let (s, first_letter) = satisfy(|c: char| c.is_ascii_uppercase())(s)?;
    let (s, rest_of_ident) = take_while1(|c: char| c.is_alphanumeric())(s)?;

    Ok((s, format!("{}{}", first_letter, rest_of_ident.fragment())))
}

fn parse_struct_ident_expr(s: Span) -> NomResult<Span, Expr> {
    let (s, expr) = parse_struct_ident(s)?;
    Ok((s, Expr::Variable(expr)))
}

fn parse_struct_init(s: Span) -> NomResult<Span, Expr> {
    let (s, name) = parse_struct_ident(s)?;
    let (s, _) = multispace0(s)?;

    let mut parser = delimited(
        char('{'),
        delimited(
            multispace0,
            separated_list0(terminated(char(','), multispace0), parse_expr),
            multispace0,
        ),
        char('}'),
    );

    let (s, args) = parser(s)?;

    Ok((s, Expr::StructInit { name, args }))
}

fn parse_struct_decl(s: Span) -> NomResult<Span, Expr> {
    let (s, _) = take_until("struct")(s)?;
    let (s, _) = tag("struct")(s)?;

    let (s, _) = multispace1(s)?;

    let (s, name) = parse_struct_ident(s)?;

    let (s, _) = multispace0(s)?;

    let mut parser = delimited(
        char('{'),
        delimited(
            multispace0,
            separated_list0(terminated(char(','), multispace0), parse_struct_field),
            multispace0,
        ),
        char('}'),
    );

    let (s, fields) = parser(s)?;

    Ok((s, Expr::StructDecl { name, fields }))
}

fn parse_struct_field(s: Span) -> NomResult<Span, FnParameter> {
    let (s, field_name) = parse_var(s)?;
    let (s, _) = multispace0(s)?;
    let (s, _) = tag("->")(s)?;
    let (s, _) = multispace0(s)?;
    let (s, kind) = parse_type(s)?;

    Ok((
        s,
        FnParameter {
            kind,
            name: field_name,
        },
    ))
}

fn parse_extends_decl(s: Span) -> NomResult<Span, Expr> {
    let (s, _) = multispace0(s)?;
    let (s, _) = tag("extends")(s)?;
    let (s, _) = multispace1(s)?;
    let (s, name) = parse_struct_ident(s)?;
    let (s, _) = multispace0(s)?;
    let (s, _) = char('{')(s)?;
    let (s, _) = multispace0(s)?;

    let mut parser = delimited(
        multispace0,
        separated_list0(multispace0, parse_fn_decl),
        multispace0,
    );

    let (s, functions) = parser(s)?;

    let (s, _) = multispace0(s)?;
    let (s, _) = char('}')(s)?;

    Ok((s, Expr::Extends { name, functions }))
}

fn parse_var_expr(s: Span) -> NomResult<Span, Expr> {
    let (s, var) = parse_var(s)?;
    Ok((s, Expr::Variable(var)))
}

fn parse_maths_op(s: Span) -> NomResult<Span, char> {
    alt((char('+'), char('-'), char('/'), char('*')))(s)
}

fn parse_maths_int(s: Span) -> NomResult<Span, Expr> {
    let (s, int1) = parse_int(s)?;
    let (s, _) = multispace0(s)?;
    let (s, op) = parse_maths_op(s)?;
    let (s, _) = multispace0(s)?;
    let (s, int2) = parse_int(s)?;

    let Expr::Primitive(Primitive::Number(left_num)) = int1 else {
        panic!("Number doesn't exist!");
    };

    let Expr::Primitive(Primitive::Number(right_num)) = int2 else {
        panic!("Number doesn't exist!");
    };

    let res = match op {
        '+' => left_num + right_num,
        '-' => left_num - right_num,
        '/' => left_num / right_num,
        '*' => left_num * right_num,
        _ => todo!(),
    };

    Ok((s, Expr::Primitive(res.into())))
}

fn parse_list(s: Span) -> NomResult<Span, Expr> {
    let mut parser = delimited(char('['), separated_list0(char(','), parse_expr), char(']'));

    let (s, list_items) = parser(s)?;
    Ok((s, Expr::Primitive(Primitive::List(list_items))))
}

fn parse_bool(s: Span) -> NomResult<Span, Expr> {
    let (s, bool) = alt((tag("true"), tag("false")))(s)?;

    let bool: bool = FromStr::from_str(bool.fragment()).unwrap();

    Ok((s, Expr::Primitive(Primitive::Bool(bool))))
}

fn parse_str(s: Span) -> NomResult<Span, Expr> {
    let mut parser = delimited(
        char('"'),
        escaped(none_of("\\\""), '\\', one_of("\"n\\")), // Handle escaped characters within quotes
        char('"'),
    );

    let (s, string) = parser(s)?;

    Ok((s, Expr::Primitive(string.fragment().to_string().into())))
}

fn parse_int(s: Span) -> NomResult<Span, Expr> {
    let (s, num) = take_while1(|c: char| c.is_ascii_digit())(s)?;

    let num_parsed: i64 = num.fragment().parse().unwrap();
    Ok((s, Expr::Primitive(num_parsed.into())))
}

fn parse_import(s: Span) -> NomResult<Span, Expr> {
    let (s, _) = take_until("plunder")(s)?;
    let (s, _) = tag("plunder")(s)?;
    let (s, _) = multispace0(s)?;

    let (s, import_name) = parse_var(s)?;

    Ok((s, Expr::Import(import_name)))
}

fn parse_expr(s: Span) -> NomResult<Span, Expr> {
    alt((
        parse_chained_fn_call,
        parse_struct_init,
        parse_maths_int,
        parse_primitive,
        parse_fn_call,
        parse_if,
        parse_var_expr,
    ))(s)
}

fn parse_type(s: Span) -> NomResult<Span, Typename> {
    let mut parser = alt((tag("string"), tag("number"), tag("bool")));

    let (s, value) = parser(s)?;

    let val = match *value.fragment() {
        "string" => Typename::String,
        "number" => Typename::Number,
        "bool" => Typename::Bool,
        _ => todo!("Handle when none of the types exist"),
    };

    Ok((s, val))
}

// Helper to parse a function parameter
fn parse_param(s: Span) -> NomResult<Span, FnParameter> {
    let (s, name) = parse_var(s)?;
    let (s, _) = char(':')(s)?; // Match the ':' separating name and type
    let (s, _) = multispace0(s)?;
    let (s, kind) = parse_type(s)?; // Parse the type

    Ok((s, FnParameter { name, kind }))
}

fn parse_fn_body(s: Span) -> NomResult<Span, Expr> {
    let (s, _) = char('{')(s)?;
    let (s, _) = multispace0(s)?;

    let mut statements = Vec::new();
    let mut s = s;

    while let Ok((next_s, stmt)) = parse_statement(s) {
        statements.push(stmt);
        s = next_s;
    }

    let (s, _) = multispace0(s)?;
    let (s, _) = char('}')(s)?;

    Ok((s, Expr::FnBody(statements)))
}

fn parse_return(s: Span) -> NomResult<Span, Expr> {
    let (s, _) = tag("return")(s)?;
    let (s, _) = multispace0(s)?;
    let (s, expr) = parse_expr(s)?;

    Ok((s, expr))
}

fn parse_fn_decl(s: Span) -> NomResult<Span, Expr> {
    let (s, _) = multispace0(s)?;
    let (s, _) = tag("plan")(s)?;
    let (s, _) = multispace1(s)?;
    let (s, fn_name) = parse_var(s)?;
    let (s, fn_params) = delimited(
        char('('),
        separated_list0(char(','), parse_param),
        char(')'),
    )(s)?;
    let (s, _) = multispace0(s)?;

    let (s, body) = parse_fn_body(s)?;

    Ok((
        s,
        Expr::FnDecl {
            fn_name,
            fn_params,
            body: Box::new(body),
        },
    ))
}

fn parse_fn_call(s: Span) -> NomResult<Span, Expr> {
    let (s, _) = multispace0(s)?;
    let (s, fn_name) = parse_var(s)?;
    let mut parser = delimited(char('('), separated_list0(char(','), parse_expr), char(')'));

    let (s, params) = parser(s)?;

    Ok((
        s,
        Expr::FnCall {
            name: fn_name,
            params,
        },
    ))
}

fn parse_primitive(s: Span) -> NomResult<Span, Expr> {
    alt((parse_str, parse_int, parse_bool, parse_list))(s)
}

fn parse_var_decl(s: Span) -> NomResult<Span, Expr> {
    let (s, _) = multispace0(s)?;
    let (s, _) = tag("yarr")(s)?;
    let (s, _) = multispace1(s)?;
    let (s, (name, expr)) = pair(
        terminated(parse_var, preceded(opt(char(' ')), tag("be"))),
        preceded(opt(char(' ')), parse_expr),
    )(s)?;

    Ok((
        s,
        Expr::Assign {
            name,
            expr: Box::new(expr),
        },
    ))
}

fn parse_eof(s: Span) -> NomResult<Span, Expr> {
    let (s, val) = eof(s)?;

    if s == val {
        Ok((s, Expr::Eof))
    } else {
        panic!("Unexpected EOF")
    }
}

pub fn parse_decl(s: Span) -> NomResult<Span, Expr> {
    alt((
        parse_struct_decl,
        parse_var_decl,
        parse_fn_decl,
        parse_extends_decl,
    ))(s)
}

pub fn parse_statement(s: Span) -> NomResult<Span, Expr> {
    alt((
        parse_chained_fn_call,
        parse_maths_int,
        parse_import,
        parse_decl,
        parse_fn_call,
        parse_expr,
        parse_eof,
    ))(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_fn_works() {
        let span = Span::new(
            "
            plan printme(myNumber: number) {
                print(myNumber)
            }
            ",
        );

        let (_, expr) = parse_statement(span).unwrap();

        assert_eq!(
            expr,
            Expr::FnDecl {
                fn_name: "printme".to_string(),
                fn_params: vec![FnParameter {
                    kind: Typename::Number,
                    name: "myNumber".to_string()
                }],
                body: Box::new(Expr::FnBody(vec![Expr::FnCall {
                    name: "print".to_string(),
                    params: vec![Expr::Variable("myNumber".to_string())]
                }]))
            }
        );
    }

    #[test]
    fn test_parse_empty_struct_works() {
        let span = Span::new("struct Meme {}");

        let (_, expr) = parse_statement(span).unwrap();

        assert_eq!(
            expr,
            Expr::StructDecl {
                name: "Meme".to_string(),
                fields: Vec::new()
            }
        );
    }

    #[test]
    fn test_parse_one_field_struct_works() {
        let span = Span::new("struct Meme { name->string, id->number } ");

        let (_, expr) = parse_statement(span).unwrap();

        assert_eq!(
            expr,
            Expr::StructDecl {
                name: "Meme".to_string(),
                fields: vec![
                    FnParameter {
                        name: "name".to_string(),
                        kind: Typename::String
                    },
                    FnParameter {
                        name: "id".to_string(),
                        kind: Typename::Number
                    },
                ]
            }
        );
    }

    #[test]
    fn test_parse_struct_ident_works() {
        let span = Span::new("Meme");

        let (_, expr) = parse_struct_ident(span).unwrap();

        assert_eq!(expr, "Meme".to_string())
    }

    #[test]
    fn test_parse_chained_fn_call_works() {
        let span = Span::new("\"Meme\".push_str(\"lord\").contains(\"m\")");

        let (_, expr) = parse_chained_fn_call(span).unwrap();

        assert_eq!(
            expr,
            Expr::ChainedFnCall {
                caller: Box::new(Expr::Primitive(Primitive::String("Meme".into()))),
                fn_calls: vec![
                    Expr::FnCall {
                        name: "push_str".into(),
                        params: vec![Expr::string("lord".into())]
                    },
                    Expr::FnCall {
                        name: "contains".into(),
                        params: vec![Expr::string("m".into())]
                    }
                ]
            }
        )
    }

    #[test]
    fn parse_extends_works() {
        let s = Span::new(
            "
            struct Meme {}

            extends Meme {
                plan meme() {}
            }
            ",
        );

        let (s, _) = parse_struct_decl(s).unwrap();

        assert!(parse_extends_decl(s).is_ok())
    }
}
