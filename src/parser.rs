use std::str::FromStr;

use nom::branch::alt;
use nom::bytes::complete::{escaped, tag, take_until, take_while1};
use nom::character::complete::{char, multispace1, none_of, one_of};

use nom::combinator::{eof, opt};

use nom::multi::separated_list0;
use nom::sequence::{delimited, pair, preceded, terminated};
use nom::IResult;
use nom_locate::{position, LocatedSpan};

pub type Span<'a> = LocatedSpan<&'a str>;

use crate::symbols::SymbolTable;

#[derive(Debug)]
enum Ident {
    Function,
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct FnParameter {
    kind: Typename,
    name: String,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Primitive {
    String(String),
    Number(i64),
    Bool(bool),
    List(Vec<Primitive>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Typename {
    String,
    Number,
    Bool,
    List,
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expr {
    Eof,
    Primitive(Primitive),
    Variable(String),
    Typename(Typename),
    FnParameter(FnParameter),
    FnBody(Vec<Expr>),
    FnCall {
        name: String,
        params: Vec<Expr>,
    },
    Assign {
        name: String,
        expr: Box<Expr>,
    },
    If(Box<Expr>, Box<Expr>),
    FnDecl {
        name: String,
        params: Vec<Expr>,
        body: Box<Expr>,
    },
    Fn {
        name: String,
        params: Vec<FnParameter>,
        body: Box<Expr>,
    },
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
    pub fn evaluate(&self, symbols: &mut SymbolTable) -> Result<Expr, String> {
        match self {
            Expr::FnCall { name, params } => todo!(),
            Expr::Eof => Ok(Expr::Eof),
            Expr::Primitive(primitive) => Ok(primitive.to_owned().into()),
            Expr::Typename(kind) => Ok(kind.to_owned().into()),
            Expr::Variable(name) => Ok(*symbols.get(name.to_string())),
            Expr::FnBody(body) => Ok(Expr::FnBody(body.to_owned())),
            Expr::FnParameter(param) => Ok(Expr::FnParameter(param.to_owned())),
            Expr::Assign { name, expr } => {
                let value = expr.evaluate(symbols)?;
                symbols.set(name.clone(), Box::new(value.clone()));
                Ok(value)
            }
            Expr::If(pred, true_branch) => {
                if Expr::Primitive(Primitive::Bool(true)) == **pred {
                    let value = true_branch.evaluate(symbols)?;
                    Ok(value)
                } else {
                    Ok(Expr::Primitive(Primitive::Bool(false)))
                }
            }
            Expr::FnDecl { name, params, body } => {
                todo!()
            }
            Expr::Fn { name, params, body } => {
                todo!()
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

fn parse_if(s: Span) -> IResult<Span, Expr> {
    let (s, _) = terminated(tag("if"), char(' '))(s)?;

    let (s, cond) = parse_expr(s)?;
    let (s, then) = parse_expr(s)?;

    Ok((s, Expr::If(Box::new(cond), Box::new(then))))
}

fn parse_var(s: Span) -> IResult<Span, String> {
    let (s, var_name) = take_while1(|c: char| c.is_alphanumeric())(s)?;
    Ok((s, var_name.fragment().to_string()))
}

fn parse_bool(s: Span) -> IResult<Span, Expr> {
    let (s, bool) = alt((tag("true"), tag("false")))(s)?;

    let bool: bool = FromStr::from_str(bool.fragment()).unwrap();

    Ok((s, Expr::Primitive(Primitive::Bool(bool))))
}

fn parse_str(s: Span) -> IResult<Span, Expr> {
    let mut parser = delimited(
        char('"'),
        escaped(none_of("\\\""), '\\', one_of("\"n\\")), // Handle escaped characters within quotes
        char('"'),
    );

    let (s, string) = parser(s)?;

    Ok((s, Expr::Primitive(string.fragment().to_string().into())))
}

fn parse_num(s: Span) -> IResult<Span, Expr> {
    let (s, num) = take_while1(|c: char| c.is_ascii_digit())(s)?;

    let num_parsed: i64 = num.fragment().parse().unwrap();
    Ok((s, Expr::Primitive(num_parsed.into())))
}

fn parse_expr(s: Span) -> IResult<Span, Expr> {
    alt((parse_if, parse_primitive))(s)
}

fn parse_type(s: Span) -> IResult<Span, Typename> {
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
fn parse_param(s: Span) -> IResult<Span, FnParameter> {
    let (s, name) = parse_var(s)?;
    let (s, _) = char(':')(s)?; // Match the ':' separating name and type
    let (s, kind) = parse_type(s)?; // Parse the type

    Ok((s, FnParameter { name, kind }))
}

fn parse_fn_body(s: Span) -> IResult<Span, Expr> {
    let (s, _) = char('{')(s)?;

    let mut statements = Vec::new();
    let mut s = s;

    while let Ok((next_s, stmt)) = parse_statement(s) {
        statements.push(stmt);
        s = next_s;
    }

    let (s, _) = char('}')(s)?;

    Ok((s, Expr::FnBody(statements)))
}

fn parse_fn_decl(s: Span) -> IResult<Span, Expr> {
    let (s, _) = tag("fn")(s)?;
    let (s, _) = multispace1(s)?;
    let (s, name) = parse_var(s)?;
    let (s, _) = char('(')(s)?;
    let (s, params) = delimited(
        char('('),
        separated_list0(char(','), parse_param),
        char(')'),
    )(s)?;

    let (s, body) = parse_fn_body(s)?;

    todo!()
}

fn parse_fn_call(s: Span) -> IResult<Span, Expr> {
    let (s, fn_name) = parse_var(s)?;
    let mut parser = delimited(
        char('('),
        separated_list0(char(','), parse_primitive),
        char('('),
    );

    let (s, params) = parser(s)?;

    Ok((
        s,
        Expr::FnCall {
            name: fn_name,
            params,
        },
    ))
}

fn parse_primitive(s: Span) -> IResult<Span, Expr> {
    alt((parse_str, parse_num, parse_bool))(s)
}

fn parse_bracketed_expr(s: Span) -> IResult<Span, Expr> {
    let mut parser = delimited(
        tag("{"),
        delimited(multispace1, parse_expr, multispace1),
        tag("}"),
    );

    let (s, bracketed_expr) = parser(s)?;

    Ok((s, bracketed_expr))
}

fn parse_decl(s: Span) -> IResult<Span, Expr> {
    let (s, _) = take_until("yarr")(s)?;
    let (s, _) = tag("yarr")(s)?;
    let (s, pos) = position(s)?;
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

fn parse_eof(s: Span) -> IResult<Span, Expr> {
    let (s, val) = eof(s)?;

    if s == val {
        Ok((s, Expr::Eof))
    } else {
        panic!("Unexpected EOF")
    }
}

pub fn parse_statement(s: Span) -> IResult<Span, Expr> {
    Ok(alt((parse_decl, parse_fn_decl, parse_eof))(s).unwrap())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parsing_should_work() {
        let s = Span::new("yarr me be 42\n yarr it be true\n yarr say be \"Meme!\"");

        let (s, me) = parse_statement(s).unwrap();
        assert_eq!(
            me,
            Expr::Assign {
                name: "me".to_string(),
                expr: Box::new(Expr::Primitive(42.into()))
            }
        );

        let (s, it) = parse_statement(s).unwrap();
        assert_eq!(
            it,
            Expr::Assign {
                name: "it".to_string(),
                expr: Box::new(Expr::Primitive(true.into()))
            }
        );

        let (s, say) = parse_statement(s).unwrap();
        assert_eq!(
            say,
            Expr::Assign {
                name: "say".to_string(),
                expr: Box::new(Expr::Primitive("Meme!".to_string().into()))
            }
        );
    }
}
