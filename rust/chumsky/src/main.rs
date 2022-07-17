use std::env::args;
use std::fs::read_to_string;

use chumsky::prelude::*;

#[derive(Debug, PartialEq)]
enum Expr {
    Num(f64),
    Var(String),

    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),

    Call(String, Vec<Expr>),
    Let {
        name: String,
        rhs: Box<Expr>,
        then: Box<Expr>,
    },
    Fn {
        name: String,
        args: Vec<String>,
        body: Box<Expr>,
        then: Box<Expr>,
    },
}

fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    let ident = text::ident().padded();
    let expr = recursive(|expr| {
        let int = text::int(10)
            .map(|s: String| Expr::Num(s.parse().unwrap()))
            .padded();
        let atom = int.or(expr.delimited_by(just('('), just(')')));
        let op = |c| just(c).padded();
        let unary = op('-')
            .repeated()
            .then(atom)
            .foldr(|_op, rhs| Expr::Neg(Box::new(rhs)));

        let product = unary
            .clone()
            .then(
                op('*')
                    .to(Expr::Mul as fn(_, _) -> _)
                    .or(op('/').to(Expr::Div as fn(_, _) -> _))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        let sum = product
            .clone()
            .then(
                op('+')
                    .to(Expr::Add as fn(_, _) -> _)
                    .or(op('-').to(Expr::Sub as fn(_, _) -> _))
                    .then(product)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        sum
    })
    .then_ignore(end());

    let decl = recursive(|decl| {
        let r#let = text::keyword("let")
            .ignore_then(ident)
            .then_ignore(just('='))
            .then(expr.clone())
            .then_ignore(just(';'))
            .then(decl)
            .map(|((name, rhs), then)| Expr::Let {
                name,
                rhs: Box::new(rhs),
                then: Box::new(then),
            });

        r#let.or(expr).padded()
    });
    decl.then_ignore(end())
}

fn eval<'a>(expr: &'a Expr, vars: &mut Vec<(&'a String, f64)>) -> Result<f64, String> {
    match expr {
        Expr::Num(x) => Ok(*x),
        Expr::Neg(a) => Ok(-eval(a, vars)?),
        Expr::Add(a, b) => Ok(eval(a, vars)? + eval(b, vars)?),
        Expr::Sub(a, b) => Ok(eval(a, vars)? - eval(b, vars)?),
        Expr::Mul(a, b) => Ok(eval(a, vars)? * eval(b, vars)?),
        Expr::Div(a, b) => Ok(eval(a, vars)? / eval(b, vars)?),
        Expr::Var(name) => {
            if let Some((_, val)) = vars.iter().rev().find(|(var, _)| *var == name) {
                Ok(*val)
            } else {
                Err(format!("Cannot find variable `{}` in scope", name))
            }
        }
        Expr::Let { name, rhs, then } => {
            let rhs = eval(rhs, vars)?;
            vars.push((name, rhs));
            let output = eval(then, vars);
            vars.pop();
            output
        }
        _ => todo!(),
    }
}

fn main() {
    let src = read_to_string(args().nth(1).unwrap()).unwrap();

    match parser().parse(src) {
        Ok(ast) => match eval(&ast, &mut Vec::new()) {
            Ok(output) => println!("{}", output),
            Err(eval_err) => println!("Evaluation error: {}", eval_err),
        },
        Err(parser_errs) => parser_errs
            .into_iter()
            .for_each(|e| println!("Parse error: {}", e)),
    }
}

#[test]
fn parse_digit() {
    let text = "5";
    let result = parser().parse(text);
    assert_eq!(result, Ok(Expr::Num(5.0)));

    let text = "42";
    let result = parser().parse(text);
    assert_eq!(result, Ok(Expr::Num(42.0)));
}

#[test]
fn parse_negate() {
    let text = "-42";
    let result = parser().parse(text);
    assert_eq!(result, Ok(Expr::Neg(Box::new(Expr::Num(42.0)))));

    let text = "--42";
    let result = parser().parse(text);
    assert_eq!(
        result,
        Ok(Expr::Neg(Box::new(Expr::Neg(Box::new(Expr::Num(42.0))))))
    );
}

#[test]
fn parse_binary() {
    let text = "1 + 2";
    let result = parser().parse(text);
    assert_eq!(
        result,
        Ok(Expr::Add(
            Box::new(Expr::Num(1.0)),
            Box::new(Expr::Num(2.0))
        ))
    );

    let text = "1 - 2";
    let result = parser().parse(text);
    assert_eq!(
        result,
        Ok(Expr::Sub(
            Box::new(Expr::Num(1.0)),
            Box::new(Expr::Num(2.0))
        ))
    );

    let text = "1 * 2";
    let result = parser().parse(text);
    assert_eq!(
        result,
        Ok(Expr::Mul(
            Box::new(Expr::Num(1.0)),
            Box::new(Expr::Num(2.0))
        ))
    );

    let text = "1 / 2";
    let result = parser().parse(text);
    assert_eq!(
        result,
        Ok(Expr::Div(
            Box::new(Expr::Num(1.0)),
            Box::new(Expr::Num(2.0))
        ))
    );
}
