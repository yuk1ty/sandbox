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
    let int = text::int(10)
        .map(|s: String| Expr::Num(s.parse().unwrap()))
        .padded();
    int.then_ignore(end())
}

fn main() {
    let src = read_to_string(args().nth(1).unwrap()).unwrap();
    println!("{:?}", parser().parse(src));
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
