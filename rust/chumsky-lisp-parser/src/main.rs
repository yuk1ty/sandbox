use chumsky::prelude::*;

#[derive(Debug, Clone)]
pub enum Token {
    Integer(i64),
    Symbol(String),
    Quote,
}

#[derive(Debug, Clone)]
pub enum SExpr {
    Atom(Token),
    List(Vec<SExpr>),
}

fn parser() -> impl Parser<char, SExpr, Error = Simple<char>> {
    let symbol = filter::<_, _, Simple<char>>(|c: &char| c.is_alphabetic())
        .repeated()
        .at_least(1)
        .collect::<String>();
    let num = text::int(10).from_str().unwrapped();
    recursive(|sexpr| {
        sexpr
            .padded()
            .repeated()
            .map(SExpr::List)
            .delimited_by(just('('), just(')'))
            .or(symbol.map(|v| SExpr::Atom(Token::Symbol(v))))
            .or(num.map(|v| SExpr::Atom(Token::Integer(v))))
    })
}

fn main() {
    let r = parser().parse_recovery("(add 42)");
    println!("{:?}", r);
}
