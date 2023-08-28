use flexi_parse::group::Group;
use flexi_parse::group::Parentheses;
use flexi_parse::parse;
use flexi_parse::parse_string;
use flexi_parse::pretty_unwrap;
use flexi_parse::token;
use flexi_parse::Parse;
use flexi_parse::ParseStream;
use flexi_parse::Punct;
use flexi_parse::Result;

use std::env;

enum Expr {
    Num(f64),
    Neg(Punct!["-"], Box<Expr>),
    Mul(Box<Expr>, Punct!["*"], Box<Expr>),
    Div(Box<Expr>, Punct!["/"], Box<Expr>),
    Mod(Box<Expr>, Punct!["%"], Box<Expr>),
    Add(Box<Expr>, Punct!["+"], Box<Expr>),
    Sub(Box<Expr>, Punct!["-"], Box<Expr>),
}

impl Expr {
    fn eval(&self) -> f64 {
        match self {
            Expr::Num(num) => *num,
            Expr::Neg(_, expr) => -expr.eval(),
            Expr::Mul(left, _, right) => left.eval() * right.eval(),
            Expr::Div(left, _, right) => left.eval() / right.eval(),
            Expr::Mod(left, _, right) => left.eval() % right.eval(),
            Expr::Add(left, _, right) => left.eval() + right.eval(),
            Expr::Sub(left, _, right) => left.eval() - right.eval(),
        }
    }
}

impl Parse for Expr {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        term(input)
    }
}

fn term(input: ParseStream<'_>) -> Result<Expr> {
    let mut expr = factor(input)?;
    loop {
        if input.peek(Punct!["+"]) {
            expr = Expr::Add(Box::new(expr), input.parse()?, Box::new(factor(input)?));
        } else if input.peek(Punct!["-"]) {
            expr = Expr::Sub(Box::new(expr), input.parse()?, Box::new(factor(input)?));
        } else {
            break;
        }
    }
    Ok(expr)
}

fn factor(input: ParseStream<'_>) -> Result<Expr> {
    let mut expr: Expr = unary(input)?;
    loop {
        if input.peek(Punct!["*"]) {
            expr = Expr::Mul(Box::new(expr), input.parse()?, Box::new(unary(input)?));
        } else if input.peek(Punct!["/"]) {
            expr = Expr::Div(Box::new(expr), input.parse()?, Box::new(unary(input)?));
        } else if input.peek(Punct!["%"]) {
            expr = Expr::Mod(Box::new(expr), input.parse()?, Box::new(unary(input)?));
        } else {
            break;
        }
    }
    Ok(expr)
}

fn unary(input: ParseStream<'_>) -> Result<Expr> {
    if input.peek(Punct!["-"]) {
        Ok(Expr::Neg(input.parse()?, Box::new(unary(input)?)))
    } else {
        primary(input)
    }
}

fn primary(input: ParseStream<'_>) -> Result<Expr> {
    let lookahead = input.lookahead();
    if lookahead.peek(token::LitFloat) {
        Ok(Expr::Num(input.parse::<token::LitFloat>()?.value()))
    } else if lookahead.peek(token::LitInt) {
        Ok(Expr::Num(input.parse::<token::LitInt>()?.value() as f64))
    } else if lookahead.peek(token::LeftParen) {
        let group: Group<Parentheses> = input.parse()?;
        parse(group.into_token_stream())
    } else {
        Err(lookahead.error())
    }
}

fn main() {
    let expr: Expr = pretty_unwrap(parse_string(env::args().nth(1).expect("expect expression")));
    println!("{}", expr.eval());
}

#[cfg(test)]
mod tests {
    use super::Expr;

    use flexi_parse::parse_string;
    use flexi_parse::pretty_unwrap;

    #[test]
    fn test_features() {
        let expr: Expr = pretty_unwrap(parse_string(
            "
                (
                    (
                        (4 - 1) + 5
                    ) / (2.5 + -0.5) * 2
                ) % 3
            "
            .to_string(),
        ));
        assert_eq!(expr.eval(), 2.0);
    }
}
