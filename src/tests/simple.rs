use crate::parse;
use crate::token::Ident;
use crate::token::LitChar;
use crate::token::LitFloat;
use crate::token::LitInt;
use crate::token::LitStrDoubleQuote;
use crate::token::LitStrSingleQuote;
use crate::ParseStream;
use crate::Parser;
use crate::Result;

use super::scan;

#[test]
fn sequential_idents() {
    fn sequential_idents_inner(input: ParseStream<'_>) -> Result<(Ident, Ident)> {
        let hello: Ident = input.parse()?;
        let world: Ident = input.parse()?;
        Ok((hello, world))
    }

    let (hello, world) = sequential_idents_inner.parse(scan("hello world")).unwrap();
    assert_eq!(
        format!("{} {}", hello.string(), world.string()),
        "hello world"
    );
}

#[test]
fn literals() {
    let c: LitChar = parse(scan("'s'")).unwrap();
    assert_eq!(c.ch(), 's');
    let f: LitFloat = parse(scan("13.46")).unwrap();
    assert_eq!(f.value(), 13.46);
    let i: LitInt = parse(scan("67")).unwrap();
    assert_eq!(i.value(), 67);
    let s1: LitStrDoubleQuote = parse(scan("\"It's-a me, Mario!\"")).unwrap();
    assert_eq!(s1.string(), "It's-a me, Mario!");
    let s2: LitStrSingleQuote = parse(scan("'Hello, world!'")).unwrap();
    assert_eq!(s2.string(), "Hello, world!");
}
