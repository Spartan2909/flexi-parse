use crate::error::Error;
use crate::group::Group;
use crate::group::Parentheses;
use crate::parse;
use crate::punctuated::Punctuated;
use crate::token::Ident;
use crate::token::LitChar;
use crate::token::LitFloat;
use crate::token::LitInt;
use crate::token::LitStrDoubleQuote;
use crate::token::LitStrSingleQuote;
use crate::Lookahead;
use crate::ParseBuffer;
use crate::ParseStream;
use crate::Parser;
use crate::Punct;
use crate::Result;
use crate::Span;

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
#[allow(clippy::float_cmp)]
fn literals() {
    let c: LitChar = parse(scan("'s'")).unwrap();
    assert_eq!(c.ch(), 's');
    println!("char done");
    let i: LitInt = parse(scan("67")).unwrap();
    assert_eq!(i.value(), 67);
    let f: LitFloat = parse(scan("13.46")).unwrap();
    assert_eq!(f.value(), 13.46);
    let s1: LitStrDoubleQuote = parse(scan("\"It's-a me, Mario!\"")).unwrap();
    assert_eq!(s1.string(), "It's-a me, Mario!");
    let s2: LitStrSingleQuote = parse(scan("'Hello, world!'")).unwrap();
    assert_eq!(s2.string(), "Hello, world!");
}

#[test]
const fn send_sync_types() {
    const fn assert_send_sync<T: Send + Sync>() {}

    assert_send_sync::<ParseBuffer>();
    assert_send_sync::<Span>();
    assert_send_sync::<Ident>();
    assert_send_sync::<Group<Parentheses>>();
    assert_send_sync::<Punctuated<Ident, Punct![","]>>();
    assert_send_sync::<Lookahead>();
    assert_send_sync::<Error>();
}
