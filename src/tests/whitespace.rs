use crate::token::Ident;
use crate::token::NewLine;
use crate::token::Space2;
use crate::token::Space4;
use crate::ParseStream;
use crate::Parser;
use crate::Result;

use super::scan;

#[test]
fn space2() {
    fn space2_inner(input: ParseStream<'_>) -> Result<(String, String, String)> {
        let _: Space2 = input.parse().unwrap();
        let test: Ident = input.parse().unwrap();
        let testing: Ident = input.parse().unwrap();
        let _: NewLine = input.parse().unwrap();
        let _: Space2 = input.parse().unwrap();
        let still_testing: Ident = input.parse().unwrap();
        Ok((test.string, testing.string, still_testing.string))
    }

    // Three spaces after 'test'.
    let mut tokens = scan("  test   testing\n  still_testing");
    tokens.prepare_whitespace();
    let (test, testing, still_testing) = space2_inner.parse(tokens).unwrap();
    assert_eq!(test, "test");
    assert_eq!(testing, "testing");
    assert_eq!(still_testing, "still_testing");
}

#[test]
fn space4() {
    fn space4_inner(input: ParseStream<'_>) -> Result<(String, String, String)> {
        let _: Space4 = input.parse().unwrap();
        let test: Ident = input.parse().unwrap();
        let testing: Ident = input.parse().unwrap();
        let _: NewLine = input.parse().unwrap();
        let _: Space4 = input.parse().unwrap();
        let still_testing: Ident = input.parse().unwrap();
        Ok((test.string, testing.string, still_testing.string))
    }

    // Five spaces after 'test'.
    let mut tokens = scan("    test     testing\n    still_testing");
    tokens.prepare_whitespace();
    let (test, testing, still_testing) = space4_inner.parse(tokens).unwrap();
    assert_eq!(test, "test");
    assert_eq!(testing, "testing");
    assert_eq!(still_testing, "still_testing");
}

#[test]
fn skip() {
    fn skip_inner(input: ParseStream<'_>) -> Result<(String, String)> {
        let _: Space4 = input.parse().unwrap();
        input.skip_whitespace();
        let test: Ident = input.parse().unwrap();
        let _: Space2 = input.parse().unwrap();
        input.skip_whitespace();
        let _: NewLine = input.parse().unwrap();
        let _: Space2 = input.parse().unwrap();
        input.skip_whitespace();
        let testing: Ident = input.parse().unwrap();
        Ok((test.string, testing.string))
    }

    let tokens = scan("      test    \n      testing");
    let (test, testing) = skip_inner.parse(tokens).unwrap();
    assert_eq!(test, "test");
    assert_eq!(testing, "testing");
}
