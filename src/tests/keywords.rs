use crate::parse;
use crate::Parser;

use super::scan;

mod kw {
    use crate::keywords;

    keywords!["let", "if", "for"];
}

#[test]
fn keyword() {
    parse::<kw::keyword_let>(scan("let")).unwrap();
}

#[test]
fn ident() {
    Parser::parse(kw::ident, scan("not_a_keyword")).unwrap();
}

#[test]
fn ident_fail() {
    Parser::parse(kw::ident, scan("let")).unwrap_err();
}
