use crate::parse;
use crate::Parser;

use super::scan;

mod kw {
    use crate::keywords;

    keywords![let as kw_let, aaa];
}

#[test]
fn keyword() {
    parse::<kw::aaa>(scan("aaa")).unwrap();
    parse::<kw::kw_let>(scan("let")).unwrap();
}

#[test]
fn ident() {
    Parser::parse(kw::ident, scan("not_a_keyword")).unwrap();
}

#[test]
fn ident_fail() {
    kw::ident.parse(scan("aaa")).unwrap_err();
    kw::ident.parse(scan("let")).unwrap_err();
}
