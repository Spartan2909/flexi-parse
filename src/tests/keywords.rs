use crate::ParseBuffer;

use super::scan;

mod kw {
    use crate::keywords;

    keywords!["let", "if", "for"];
}

#[test]
fn keyword() {
    ParseBuffer::from(&scan("let"))
        .parse::<kw::keyword_let>()
        .unwrap();
}

#[test]
fn ident() {
    ParseBuffer::from(&scan("not_a_keyword"))
        .parse_with(kw::ident)
        .unwrap();
}

#[test]
fn keyword_as_ident() {
    ParseBuffer::from(&scan("let"))
        .parse_with(kw::ident)
        .unwrap_err();
}
