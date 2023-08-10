use crate::ParseBuffer;

use super::scan;

mod kw {
    use crate::keywords;

    keywords!["let", "if", "for"];
}

#[test]
fn keywords() {
    ParseBuffer::from(&scan("let"))
        .parse::<kw::keyword_let>()
        .unwrap();
    ParseBuffer::from(&scan("if"))
        .parse::<kw::keyword_let>()
        .unwrap_err();
}

#[test]
fn ident_keywords() {
    ParseBuffer::from(&scan("not_a_keyword"))
        .parse_with(kw::ident)
        .unwrap();
    ParseBuffer::from(&scan("let"))
        .parse_with(kw::ident)
        .unwrap_err();
}
