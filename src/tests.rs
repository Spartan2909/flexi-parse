use crate::ParseBuffer;
use crate::TokenStream;
use crate::scanner;
use crate::SourceFile;

use std::rc::Rc;

mod kw {
    use crate::keywords;

    keywords!["let", "if", "for"];
}

fn scan(source: &str) -> TokenStream {
    let (tokens, error) = scanner::scan(Rc::new(SourceFile::new("test data".to_string(), source.to_string())));
    assert!(error.is_none());
    tokens
}

#[test]
fn keywords() {
    ParseBuffer::from(&scan("let")).parse::<kw::keyword_let>().unwrap();
    ParseBuffer::from(&scan("if")).parse::<kw::keyword_let>().unwrap_err();
}

#[test]
fn ident_keywords() {
    ParseBuffer::from(&scan("not_a_keyword")).parse_with(kw::ident).unwrap();
    ParseBuffer::from(&scan("let")).parse_with(kw::ident).unwrap_err();
}
