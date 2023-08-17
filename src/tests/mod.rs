use crate::scanner;
use crate::SourceFile;
use crate::TokenStream;

use std::rc::Rc;

mod keywords;
mod punct;
mod simple;
mod whitespace;

fn scan(source: &str) -> TokenStream {
    let (tokens, error) = scanner::scan(Rc::new(SourceFile::new(
        "test data".to_string(),
        source.to_string(),
    )));
    if let Some(error) = error {
        panic!("{:?}", error);
    }
    tokens
}
