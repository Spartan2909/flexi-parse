use crate::scanner;
use crate::SourceFile;
use crate::TokenStream;

use std::rc::Rc;

mod keywords;
mod punct;

fn scan(source: &str) -> TokenStream {
    let (tokens, error) = scanner::scan(Rc::new(SourceFile::new(
        "test data".to_string(),
        source.to_string(),
    )));
    assert!(error.is_none(), "scan failed");
    tokens
}
