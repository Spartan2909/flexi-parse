use crate::scanner;
use crate::SourceFile;
use crate::TokenStream;

use std::sync::Arc;

mod keywords;
mod punct;
mod simple;
mod whitespace;

fn scan(source: &str) -> TokenStream {
    let (tokens, error) = scanner::scan(
        Arc::new(SourceFile::new("test data".to_string(), source.to_string())),
        0,
        None,
    );
    if let Some(error) = error {
        panic!("{error:?}");
    }
    tokens
}
