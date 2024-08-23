use crate::token::Token;
use crate::Entry;
use crate::TokenStream;

use std::fmt;

impl fmt::Display for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut last_token_end = usize::MAX;
        let mut this_token_span;
        for token in &self.tokens {
            let string = match token {
                Entry::Error(_) => return Err(fmt::Error),
                Entry::Ident(ident) => {
                    this_token_span = ident.span.clone();
                    ident.string().to_owned()
                }
                Entry::Punct(punct) => {
                    this_token_span = punct.span.clone();
                    char::from(punct.kind).to_string()
                }
                Entry::WhiteSpace(whitespace) => {
                    this_token_span = whitespace.span().clone();
                    whitespace.display()
                }
                #[cfg(feature = "scan-strings")]
                Entry::LitStrDoubleQuote(str) => {
                    this_token_span = str.span().clone();
                    format!(r#""{}""#, str.string())
                }
                #[cfg(feature = "scan-strings")]
                Entry::LitStrSingleQuote(str) => {
                    this_token_span = str.span().clone();
                    format!("'{}'", str.string())
                }
            };
            for _ in last_token_end..this_token_span.start() {
                write!(f, " ")?;
            }
            write!(f, "{string}")?;
            last_token_end = this_token_span.end();
        }

        Ok(())
    }
}
