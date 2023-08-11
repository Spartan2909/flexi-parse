use crate::Span;
use crate::TokenStream;
use crate::TokenTree;

use std::fmt;

impl fmt::Display for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut last_token_end = 0;
        let mut this_token_span = Span::dummy();
        for (_, token) in &self.tokens {
            match token {
                TokenTree::Error(_) => Err(fmt::Error)?,
                TokenTree::Ident(ident) => {
                    write!(f, "{}", ident.string())?;
                    this_token_span = ident.span.clone();
                }
                TokenTree::Punct(punct) => {
                    write!(f, "{}", char::from(punct.kind))?;
                    this_token_span = punct.span.clone();
                }
                TokenTree::Literal(lit) => {
                    write!(f, "{}", &lit.value)?;
                }
                TokenTree::End => break,
            }
            for _ in last_token_end..this_token_span.start {
                write!(f, " ")?;
            }
            last_token_end = this_token_span.end;
        }

        Ok(())
    }
}
