use crate::error::Error;
use crate::token::Token;
use crate::ParseBuffer;

use std::cell::RefCell;
use std::collections::HashSet;

pub struct Lookahead<'a> {
    stream: ParseBuffer<'a>,
    comparisons: RefCell<HashSet<String>>,
}

impl<'a> Lookahead<'a> {
    pub(crate) fn new(stream: ParseBuffer<'a>) -> Lookahead<'a> {
        Lookahead {
            stream,
            comparisons: RefCell::new(HashSet::new()),
        }
    }

    pub fn peek<T: Token>(&self) -> bool {
        if self.stream.peek::<T>() {
            true
        } else {
            self.comparisons.borrow_mut().insert(T::display());
            false
        }
    }

    pub fn error(self) -> Error {
        let span = match self.stream.current() {
            Ok((_, token)) => token.span().clone(),
            Err(err) => return err,
        };
        Error::unexpected_token(self.comparisons.into_inner(), span, self.stream.source)
    }
}
