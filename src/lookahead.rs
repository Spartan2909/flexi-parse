//! A utility for checking the type of the next token.

use crate::error::Error;
use crate::token::Token;
use crate::ParseBuffer;

use std::cell::RefCell;
use std::collections::HashSet;

/// A type for peeking at the next token, and generating a helpful error if it
/// isn't an expected type.
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

    /// Returns true if the next token is the given type.
    pub fn peek<T: Token>(&self) -> bool {
        if self.stream.peek::<T>() {
            true
        } else {
            self.comparisons.borrow_mut().insert(T::display());
            false
        }
    }

    /// Generates an error based on the peek attempts.
    pub fn error(self) -> Error {
        self.stream.unexpected_token(self.comparisons.into_inner())
    }
}
