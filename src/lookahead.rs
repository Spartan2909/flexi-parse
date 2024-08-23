use crate::error::Error;
use crate::private::Marker;
use crate::private::Sealed;
use crate::token::Token;
use crate::ParseBuffer;

use std::collections::HashSet;
use std::sync::Mutex;

/// A type for peeking at the next token, and generating a helpful error if it isn't an expected
/// type.
pub struct Lookahead<'a> {
    stream: ParseBuffer<'a>,
    comparisons: Mutex<HashSet<String>>,
}

impl<'a> Lookahead<'a> {
    pub(crate) fn new(stream: ParseBuffer<'a>) -> Lookahead<'a> {
        Lookahead {
            stream,
            comparisons: Mutex::new(HashSet::new()),
        }
    }

    /// Returns true if the next token is the given type.
    ///
    /// ## Panics
    ///
    /// Panics if another thread panicked while peeking through this value.
    pub fn peek<T: Peek>(&self, token: T) -> bool {
        if self.stream.peek(token) {
            true
        } else {
            self.comparisons.lock().unwrap().insert(T::Token::display());
            false
        }
    }

    /// Generates an error based on the peek attempts.
    ///
    /// ## Panics
    ///
    /// Panics if another thread panicked while peeking through this value.
    pub fn error(self) -> Error {
        self.stream
            .unexpected_token(self.comparisons.into_inner().unwrap())
    }
}

/// Types that can be parsed by looking at a single token.
///
/// This trait is sealed, and cannot be implemented for types outside of this crate.
pub trait Peek: Sealed {
    #[doc(hidden)]
    type Token: Token;
}

impl<F: FnOnce(Marker) -> T, T: Token> Sealed for F {}

impl<F: FnOnce(Marker) -> T, T: Token> Peek for F {
    type Token = T;
}
