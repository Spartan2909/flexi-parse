use crate::error::Error;
use crate::private::Marker;
use crate::private::Sealed;
use crate::token::Token;
use crate::ParseBuffer;
use crate::ParseStream;

use std::cell::RefCell;
use std::collections::HashSet;

use either::Either;

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
    pub fn peek<T: Peek>(&self, token: T) -> bool {
        if self.stream.peek(token) {
            true
        } else {
            self.comparisons.borrow_mut().insert(T::Token::display());
            false
        }
    }

    pub(crate) fn peek_turbofish<T: Token>(&self) -> bool {
        if self.stream.peek_turbofish::<T>() {
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

/// Types that can be parsed by looking at a single token.
///
/// This trait is sealed, and cannot be implemented for types outside of this
/// crate.
pub trait Peek: Sealed {
    #[doc(hidden)]
    type Token: Token;
}

impl<F: FnOnce(Marker) -> T, T: Token> Sealed for F {}

impl<F: FnOnce(Marker) -> T, T: Token> Peek for F {
    type Token = T;
}

/// A trait for types that represent one of a few possible tokens.
///
/// This allows peeking for multiple types through the provided trait methods.
///
/// The `TokenTypes` type should be token, or an [`Either<L, R>`], where `L` is
/// a token, and `R` is either a token or another [`Either<L, R>`] with the same
/// bound.
pub trait TokenLike {
    /// The token types that this type can represent. See
    /// [the trait documentation](Self) for more details.
    type TokenTypes: TokenTypes;

    /// Returns `true` if the next token in `input` is any of the types in
    /// [`Self::TokenTypes`].
    ///
    /// The default implementation should never be overriden.
    fn peek(input: ParseStream<'_>) -> bool {
        Self::TokenTypes::peek(input)
    }

    /// Returns `true` if `lookahead` is pointing to any of the types in
    /// [`Self::TokenTypes`].
    ///
    /// The default implementation should never be overriden.
    fn lookahead(lookahead: &Lookahead<'_>) -> bool {
        Self::TokenTypes::lookahead(lookahead)
    }
}

pub trait TokenTypes: Sealed {
    fn peek(input: ParseStream<'_>) -> bool;
    fn lookahead(lookahead: &Lookahead<'_>) -> bool;
}

impl<T: Token> TokenTypes for T {
    fn peek(input: ParseStream<'_>) -> bool {
        input.peek_turbofish::<T>()
    }

    fn lookahead(lookahead: &Lookahead<'_>) -> bool {
        lookahead.peek_turbofish::<T>()
    }
}

impl<L: Token, R: TokenTypes> Sealed for Either<L, R> {}

impl<L: Token, R: TokenTypes> TokenTypes for Either<L, R> {
    fn peek(input: ParseStream<'_>) -> bool {
        L::peek(input) || R::peek(input)
    }

    fn lookahead(lookahead: &Lookahead<'_>) -> bool {
        L::lookahead(lookahead) || R::lookahead(lookahead)
    }
}
