use crate::SourceFile;
use crate::Span;

use std::collections::HashSet;

#[cfg(feature = "ariadne")]
mod ariadne;

#[cfg(feature = "ariadne")]
pub use self::ariadne::Report;

#[derive(Debug, Clone)]
#[repr(u8)]
pub(crate) enum ErrorKind<'src> {
    UnknownCharacter(Span<'src>),
    UnterminatedChar(Span<'src>),
    UnexpectedToken {
        expected: HashSet<String>,
        span: Span<'src>,
    },
    EndOfFile(usize),
    UnterminatedString(Span<'src>),
    UnopenedDelimiter(Span<'src>),
}

impl ErrorKind<'_> {
    fn discriminant(&self) -> u8 {
        // SAFETY: ErrorKind is `repr(u8)`, making it a `repr(C)` struct with
        // a `u8` as its first field
        unsafe { *<*const _>::from(self).cast::<u8>() }
    }

    fn start(&self) -> usize {
        match self {
            ErrorKind::UnknownCharacter(span)
            | ErrorKind::UnterminatedChar(span)
            | ErrorKind::UnterminatedString(span)
            | ErrorKind::UnexpectedToken { span, .. }
            | ErrorKind::UnopenedDelimiter(span) => span.start,
            ErrorKind::EndOfFile(n) => *n,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct SingleError<'src> {
    source: &'src SourceFile,
    kind: ErrorKind<'src>
}

/// An error or collection of errors raised during parsing.
#[derive(Debug, Clone)]
pub struct Error<'src> {
    errors: Vec<SingleError<'src>>,
}

impl<'src> Error<'src> {
    pub(crate) fn new(source: &'src SourceFile, kind: ErrorKind<'src>) -> Error<'src> {
        Error { errors: vec![SingleError { source, kind }] }
    }

    pub(crate) fn empty() -> Error<'static> {
        Error { errors: vec![] }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub(crate) fn pop(&mut self) {
        self.errors.pop();
    }

    pub fn add(&mut self, mut other: Error<'src>) {
        self.errors.append(&mut other.errors)
    }
}
