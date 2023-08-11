use crate::SourceFile;
use crate::Span;

use std::collections::HashSet;
use std::rc::Rc;

#[cfg(feature = "ariadne")]
mod ariadne;
#[cfg(feature = "ariadne")]
pub use self::ariadne::Report;

#[derive(Debug, Clone)]
#[repr(u8)]
pub(crate) enum ErrorKind {
    Silent,
    UnknownCharacter(Span),
    UnterminatedGroup(Span),
    UnterminatedChar(Span),
    LongChar(Span),
    UnterminatedString(Span),
    UnexpectedToken {
        expected: HashSet<String>,
        span: Span,
    },
    EndOfFile(usize),
}

impl ErrorKind {
    fn discriminant(&self) -> u8 {
        // SAFETY: ErrorKind is `repr(u8)`, making it a `repr(C)` struct with
        // a `u8` as its first field
        unsafe { *<*const _>::from(self).cast::<u8>() }
    }

    fn start(&self) -> usize {
        match self {
            ErrorKind::Silent => panic!("called `start` on `ErrorKind::Silent`"),
            ErrorKind::UnknownCharacter(span)
            | ErrorKind::UnterminatedGroup(span)
            | ErrorKind::UnterminatedChar(span)
            | ErrorKind::LongChar(span)
            | ErrorKind::UnterminatedString(span)
            | ErrorKind::UnexpectedToken { span, .. } => span.start,
            ErrorKind::EndOfFile(n) => *n,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct SingleError {
    source: Rc<SourceFile>,
    kind: ErrorKind,
}

/// An error or collection of errors raised during parsing.
#[derive(Debug, Clone)]
pub struct Error {
    errors: Vec<SingleError>,
}

impl Error {
    pub(crate) fn new(source: Rc<SourceFile>, kind: ErrorKind) -> Error {
        Error {
            errors: vec![SingleError { source, kind }],
        }
    }

    pub(crate) fn empty() -> Error {
        Error { errors: vec![] }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub(crate) fn eof_to_group(&mut self, span: Span) {
        for error in &mut self.errors {
            if let ErrorKind::EndOfFile(_) = error.kind {
                error.kind = ErrorKind::UnterminatedGroup(span.clone());
            }
        }
    }

    pub(crate) fn group_to_string(&mut self) {
        for error in &mut self.errors {
            if let ErrorKind::UnterminatedGroup(span) = &error.kind {
                let span = span.to_owned();
                error.kind = ErrorKind::UnterminatedString(span);
            }
        }
    }

    pub fn add(&mut self, mut other: Error) {
        self.errors.append(&mut other.errors);
    }

    pub fn unexpected_token(
        expected: HashSet<String>,
        span: Span,
        source: Rc<SourceFile>,
    ) -> Error {
        Error::new(source, ErrorKind::UnexpectedToken { expected, span })
    }
}
