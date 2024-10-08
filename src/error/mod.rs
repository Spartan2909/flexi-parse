//! Types for error reporting.
//!
//! This module is built around the [`Error`] type, which is returned when any parsing function
//! encounters an error. Typically, these will not be created directly, but instead propagated up
//! from the built-in parsing functions. The primary exception to this is
//! [`Lookahead::error`][lookahead-error].
//!
//! [lookahead-error]: crate::lookahead::Lookahead::error
//! [parsebuffer-error]: crate::ParseBuffer::error

use crate::SourceFile;
use crate::Span;

use std::collections::HashSet;
use std::fmt;
use std::sync::Arc;

#[cfg(feature = "ariadne")]
mod ariadne;
#[cfg(feature = "ariadne")]
pub use self::ariadne::Report;

#[derive(Debug, Clone)]
#[repr(u16)]
pub(crate) enum ErrorKind {
    Silent,
    UnknownCharacter(Span),
    UnterminatedGroup {
        start: String,
        span: Span,
    },
    UnterminatedChar(Span),
    LongChar(Span),
    UnterminatedString(Span),
    UnexpectedToken {
        expected: HashSet<String>,
        span: Span,
    },
    EndOfFile(usize),
    Custom {
        message: String,
        span: Span,
        code: u16,
    },
}

impl ErrorKind {
    const fn code(&self) -> u16 {
        match self {
            ErrorKind::Silent => 0,
            ErrorKind::UnknownCharacter(_) => 1,
            ErrorKind::UnterminatedGroup { .. } => 2,
            ErrorKind::UnterminatedChar(_) => 3,
            ErrorKind::LongChar(_) => 4,
            ErrorKind::UnterminatedString(_) => 5,
            ErrorKind::UnexpectedToken { .. } => 6,
            ErrorKind::EndOfFile(_) => 7,
            ErrorKind::Custom { code, .. } => 8 + *code,
        }
    }

    fn start(&self) -> usize {
        match self {
            ErrorKind::Silent => panic!("called `start` on `ErrorKind::Silent`"),
            ErrorKind::Custom { span, .. }
            | ErrorKind::UnknownCharacter(span)
            | ErrorKind::UnterminatedGroup { span, .. }
            | ErrorKind::UnterminatedChar(span)
            | ErrorKind::LongChar(span)
            | ErrorKind::UnterminatedString(span)
            | ErrorKind::UnexpectedToken { span, .. } => span.start(),
            ErrorKind::EndOfFile(n) => *n,
        }
    }
}

fn unexpected_token_message(expected: &HashSet<String>) -> String {
    if expected.len() == 1 {
        format!("Expected {}", expected.iter().next().unwrap())
    } else if expected.len() == 2 {
        let mut iter = expected.iter();
        format!(
            "Expected {} or {}",
            iter.next().unwrap(),
            iter.next().unwrap()
        )
    } else {
        let mut message = "Expected one of: ".to_string();
        for (i, token) in expected.iter().enumerate() {
            message.push_str(token);
            if i + 1 < expected.len() {
                message.push_str(", ");
            }
        }
        message
    }
}

#[derive(Debug, Clone)]
pub(crate) struct SingleError {
    source: Arc<SourceFile>,
    kind: ErrorKind,
}

/// An error or collection of errors raised during parsing.
///
/// These errors are intended to be reported using [`ariadne`][ariadne], but an implementation of
/// [`ToString`] is provided as an alternative if that is not possible.
///
/// [ariadne]: https://docs.rs/ariadne/latest/ariadne/
#[derive(Debug, Clone)]
pub struct Error {
    errors: Vec<SingleError>,
}

impl Error {
    pub(crate) fn new(source: Arc<SourceFile>, kind: ErrorKind) -> Error {
        Error {
            errors: vec![SingleError { source, kind }],
        }
    }

    pub(crate) const fn empty() -> Error {
        Error { errors: vec![] }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub(crate) fn eof_to_group(&mut self, span: &Span, start: &str) {
        for error in &mut self.errors {
            if let ErrorKind::EndOfFile(_) = error.kind {
                error.kind = ErrorKind::UnterminatedGroup {
                    start: start.to_string(),
                    span: span.clone(),
                };
            }
        }
    }

    pub(crate) fn group_to_string(&mut self) {
        for error in &mut self.errors {
            if let ErrorKind::UnterminatedGroup { span, .. } = &error.kind {
                error.kind = ErrorKind::UnterminatedString(span.clone());
            }
        }
    }

    pub(crate) fn string_to_char(&mut self) {
        for error in &mut self.errors {
            if let ErrorKind::UnterminatedString(span) = &error.kind {
                error.kind = ErrorKind::UnterminatedChar(span.clone());
            }
        }
    }

    /// Appends the given error to this one.
    pub fn add(&mut self, mut other: Error) {
        self.errors.append(&mut other.errors);
    }

    /// Consumes `self` and `other`, returning a new error with the contents of
    /// both.
    #[must_use]
    pub fn with(mut self, other: Error) -> Self {
        self.add(other);
        self
    }

    pub(crate) fn details(&self) -> Option<String> {
        #[cfg(feature = "ariadne")]
        let details = {
            let mut buf = vec![];
            self.to_reports()
                .into_iter()
                .try_for_each(|report| report.write(&mut buf))
                .ok()
                .and_then(|()| String::from_utf8(buf).ok())
        };

        #[cfg(not(feature = "ariadne"))]
        let details = None;

        details
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for error in &self.errors {
            let file_name = error.source.name();
            match &error.kind {
                ErrorKind::Silent => {}
                ErrorKind::Custom { message, span, .. } => {
                    writeln!(f, "[E{:02}] Error: {}", error.kind.code(), message)?;
                    let (line, col) = span.start_location();
                    write!(f, "[{file_name}:{line}:{col}]")?;
                }
                ErrorKind::UnknownCharacter(span) => {
                    writeln!(
                        f,
                        "[E{:02}] Error: Unrecognised character",
                        error.kind.code()
                    )?;
                    let (line, col) = span.start_location();
                    write!(f, "[{file_name}:{line}:{col}]")?;
                }
                ErrorKind::UnterminatedGroup { start, span } => {
                    writeln!(
                        f,
                        "[E{:02}] Error: Unmatched '{}'",
                        error.kind.code(),
                        start
                    )?;
                    let (line, col) = span.start_location();
                    write!(f, "[{file_name}:{line}:{col}]")?;
                }
                ErrorKind::UnterminatedChar(span) => {
                    writeln!(
                        f,
                        "[E{:02}] Error: Unterminated character literal",
                        error.kind.code()
                    )?;
                    let (line, col) = span.start_location();
                    write!(f, "[{file_name}:{line}:{col}]")?;
                }
                ErrorKind::LongChar(span) => {
                    writeln!(
                        f,
                        "[E{:02}] Error: Character literals must be exactly one character long",
                        error.kind.code()
                    )?;
                    let (line, col) = span.start_location();
                    write!(f, "[{file_name}:{line}:{col}]")?;
                }
                ErrorKind::UnterminatedString(span) => {
                    writeln!(
                        f,
                        "[E{:02}] Error: Unterminated string literal",
                        error.kind.code()
                    )?;
                    let (line, col) = span.start_location();
                    write!(f, "[{file_name}:{line}:{col}]")?;
                }
                ErrorKind::UnexpectedToken { expected, span } => {
                    writeln!(f, "[E{:02}] Error: Unexpected token", error.kind.code())?;
                    let (line, col) = span.start_location();
                    writeln!(f, "[{file_name}:{line}:{col}]")?;
                    write!(f, "{}", unexpected_token_message(expected))?;
                }
                ErrorKind::EndOfFile(_) => write!(f, "Unexpected end of file while parsing")?,
            }
        }

        Ok(())
    }
}
