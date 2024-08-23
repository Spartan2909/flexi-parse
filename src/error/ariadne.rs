use crate::error::unexpected_token_message;
use crate::error::Error;
use crate::error::ErrorKind;
use crate::error::SingleError;
use crate::SourceCache;
use crate::SourceId;
use crate::Span;
use crate::SOURCE_CACHE;

use std::io;
use std::io::Write;

use ariadne::Color;
use ariadne::Label;
use ariadne::ReportKind;
use ariadne::Source;

impl ariadne::Span for Span {
    type SourceId = SourceId;

    fn source(&self) -> &Self::SourceId {
        self.source.id()
    }

    fn start(&self) -> usize {
        self.start()
    }

    fn end(&self) -> usize {
        self.end()
    }
}

impl From<&ErrorKind> for ReportKind<'static> {
    fn from(value: &ErrorKind) -> Self {
        match value {
            ErrorKind::Silent
            | ErrorKind::Custom { .. }
            | ErrorKind::UnknownCharacter(_)
            | ErrorKind::UnterminatedGroup { .. }
            | ErrorKind::UnterminatedChar(_)
            | ErrorKind::LongChar(_)
            | ErrorKind::UnterminatedString(_)
            | ErrorKind::UnexpectedToken { .. }
            | ErrorKind::EndOfFile(_) => ReportKind::Error,
        }
    }
}

impl ariadne::Cache<SourceId> for &SourceCache {
    fn fetch(&mut self, id: &SourceId) -> Result<&Source, Box<dyn std::fmt::Debug + '_>> {
        self.sources
            .get(id)
            .ok_or_else(|| unreachable!())
            .map(|source| &source.ariadne_source)
    }

    fn display<'a>(&self, id: &'a SourceId) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(self.sources.get(id)?.file.name().into_owned()))
    }
}

/// A reported error, ready to be written to stderr.
///
/// This type exposes a very similar API to [`ariadne::Report`].
///
/// A `Vec<Report>` can be created from an [`Error`], but in most cases, the [`Error::eprint`]
/// method will suffice.
pub struct Report {
    report: ariadne::Report<'static, Span>,
}

impl Report {
    /// Writes this diagnostic to an implementor of [`Write`].
    ///
    /// For more details, see [`ariadne::Report::write`].
    ///
    /// ## Errors
    ///
    /// Forwards any errors from `W::write`.
    #[allow(clippy::missing_panics_doc)]
    pub fn write<W: Write>(&self, w: W) -> io::Result<()> {
        self.report.write(&*SOURCE_CACHE.read().unwrap(), w)
    }

    /// Writes this diagnostic to an implementor of [`Write`].
    ///
    /// For more details, see [`ariadne::Report::write_for_stdout`].
    ///
    /// ## Errors
    ///
    /// Forwards any errors from `W::write`.
    #[allow(clippy::missing_panics_doc)]
    pub fn write_for_stdout<W: Write>(&self, w: W) -> io::Result<()> {
        self.report
            .write_for_stdout(&*SOURCE_CACHE.read().unwrap(), w)
    }

    /// Prints this diagnostic to stderr.
    ///
    /// For more details, see [`ariadne::Report::eprint`].
    ///
    /// ## Errors
    ///
    /// Returns an error if writing to stderr fails.
    #[allow(clippy::missing_panics_doc)]
    pub fn eprint(&self) -> io::Result<()> {
        self.report.eprint(&*SOURCE_CACHE.read().unwrap())
    }

    /// Prints this diagnostic to stdout. In most cases, [`Report::eprint`] is  preferable to this.
    ///
    /// For more details, see [`ariadne::Report::print`].
    ///
    /// ## Errors
    ///
    /// Returns an error if writing to stdout fails.
    #[allow(clippy::missing_panics_doc)]
    pub fn print(&self) -> io::Result<()> {
        self.report.print(&*SOURCE_CACHE.read().unwrap())
    }
}

impl From<&SingleError> for Report {
    fn from(value: &SingleError) -> Self {
        let mut builder =
            ariadne::Report::build((&value.kind).into(), *value.source.id(), value.kind.start())
                .with_code(value.kind.code());
        match &value.kind {
            ErrorKind::Silent => unreachable!("attempted to print silent error"),
            ErrorKind::Custom { message, span, .. } => {
                builder.set_message(message);
                builder.add_label(Label::new(span.clone()).with_color(Color::Red));
            }
            ErrorKind::UnknownCharacter(span) => {
                builder.set_message("Unrecognised character");
                builder.add_label(Label::new(span.clone()).with_color(Color::Red));
            }
            ErrorKind::UnterminatedGroup { start, span } => {
                builder.set_message(format!("Unmatched '{start}'"));
                builder.add_label(Label::new(span.clone()).with_color(Color::Red));
            }
            ErrorKind::UnterminatedChar(span) => {
                builder.set_message("Expect \"'\" after character literal");
                builder.add_label(Label::new(span.clone()).with_color(Color::Red));
            }
            ErrorKind::LongChar(span) => {
                builder.set_message("Character literals must be exactly one character long");
                builder.add_label(Label::new(span.clone()).with_color(Color::Red));
            }
            ErrorKind::UnterminatedString(span) => {
                builder.set_message("Expect '\"' at end of string literal");
                builder.add_label(Label::new(span.clone()).with_color(Color::Red));
            }
            ErrorKind::UnexpectedToken { expected, span } => {
                builder.set_message("Unexpected token");
                builder.add_label(
                    Label::new(span.clone())
                        .with_color(Color::Red)
                        .with_message(unexpected_token_message(expected)),
                );
            }
            ErrorKind::EndOfFile(_) => builder.set_message("Unexpected end of file while parsing"),
        }

        Report {
            report: builder.finish(),
        }
    }
}

impl From<&Error> for Vec<Report> {
    fn from(value: &Error) -> Self {
        let mut reports = Vec::with_capacity(value.errors.len());
        for error in &value.errors {
            if !matches!(&error.kind, ErrorKind::Silent) {
                reports.push(Report::from(error));
            }
        }
        reports
    }
}

impl Error {
    /// Prints this error to stderr.
    ///
    /// ## Errors
    ///
    /// Returns an error if writing to stderr fails.
    pub fn eprint(&self) -> io::Result<()> {
        let reports: Vec<Report> = self.into();
        for report in reports {
            report.eprint()?;
        }
        Ok(())
    }

    /// Generates a series of reports from `self`.
    pub fn to_reports(&self) -> Vec<Report> {
        self.into()
    }
}
