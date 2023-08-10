use crate::error::Error;
use crate::error::ErrorKind;
use crate::error::SingleError;
use crate::SourceFile;
use crate::Span;

use std::io;
use std::io::Write;
use std::rc::Rc;

use ariadne::Color;
use ariadne::Label;
use ariadne::ReportKind;
use ariadne::Source;

impl ariadne::Span for Span {
    type SourceId = String;

    fn source(&self) -> &Self::SourceId {
        self.source.id()
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

impl From<&ErrorKind> for ReportKind<'static> {
    fn from(value: &ErrorKind) -> Self {
        match value {
            ErrorKind::UnknownCharacter(_)
            | ErrorKind::UnterminatedChar(_)
            | ErrorKind::UnexpectedToken { .. }
            | ErrorKind::EndOfFile(_)
            | ErrorKind::UnterminatedString(_)
            | ErrorKind::UnopenedDelimiter(_) => ReportKind::Error,
        }
    }
}

/// A wrapper for [`ariadne::Report`].
///
/// A `Vec<Report>` can be created from an [`Error`], but in many cases, the
/// [`Error::eprint`] method will suffice.
pub struct Report {
    report: ariadne::Report<'static, Span>,
    source: Rc<SourceFile>,
}

impl Report {
    pub fn write<W: Write>(&self, w: W) -> io::Result<()> {
        self.report.write(
            (
                self.source.id().to_owned(),
                Source::from(&self.source.contents),
            ),
            w,
        )
    }

    pub fn write_for_stdout<W: Write>(&self, w: W) -> io::Result<()> {
        self.report.write_for_stdout(
            (
                self.source.id().to_owned(),
                Source::from(&self.source.contents),
            ),
            w,
        )
    }

    pub fn eprint(&self) -> io::Result<()> {
        self.report.eprint((
            self.source.id().to_owned(),
            Source::from(&self.source.contents),
        ))
    }

    pub fn print(&self) -> io::Result<()> {
        self.report.print((
            self.source.id().to_owned(),
            Source::from(&self.source.contents),
        ))
    }
}

impl From<&SingleError> for Report {
    fn from(value: &SingleError) -> Self {
        let mut builder =
            ariadne::Report::build((&value.kind).into(), value.source.id(), value.kind.start())
                .with_code(value.kind.discriminant());
        match &value.kind {
            ErrorKind::UnknownCharacter(span) => {
                builder.set_message("Unknown character");
                builder.add_label(Label::new(span.clone()).with_color(Color::Red));
            }
            ErrorKind::UnterminatedChar(span) => {
                builder.set_message("Expect \"'\" after character literal");
                builder.add_label(Label::new(span.clone()).with_color(Color::Red));
            }
            ErrorKind::UnexpectedToken { expected, span } => {}
            ErrorKind::EndOfFile(_) => builder.set_message("Unexpected end of file while parsing"),
            ErrorKind::UnterminatedString(span) => {
                builder.set_message("Expect '\"' at end of string literal");
                builder.add_label(Label::new(span.clone()).with_color(Color::Red));
            }
            ErrorKind::UnopenedDelimiter(span) => {
                builder.set_message("Unmatched delimiter");
                builder.add_label(Label::new(span.clone()).with_color(Color::Red));
            }
        }
        Report {
            report: builder.finish(),
            source: Rc::clone(&value.source),
        }
    }
}

impl From<&Error> for Vec<Report> {
    fn from(value: &Error) -> Self {
        value.errors.iter().map(Report::from).collect()
    }
}

impl Error {
    pub fn eprint(&self) -> io::Result<()> {
        let reports: Vec<Report> = self.into();
        for report in reports {
            report.eprint()?;
        }
        Ok(())
    }

    pub fn report(&self) -> Vec<Report> {
        self.into()
    }
}
