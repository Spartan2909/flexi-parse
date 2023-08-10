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
            ErrorKind::Silent
            | ErrorKind::UnknownCharacter(_)
            | ErrorKind::UnterminatedChar(_)
            | ErrorKind::UnexpectedToken { .. }
            | ErrorKind::EndOfFile(_)
            | ErrorKind::UnterminatedString(_) => ReportKind::Error,
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
            ErrorKind::Silent => unreachable!(),
            ErrorKind::UnknownCharacter(span) => {
                builder.set_message("Unknown character");
                builder.add_label(Label::new(span.clone()).with_color(Color::Red));
            }
            ErrorKind::UnterminatedChar(span) => {
                builder.set_message("Expect \"'\" after character literal");
                builder.add_label(Label::new(span.clone()).with_color(Color::Red));
            }
            ErrorKind::UnexpectedToken { expected, span } => {
                builder.set_message("Unexpected token");
                let message = if expected.len() == 1 {
                    format!("Expected {}", expected.into_iter().next().unwrap())
                } else {
                    let mut message = "Expected one of ".to_string();
                    for (i, token) in expected.into_iter().enumerate() {
                        message.push_str(token);
                        if i + 1 < expected.len() {
                            message.push_str(", ");
                        }
                    }
                    message
                };
                builder.add_label(
                    Label::new(span.clone())
                        .with_color(Color::Red)
                        .with_message(message),
                );
            }
            ErrorKind::EndOfFile(_) => builder.set_message("Unexpected end of file while parsing"),
            ErrorKind::UnterminatedString(span) => {
                builder.set_message("Expect '\"' at end of string literal");
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
