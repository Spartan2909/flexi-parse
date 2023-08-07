use crate::error::Error;
use crate::error::SingleError;
use crate::error::ErrorKind;
use crate::SourceFile;
use crate::Span;

use std::io::{self, Write};

use ariadne::{ColorGenerator, Label, ReportKind, Source, Color};

impl ariadne::Span for Span<'_> {
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

impl From<&ErrorKind<'_>> for ReportKind<'_> {
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

pub struct Report<'src> {
    report: ariadne::Report<'static, Span<'src>>,
    source: &'src SourceFile,
}

impl Report<'_> {
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

impl<'src> From<&SingleError<'src>> for Report<'src> {
    fn from(value: &SingleError<'src>) -> Self {
        let mut builder =
            ariadne::Report::build((&value.kind).into(), value.source.id(), value.kind.start())
                .with_code(value.kind.discriminant());
        match &value.kind {
            ErrorKind::UnknownCharacter(span) => {
                builder.set_message("Unknown character");
                builder.add_label(Label::new(*span).with_color(Color::Red));
            }
            ErrorKind::UnterminatedChar(span) => {
                builder.set_message("Expect \"'\" after character literal");
                builder.add_label(Label::new(*span).with_color(Color::Red));
            }
            ErrorKind::UnexpectedToken { expected, span } => {}
            ErrorKind::EndOfFile(_) => builder.set_message("Unexpected end of file while parsing"),
            ErrorKind::UnterminatedString(span) => {
                builder.set_message("Expect '\"' at end of string literal");
                builder.add_label(Label::new(*span).with_color(Color::Red));
            }
            ErrorKind::UnopenedDelimiter(span) => {
                builder.set_message("Unmatched delimiter");
                builder.add_label(Label::new(*span).with_color(Color::Red));
            }
        }
        Report {
            report: builder.finish(),
            source: value.source,
        }
    }
}

impl<'src> From<&Error<'src>> for Vec<Report<'src>> {
    fn from(value: &Error<'src>) -> Self {
        value.errors.iter().map(|e| Report::from(e)).collect()
    }
}

impl Error<'_> {
    pub fn report(&self) -> io::Result<()> {
        let reports: Vec<Report> = self.into();
        for report in reports {
            report.eprint()?;
        }
        Ok(())
    }
}
