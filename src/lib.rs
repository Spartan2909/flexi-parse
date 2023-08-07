#![warn(clippy::cast_lossless)]
#![warn(clippy::semicolon_if_nothing_returned)]
#![warn(clippy::semicolon_outside_block)]
#![warn(clippy::significant_drop_tightening)]
#![warn(clippy::std_instead_of_core)]
#![warn(clippy::std_instead_of_core)]
#![forbid(clippy::dbg_macro)]
#![forbid(unsafe_op_in_unsafe_fn)]
#![forbid(clippy::multiple_unsafe_ops_per_block)]
#![forbid(clippy::todo)]
#![forbid(clippy::undocumented_unsafe_blocks)]

use std::collections::HashSet;
use std::fs;
use std::io;
use std::path::PathBuf;

pub mod error;
pub mod token;
mod scanner;
use error::Error;
use error::ErrorKind;
use token::Ident;
use token::Literal;
use token::SingleCharPunct;

#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    name: String,
    path: Option<String>,
    contents: String,
}

impl SourceFile {
    pub fn read(path: PathBuf) -> io::Result<SourceFile> {
        let name = path
            .file_name()
            .ok_or(io::Error::new(
                io::ErrorKind::InvalidInput,
                "invalid filename",
            ))?
            .to_string_lossy()
            .into_owned();
        let contents = fs::read_to_string(&path)?;
        Ok(SourceFile {
            name,
            path: Some(path.to_string_lossy().into_owned()),
            contents,
        })
    }

    pub fn new(name: String, contents: String) -> SourceFile {
        SourceFile { name, path: None, contents }
    }

    fn id(&self) -> &String {
        self.path.as_ref().unwrap_or(&self.name)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span<'src> {
    start: usize,
    end: usize,
    source: &'src SourceFile,
}

impl<'src> Span<'src> {
    fn new(start: usize, end: usize, source: &'src SourceFile) -> Span<'src> {
        Span { start, end, source }
    }
}

pub trait Parse<'src>: Sized {
    fn from_tokens(tokens: &mut TokenStream<'src>) -> Result<Self, Error<'src>>;
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenStream<'src> {
    tokens: Vec<TokenTree<'src>>,
    location: usize,
    source_file: &'src SourceFile,
}

impl<'src> TokenStream<'src> {
    pub fn new(source_file: &'src SourceFile) -> (TokenStream<'src>, Option<Error<'src>>) {
        source_file.into()
    }

    pub fn parse<T: Parse<'src>>(&mut self) -> Result<T, Error<'src>> {
        T::from_tokens(self)
    }

    fn try_parse<T: Parse<'src>>(&mut self) -> Result<T, Error<'src>> {
        let start = self.location;
        self.parse().map_err(|err| {
            self.location = start;
            err
        })
    }

    fn peek(&self, offset: isize) -> Result<&TokenTree<'src>, Error<'src>> {
        if let Some(token) = self.tokens.get((self.location as isize + offset) as usize) {
            Ok(token)
        } else {
            Err(Error::new(
                self.source_file,
                ErrorKind::EndOfFile(self.source_file.contents.len()),
            ))
        }
    }

    fn next(&mut self) -> Result<&TokenTree<'src>, Error<'src>> {
        if let Some(token) = self.tokens.get(self.location) {
            self.location += 1;
            Ok(token)
        } else {
            Err(Error::new(
                self.source_file,
                ErrorKind::EndOfFile(self.source_file.contents.len()),
            ))
        }
    }

    pub fn span(&self, start: Span<'_>, end: Span<'_>) -> Span<'src> {
        Span::new(start.start, end.end, self.source_file)
    }

    pub fn error<I: IntoIterator<Item = T>, T: Into<String>>(
        &self,
        expected: I,
        span: Span<'src>,
    ) -> Error<'src> {
        Error::new(
            self.source_file,
            ErrorKind::UnexpectedToken {
                expected: HashSet::from_iter(expected.into_iter().map(|t| t.into())),
                span,
            },
        )
    }
}

impl<'src> From<&'src SourceFile> for (TokenStream<'src>, Option<Error<'src>>) {
    fn from(value: &'src SourceFile) -> Self {
        scanner::scan(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum TokenTree<'src> {
    Error(Span<'src>),
    Ident(Ident<'src>),
    Punct(SingleCharPunct<'src>),
    Literal(Literal<'src>),
    End,
}

impl<'src> TokenTree<'src> {
    /*fn span(&self) -> Span<'src> {
        match self {
            TokenTree::Error(span) => *span,
            TokenTree::Ident(ident) => ident.span,
            TokenTree::Punct(punct) => punct.span,
            TokenTree::Literal(literal) => literal.span,
        }
    }*/
}

impl<'src> From<Ident<'src>> for TokenTree<'src> {
    fn from(value: Ident<'src>) -> Self {
        Self::Ident(value)
    }
}

impl<'src> From<SingleCharPunct<'src>> for TokenTree<'src> {
    fn from(value: SingleCharPunct<'src>) -> Self {
        Self::Punct(value)
    }
}

impl<'src> From<Literal<'src>> for TokenTree<'src> {
    fn from(value: Literal<'src>) -> Self {
        Self::Literal(value)
    }
}

mod private {
    pub trait Sealed {}
}
