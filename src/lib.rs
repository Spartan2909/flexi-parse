//! flexi-parse is a crate for parsing arbitrary syntax into a syntax tree. It
//! is intended to be more flexible than a parser generator or parser
//! combinator, while still being simple to use.

#![cfg_attr(all(doc, not(doctest)), feature(doc_auto_cfg))]

use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt;
use std::fs;
use std::io;
use std::path::Path;
use std::ptr;
use std::result;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::sync::Mutex;

pub mod error;
use error::Error;
use error::ErrorKind;

pub mod group;

mod lookahead;
pub use lookahead::Lookahead;
pub use lookahead::Peek;

pub mod punctuated;

mod scanner;

mod to_string;

pub mod token;
use token::Ident;
use token::SingleCharPunct;
use token::Token;
use token::WhiteSpace;

#[cfg(feature = "proc-macro2")]
mod proc_macro;

/// A struct representing a file of source code.
///
/// This type is the input to [`parse_source`].
#[derive(Clone, PartialEq, Eq)]
pub struct SourceFile {
    name: String,
    path: Option<String>,
    contents: String,
}

impl SourceFile {
    /// Reads the file at the given path into a `SourceFile`.
    ///
    /// ## Errors
    /// This function returns an error if the given path is not readable.
    pub fn read(path: &Path) -> io::Result<SourceFile> {
        let name = path
            .file_name()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "invalid filename"))?
            .to_string_lossy()
            .into_owned();
        let contents = fs::read_to_string(path)?;
        Ok(SourceFile {
            name,
            path: Some(path.to_string_lossy().into_owned()),
            contents,
        })
    }

    /// Creates a new `SourceFile` with the given name and contents.
    pub const fn new(name: String, contents: String) -> SourceFile {
        SourceFile {
            name,
            path: None,
            contents,
        }
    }

    fn id(&self) -> &String {
        self.path.as_ref().unwrap_or(&self.name)
    }
}

impl fmt::Debug for SourceFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SourceFile")
            .field("name", &self.name)
            .field("path", &self.path)
            .finish_non_exhaustive()
    }
}

/// A region of source code.
///
/// Note that unlike [`proc_macro::Span`], this struct contains a reference to
/// the file containing it.
///
/// [`proc_macro::Span`]: https://doc.rust-lang.org/stable/proc_macro/struct.Span.html
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    start: usize,
    end: usize,
    source: Arc<SourceFile>,
}

impl Span {
    const fn new(start: usize, end: usize, source: Arc<SourceFile>) -> Span {
        Span { start, end, source }
    }

    /// Creates a new `Span` from the start of `start` to the end of `end`.
    ///
    /// ## Panics
    /// This function will panic if `start` and `end` come from different source
    /// files.
    pub fn across(start: &Span, end: &Span) -> Span {
        assert_eq!(
            start.source, end.source,
            "both inputs to `across` must come from the same source file"
        );
        Span {
            start: start.start,
            end: end.end,
            source: Arc::clone(&start.source),
        }
    }

    #[doc(hidden)]
    pub const fn source(&self) -> &Arc<SourceFile> {
        &self.source
    }

    /// Returns the start line and start column.
    fn start_location(&self) -> (usize, usize) {
        let mut newlines = 0;
        let mut last_newline = 0;
        for (i, char) in self.source.contents[..self.start].chars().enumerate() {
            if char == '\n' {
                newlines += 1;
                last_newline = i;
            }
        }

        (newlines + 1, self.start - last_newline + 1)
    }

    /// Returns true if the span was created with `Span::new_empty()`.
    pub const fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

/// Parsing interface for types with a default parsing method.
pub trait Parse: Sized {
    /// Parses the input into this type.
    ///
    /// ## Errors
    /// This function returns an error if `source` doesn't contain a valid instance
    /// of `T`.
    fn parse(input: ParseStream) -> Result<Self>;
}

impl<T: Parse> Parse for Box<T> {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Box::new(T::parse(input)?))
    }
}

/// A parser that can parse a stream of tokens into a syntax tree node.
pub trait Parser: Sized {
    /// The return type of this parser.
    type Output;

    /// Parses a [`TokenStream`] into the relevant syntax tree node.
    ///
    /// ## Errors
    /// This function returns an error if `source` doesn't contain a valid
    /// instance of `T`.
    fn parse(self, tokens: TokenStream) -> Result<Self::Output>;
}

impl<F: FnOnce(ParseStream) -> Result<T>, T> Parser for F {
    type Output = T;

    fn parse(self, tokens: TokenStream) -> Result<Self::Output> {
        let cursor = Cursor {
            stream: Cow::Borrowed(tokens.tokens.as_slice()),
            offset: AtomicUsize::new(0),
            last: tokens.tokens.len() - 1,
        };
        self(&ParseBuffer::new(cursor, Arc::clone(&tokens.source)))
    }
}

/// Parses the given tokens into the syntax tree node `T`.
///
/// This function ignores all whitespace.
///
/// ## Errors
/// Forwards any error from `T::parse`.
pub fn parse<T: Parse>(mut tokens: TokenStream) -> Result<T> {
    tokens.remove_whitespace();
    Parser::parse(T::parse, tokens)
}

/// Scans and parses the given source file into the syntax tree node `T`.
///
/// This function ignores all whitespace.
///
/// ## Errors
/// Forwards any errors from `T::parse`.
pub fn parse_source<T: Parse>(source: Arc<SourceFile>) -> Result<T> {
    let (tokens, error) = scanner::scan(source, 0, None);
    match parse(tokens) {
        Ok(value) => error.map_or(Ok(value), Err),
        Err(err) => {
            if let Some(error) = error {
                Err(error.with(err))
            } else {
                Err(err)
            }
        }
    }
}

/// Scans and parses the given string into the syntax tree node `T`.
///
/// This function ignores all whitespace.
///
/// ## Errors
/// Forwards any error from `T::parse`.
pub fn parse_string<T: Parse>(source: String) -> Result<T> {
    let source = Arc::new(SourceFile {
        name: "str".to_string(),
        path: None,
        contents: source,
    });
    parse_source(source)
}

/// Attempts to repeatedly parse `input` into the given syntax tree node,
/// using `T`'s default parsing implementation, and continuing until `input` is
/// exhausted.
///
/// Note that this function doesn't perform any error recovery.
///
/// ## Errors
/// Forwards any errors from `T::parse`.
pub fn parse_repeated<T: Parse>(input: ParseStream) -> Result<Vec<T>> {
    let mut items = vec![];

    while !input.is_empty() {
        items.push(input.parse()?);
    }

    Ok(items)
}

/// Gets the `Ok` value, panicking with a formatted error message if the value
/// is `Err`.
///
/// ## Panics
/// Panics if the contained value is `Err`.
#[cfg(feature = "ariadne")]
pub fn pretty_unwrap<T>(result: Result<T>) -> T {
    result.unwrap_or_else(|err| {
        let mut buf = vec![];
        for report in err.to_reports() {
            report.write(&mut buf).unwrap();
        }
        String::from_utf8(buf).map_or_else(
            |_| {
                err.eprint().unwrap();
                panic!("failed due to above errors");
            },
            |s| panic!("{s}"),
        )
    })
}

/// A sequence of tokens.
///
/// This is the return type of
/// [`Group::token_stream`][group::Group::into_token_stream], and can be created
/// from a [`proc_macro::TokenStream`][proc-macro] or
/// [`proc_macro2::TokenStream`][proc-macro2].
///
/// [proc-macro]: https://doc.rust-lang.org/proc_macro/struct.TokenStream.html
/// [proc-macro2]: https://docs.rs/proc-macro2/latest/proc_macro2/struct.TokenStream.html
#[derive(Debug, Clone, PartialEq)]
pub struct TokenStream {
    tokens: Vec<Entry>,
    source: Arc<SourceFile>,
}

impl TokenStream {
    fn new(tokens: Vec<Entry>, source: Arc<SourceFile>) -> TokenStream {
        TokenStream { tokens, source }
    }

    fn filter<F: FnMut(&TokenStream) -> Vec<usize>>(&mut self, mut function: F) {
        let mut indices = function(self);
        indices.sort_unstable();
        indices.reverse();
        for index in indices {
            self.tokens.remove(index);
        }
    }

    /// Removes all whitespace that doesn't come at the start of a line.
    ///
    /// Note that the `parse*` functions remove all whitespace.
    pub fn prepare_whitespace(&mut self) {
        self.filter(|tokens| {
            let mut indices = vec![];
            let mut post_newline = true;
            for (index, entry) in tokens.tokens.iter().enumerate() {
                if let Entry::WhiteSpace(whitespace) = entry {
                    if matches!(whitespace, WhiteSpace::NewLine(_)) {
                        post_newline = true;
                    } else if !post_newline {
                        indices.push(index);
                    }
                } else {
                    post_newline = false;
                }
            }
            indices
        });
    }

    /// Removes all non-newline whitespace from `self`.
    ///
    /// Note that the `parse*` functions will remove all whitespace.
    pub fn remove_blank_space(&mut self) {
        self.filter(|tokens| {
            let mut indices = vec![];
            for (index, entry) in tokens.tokens.iter().enumerate() {
                if let Entry::WhiteSpace(whitespace) = entry {
                    if !matches!(whitespace, WhiteSpace::NewLine(_)) {
                        indices.push(index);
                    }
                }
            }
            indices
        });
    }

    /// Removes all whitespace tokens from this stream.
    ///
    /// This method is automatically called by the `parse*` functions.
    pub fn remove_whitespace(&mut self) {
        self.filter(|tokens| {
            let mut indices = vec![];
            for (index, entry) in tokens.tokens.iter().enumerate() {
                if let Entry::WhiteSpace(_) = entry {
                    indices.push(index);
                }
            }
            indices
        });
    }

    /// Returns true if there are no tokens in `self`.
    pub fn is_empty(&self) -> bool {
        self.tokens.len() == 1
    }
}

impl TryFrom<Arc<SourceFile>> for TokenStream {
    type Error = Error;

    fn try_from(value: Arc<SourceFile>) -> Result<Self> {
        let (tokens, error) = scanner::scan(value, 0, None);
        error.map_or(Ok(tokens), Err)
    }
}

/// Creates a new error in the given source file, at the given location, and
/// with the given message and code.
///
/// `location` will accept any type that is `Token`, `Delimiter`, or a `Span`.
pub fn new_error<L: Into<Span>>(message: String, location: L, code: u16) -> Error {
    let span = location.into();
    Error::new(
        Arc::clone(&span.source),
        ErrorKind::Custom {
            message,
            span,
            code,
        },
    )
}

/// A cursor position within a token stream.
pub struct ParseBuffer<'a> {
    cursor: Cursor<'a>,
    source: Arc<SourceFile>,
    error: Mutex<Error>,
}

impl<'a> ParseBuffer<'a> {
    fn new(cursor: Cursor<'a>, source: Arc<SourceFile>) -> ParseBuffer<'a> {
        ParseBuffer {
            cursor,
            source,
            error: Mutex::new(Error::empty()),
        }
    }

    /// Attempts to parse `self` into the given syntax tree node, using `T`'s
    /// default parsing implementation.
    ///
    /// ## Errors
    /// Returns an error if `T`'s `Parse` implementation fails.
    pub fn parse<T: Parse>(&self) -> Result<T> {
        T::parse(self)
    }

    /// Returns true if this stream has been exhausted.
    pub fn is_empty(&self) -> bool {
        self.cursor.eof()
    }

    /// Creates a new error at the given location with the given message and
    /// code.
    pub fn new_error<T: Into<Span>>(&self, message: String, location: T, code: u16) -> Error {
        Error::new(
            Arc::clone(&self.source),
            ErrorKind::Custom {
                message,
                span: location.into(),
                code,
            },
        )
    }

    /// Adds a new error to this buffer's storage.
    #[allow(clippy::missing_panics_doc)] // Will not panic.
    pub fn add_error(&self, error: Error) {
        self.error.lock().unwrap().add(error);
    }

    /// Returns an error consisting of all errors from
    /// [`ParseBuffer::add_error`], if it has been called.
    #[allow(clippy::missing_panics_doc)] // Will not panic.
    pub fn get_error(&self) -> Option<Error> {
        let error = self.error.lock().unwrap();
        if error.is_empty() {
            None
        } else {
            Some(error.to_owned())
        }
    }

    /// Repeatedly skips tokens until `function` returns true or `self` is
    /// empty.
    pub fn synchronise<F: FnMut(ParseStream<'_>) -> bool>(&self, mut function: F) {
        while !self.is_empty() && !function(self) {
            let _ = self.next();
        }
        let _ = self.next();
    }

    fn try_parse<T: Parse>(&self) -> Result<T> {
        let offset = self.cursor.offset.load(Ordering::SeqCst);
        T::parse(self).map_err(move |err| {
            self.cursor.offset.store(offset, Ordering::SeqCst);
            err
        })
    }

    /// Parses `T1` and `T2`, with no whitespace allowed between them.
    ///
    /// ## Errors
    /// Returns an error if `self` does not start with the required tokens.
    pub fn parse_joint<T1: Token, T2: Token>(&self) -> Result<(T1, T2)> {
        if self.current()?.span().end < self.next()?.span().start {
            return Err(Error::new(
                Arc::clone(&self.source),
                ErrorKind::UnexpectedToken {
                    expected: HashSet::from_iter([T1::display() + &T2::display()]),
                    span: self.current()?.span().to_owned(),
                },
            ));
        }
        let t1 = self.parse()?;
        let t2 = self.parse()?;
        Ok((t1, t2))
    }

    /// Attempts to parse `self` into `Vec<T>`, with no separating punctuation,
    /// fully consuming `self`.
    ///
    /// To parse separated instances of `T`, see
    /// [Punctuated][punctuated::Punctuated].
    ///
    /// ## Errors
    /// Returns an error if `self` is not a valid sequence of `T`.
    pub fn parse_repeated<T: Parse>(&self) -> Result<Vec<T>> {
        let mut items = vec![];

        while !self.is_empty() {
            items.push(self.parse()?);
        }

        Ok(items)
    }

    /// Returns true if the next token is an instance of `T`.
    #[allow(clippy::needless_pass_by_value)]
    pub fn peek<T: Peek>(&self, token: T) -> bool {
        let _ = token;
        self.parse_undo::<T::Token>().is_ok()
    }

    /// Returns true if the next token is an instance of `T`.
    ///
    /// Note that for the purposes of this function, multi-character punctuation
    /// like `+=` is considered to be two tokens, and float literals are
    /// considered to be three tokens (start, `.`, end).
    pub fn peek2<T: Peek>(&self, token: T) -> bool {
        let buffer = self.fork();
        let _ = buffer.next();
        buffer.peek::<T>(token)
    }

    fn parse_undo<T: Parse>(&self) -> Result<T> {
        let offset = self.cursor.offset.load(Ordering::SeqCst);
        let val = T::parse(self);
        self.cursor.offset.store(offset, Ordering::SeqCst);
        val
    }

    fn report_error_tokens(&self) -> Result<()> {
        let mut error = false;
        while let (Entry::Error(_), offset) = self.cursor.next() {
            self.cursor.offset.store(offset, Ordering::SeqCst);
            error = true;
        }
        if error {
            Err(Error::new(Arc::clone(&self.source), ErrorKind::Silent))
        } else {
            Ok(())
        }
    }

    fn next(&'a self) -> Result<&'a Entry> {
        self.report_error_tokens()?;
        if self.cursor.eof() {
            Err(Error::new(
                Arc::clone(&self.source),
                ErrorKind::EndOfFile(self.source.contents.len()),
            ))
        } else {
            let (token, offset) = self.cursor.next();
            self.cursor.offset.store(offset, Ordering::SeqCst);
            Ok(token)
        }
    }

    fn current(&'a self) -> Result<&'a Entry> {
        self.report_error_tokens()?;
        if self.cursor.eof() {
            Err(Error::new(
                Arc::clone(&self.source),
                ErrorKind::EndOfFile(self.source.contents.len()),
            ))
        } else {
            Ok(self.cursor.current())
        }
    }

    /// Gets the span of the current token.
    ///
    /// ## Errors
    /// Returns an error if `self` is empty.
    pub fn current_span(&self) -> Result<Span> {
        Ok(self.current()?.span().to_owned())
    }

    fn get_relative(&'a self, offset: isize) -> Result<&'a Entry> {
        self.cursor.get_relative(offset).ok_or(Error::new(
            Arc::clone(&self.source),
            ErrorKind::EndOfFile(self.source.contents.len()),
        ))
    }

    /// Creates a new `ParseBuffer` at the same position as `self`.
    ///
    /// Changes to `self` will not affect the fork, and vice versa.
    #[must_use]
    pub fn fork(&self) -> ParseBuffer<'a> {
        ParseBuffer::new(self.cursor.clone(), Arc::clone(&self.source))
    }

    /// Commits a forked buffer into `self`, updating `self` to reflect `fork`.
    ///
    /// ## Panics
    /// This function will panic if `fork` wasn't forked from `self` or if
    /// `self` is further ahead than `fork`.
    pub fn commit(&self, fork: &Self) {
        if !ptr::eq(self.cursor.stream.as_ptr(), fork.cursor.stream.as_ptr()) {
            panic!("cannot commit ParseBuffer that wasn't forked from this buffer");
        } else if fork.cursor.offset.load(Ordering::SeqCst)
            < self.cursor.offset.load(Ordering::SeqCst)
        {
            panic!("cannot commit original ParseBuffer into fork");
        }
        self.cursor
            .offset
            .store(fork.cursor.offset.load(Ordering::SeqCst), Ordering::SeqCst);
    }

    /// Creates an error with the message `Unexpected token` and the given
    /// expected tokens.
    ///
    /// Use of this function is generally discouraged in favour of
    /// [`Lookahead::error`].
    pub fn unexpected_token(&self, expected: HashSet<String>) -> Error {
        let current = match self.current() {
            Ok(current) => current,
            Err(err) => return err,
        };
        Error::new(
            Arc::clone(&self.source),
            ErrorKind::UnexpectedToken {
                expected,
                span: current.span().clone(),
            },
        )
    }

    /// Creates a helper struct for peeking at the next token.
    pub fn lookahead(&self) -> Lookahead<'a> {
        Lookahead::new(self.fork())
    }

    /// Skips over all whitespace tokens before the next non-whitespace token.
    ///
    /// This method will not skip newlines.
    pub fn skip_whitespace(&self) {
        while let (Entry::WhiteSpace(whitespace), offset) = self.cursor.next() {
            if matches!(whitespace, WhiteSpace::NewLine(_)) {
                break;
            }
            self.cursor.offset.store(offset, Ordering::SeqCst);
        }
    }

    /// Creates a new empty Span with this stream's source file.
    pub fn empty_span(&self) -> Span {
        Span {
            start: 0,
            end: 0,
            source: Arc::clone(&self.source),
        }
    }
}

impl fmt::Debug for ParseBuffer<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ParseBuffer")
            .field(
                "tokens",
                &&self.cursor.stream[self.cursor.offset.load(Ordering::SeqCst)..],
            )
            .field("source", &self.source)
            .field("error", &self.error)
            .finish()
    }
}

impl<'a> From<TokenStream> for ParseBuffer<'a> {
    fn from(value: TokenStream) -> Self {
        let last = value.tokens.len() - 1;
        let cursor = Cursor {
            stream: Cow::Owned(value.tokens),
            offset: AtomicUsize::new(0),
            last,
        };
        ParseBuffer::new(cursor, value.source)
    }
}

/// Returns true if [`ParseBuffer::peek`] would return true for any types
/// passed.
///
/// Accepts a `ParseStream` followed by one or more types.
#[macro_export]
macro_rules! peek_any {
    ( $input:expr, $( $ty:tt ),+ ) => {
        $( $input.peek($ty) || )+ false
    };
}

/// Returns true if [`ParseBuffer::peek2`] would return true for any types
/// passed.
///
/// Accepts a `ParseStream` followed by one or more types.
#[macro_export]
macro_rules! peek2_any {
    ( $input:expr, $( $ty:tt ),+ ) => {
        $( $input.peek2($ty) || )+ false
    };
}

/// The input type for all parsing functions.
pub type ParseStream<'a> = &'a ParseBuffer<'a>;

#[derive(Debug)]
struct Cursor<'a> {
    stream: Cow<'a, [Entry]>,
    offset: AtomicUsize,
    last: usize,
}

impl<'a> Cursor<'a> {
    fn bump(&self) -> usize {
        let offset = self.offset.load(Ordering::SeqCst);
        if offset == self.last {
            offset
        } else {
            offset + 1
        }
    }

    fn current(&'a self) -> &'a Entry {
        &self.stream[self.offset.load(Ordering::SeqCst)]
    }

    pub fn eof(&self) -> bool {
        self.offset.load(Ordering::SeqCst) == self.last
    }

    fn next(&'a self) -> (&'a Entry, usize) {
        let token_tree = self.current();
        let offset = self.bump();
        (token_tree, offset)
    }

    fn get_relative(&'a self, offset: isize) -> Option<&'a Entry> {
        let current_offset = self.offset.load(Ordering::SeqCst);
        let index = if offset < 0 {
            current_offset - offset.unsigned_abs()
        } else {
            current_offset + offset.unsigned_abs()
        };
        self.stream.get(index)
    }
}

impl<'a> Clone for Cursor<'a> {
    fn clone(&self) -> Self {
        Cursor {
            stream: self.stream.clone(),
            offset: AtomicUsize::new(self.offset.load(Ordering::SeqCst)),
            last: self.last,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Entry {
    Error(Span),
    Ident(Ident),
    Punct(SingleCharPunct),
    WhiteSpace(WhiteSpace),
    End,
}

impl Entry {
    fn span(&self) -> &Span {
        match self {
            Entry::Error(span) => span,
            Entry::Ident(ident) => &ident.span,
            Entry::Punct(punct) => &punct.span,
            Entry::WhiteSpace(whitespace) => whitespace.span(),
            Entry::End => unreachable!(),
        }
    }

    #[cfg(feature = "proc-macro2")]
    fn set_span(&mut self, span: Span) {
        match self {
            Entry::Error(current_span) => *current_span = span,
            Entry::Ident(ident) => ident.span = span,
            Entry::Punct(punct) => punct.span = span,
            Entry::WhiteSpace(whitespace) => whitespace.set_span(span),
            Entry::End => unreachable!(),
        }
    }
}

impl From<Ident> for Entry {
    fn from(value: Ident) -> Self {
        Self::Ident(value)
    }
}

impl From<SingleCharPunct> for Entry {
    fn from(value: SingleCharPunct) -> Self {
        Self::Punct(value)
    }
}

/// The return type of a parsing function.
pub type Result<T> = result::Result<T, Error>;

#[doc(hidden)]
pub mod private {
    pub trait Sealed {}

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub enum Marker {}
}

#[cfg(test)]
mod tests;
