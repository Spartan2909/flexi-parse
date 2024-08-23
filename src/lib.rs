//! flexi-parse is a crate for parsing arbitrary syntax into a syntax tree. It is intended to be
//! more flexible than a parser generator or parser combinator, while still being simple to use.

#![cfg_attr(all(doc, not(doctest)), feature(doc_auto_cfg))]

#[cfg(target_pointer_width = "16")]
compile_error!("targets with 16-bit pointers are not supported");

use std::borrow::Cow;
use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::fs;
use std::io;
use std::iter::Extend;
use std::path::PathBuf;
use std::ptr;
use std::result;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::sync::LazyLock;
use std::sync::RwLock;

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

pub mod to_tokens;

pub mod token;
use to_tokens::ToTokens;
use token::Ident;
use token::LitStrDoubleQuote;
use token::LitStrSingleQuote;
use token::SingleCharPunct;
use token::Token;
use token::WhiteSpace;

#[cfg(feature = "proc-macro2")]
mod proc_macro;
#[cfg(feature = "proc-macro2")]
pub use proc_macro::ToTokensWrapper;

static DEFAULT_SOURCE_FILE: LazyLock<Arc<SourceFile>> =
    LazyLock::new(|| SOURCE_CACHE.write().unwrap().insert(SourceFileInner::Dummy));

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum SourceFileInner {
    Named { name: String, contents: String },
    Path { path: PathBuf, contents: String },
    Dummy,
}

impl SourceFileInner {
    fn contents(&self) -> &str {
        match self {
            SourceFileInner::Named { name: _, contents }
            | SourceFileInner::Path { path: _, contents } => contents,
            SourceFileInner::Dummy => "",
        }
    }
}

#[doc(hidden)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceId(u32);

impl SourceId {
    fn new() -> SourceId {
        static NEXT: AtomicU32 = AtomicU32::new(0);

        let id = NEXT.fetch_add(1, Ordering::Relaxed);
        SourceId(id)
    }
}

struct Source {
    file: Arc<SourceFile>,
    #[cfg(feature = "ariadne")]
    ariadne_source: ariadne::Source,
}

impl Source {
    fn new(source: Arc<SourceFile>) -> Source {
        Source {
            #[cfg(feature = "ariadne")]
            ariadne_source: source.contents().into(),
            file: source,
        }
    }
}

struct SourceCache {
    #[cfg(feature = "ariadne")]
    sources: HashMap<SourceId, Source>,
}

impl SourceCache {
    fn new() -> SourceCache {
        SourceCache {
            #[cfg(feature = "ariadne")]
            sources: HashMap::new(),
        }
    }

    fn insert(&mut self, source: SourceFileInner) -> Arc<SourceFile> {
        let id = SourceId::new();
        let source_file = Arc::new(SourceFile { inner: source, id });
        let source = Source::new(Arc::clone(&source_file));
        self.sources.insert(id, source);
        source_file
    }
}

static SOURCE_CACHE: LazyLock<RwLock<SourceCache>> =
    LazyLock::new(|| RwLock::new(SourceCache::new()));

/// A struct representing a file of source code.
///
/// This type is the input to [`parse_source`].
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceFile {
    inner: SourceFileInner,
    id: SourceId,
}

impl SourceFile {
    /// Reads the file at the given path into a `SourceFile`.
    ///
    /// ## Errors
    ///
    /// This function returns an error if the given path is not readable.
    pub fn read(path: PathBuf) -> io::Result<SourceFile> {
        let contents = fs::read_to_string(&path)?;
        Ok(SourceFile {
            inner: SourceFileInner::Path { path, contents },
            id: SourceId::new(),
        })
    }

    /// Creates a new `SourceFile` with the given name and contents.
    pub fn new(name: String, contents: String) -> SourceFile {
        SourceFile {
            inner: SourceFileInner::Named { name, contents },
            id: SourceId::new(),
        }
    }

    const fn id(&self) -> &SourceId {
        &self.id
    }

    fn contents(&self) -> &str {
        self.inner.contents()
    }

    fn merge<'a>(
        self: &'a Arc<SourceFile>,
        other: &'a Arc<SourceFile>,
    ) -> Option<&'a Arc<SourceFile>> {
        if self.inner == SourceFileInner::Dummy {
            Some(other)
        } else if self.inner == other.inner {
            Some(self)
        } else {
            None
        }
    }

    fn name(&self) -> Cow<str> {
        match &self.inner {
            SourceFileInner::Named { name, contents: _ } => Cow::Borrowed(name),
            SourceFileInner::Path { path, contents: _ } => path.to_string_lossy(),
            SourceFileInner::Dummy => Cow::Borrowed("<none>"),
        }
    }
}

impl fmt::Debug for SourceFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner {
            SourceFileInner::Named {
                ref name,
                contents: _,
            } => f
                .debug_struct("SourceFile")
                .field("name", name)
                .finish_non_exhaustive(),
            SourceFileInner::Path {
                ref path,
                contents: _,
            } => f
                .debug_struct("SourceFile")
                .field("path", path)
                .finish_non_exhaustive(),
            SourceFileInner::Dummy => f.debug_struct("SourceFile").finish_non_exhaustive(),
        }
    }
}

/// A region of source code.
///
/// Note that unlike [`proc_macro::Span`], this struct contains a reference containing file.
///
/// [`proc_macro::Span`]: https://doc.rust-lang.org/stable/proc_macro/struct.Span.html
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    start: u32,
    end: u32,
    source: Arc<SourceFile>,
}

impl Span {
    const fn new(start: u32, end: u32, source: Arc<SourceFile>) -> Span {
        Span { start, end, source }
    }

    fn try_new(start: usize, end: usize, source: Arc<SourceFile>) -> Option<Span> {
        Some(Span {
            start: start.try_into().ok()?,
            end: end.try_into().ok()?,
            source,
        })
    }

    /// Create a new [`Span`] covering no tokens.
    ///
    /// This span has a special 'sentinel' source file which is automatically overridden when
    /// joining with [`Span::across`].
    pub fn empty() -> Span {
        Span {
            start: 0,
            end: 0,
            source: Arc::clone(&DEFAULT_SOURCE_FILE),
        }
    }

    const fn start(&self) -> usize {
        self.start as usize
    }

    const fn end(&self) -> usize {
        self.end as usize
    }

    /// Create a new [`Span`] from the start of `start` to the end of `end`.
    ///
    /// [`None`] is returned if `start` and `end` come from different sources and neither of them
    /// was obtained from [`Span::empty`].
    pub fn across(start: &Span, end: &Span) -> Option<Span> {
        let source = Arc::clone(start.source.merge(&end.source)?);
        Some(Span {
            start: start.start,
            end: end.end,
            source,
        })
    }

    /// Returns the start line and start column.
    fn start_location(&self) -> (usize, usize) {
        let (newlines, last_newline) = self.source.contents()[..self.start()]
            .chars()
            .enumerate()
            .fold((0, 0), |(newlines, last_newline), (index, ch)| {
                if ch == '\n' {
                    (newlines + 1, index)
                } else {
                    (newlines, last_newline)
                }
            });

        (newlines + 1, self.start() - last_newline + 1)
    }

    /// Returns true if `self` covers no tokens.
    pub const fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

/// Parsing interface for types with a default parsing method.
pub trait Parse: Sized {
    /// Parses the input into this type.
    ///
    /// ## Errors
    ///
    /// This function returns an error if `source` doesn't contain a valid instance of `T`.
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
    ///
    /// This function returns an error if `source` doesn't contain a valid instance of `T`.
    fn parse(self, tokens: TokenStream) -> Result<Self::Output>;
}

impl<F: FnOnce(ParseStream) -> Result<T>, T> Parser for F {
    type Output = T;

    fn parse(self, tokens: TokenStream) -> Result<Self::Output> {
        let cursor = Cursor {
            stream: Cow::Borrowed(tokens.tokens.as_slice()),
            offset: Cell::new(0),
            len: tokens.tokens.len(),
        };
        self(&ParseBuffer::new(
            cursor,
            Arc::clone(tokens.source.as_ref().unwrap()),
        ))
    }
}

/// Parses the given tokens into the syntax tree node `T`.
///
/// This function ignores all whitespace.
///
/// ## Errors
///
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
///
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
///
/// Forwards any errors from `T::parse`.
pub fn parse_string<T: Parse>(source: String) -> Result<T> {
    let source = Arc::new(SourceFile {
        inner: SourceFileInner::Named {
            name: "<string input>".to_string(),
            contents: source,
        },
        id: SourceId::new(),
    });
    parse_source(source)
}

/// Attempts to repeatedly parse `input` into the given syntax tree node, using `T`'s default
/// parsing implementation, and continuing until `input` is exhausted.
///
/// Note that this function doesn't perform any error recovery.
///
/// ## Errors
///
/// Forwards any errors from `T::parse`.
pub fn parse_repeated<T: Parse>(input: ParseStream) -> Result<Vec<T>> {
    let mut items = vec![];

    while !input.is_empty() {
        items.push(input.parse()?);
    }

    Ok(items)
}

/// Gets the `Ok` value, panicking with a formatted error message if the value is `Err`.
///
/// ## Panics
///
/// Panics if the contained value is `Err`.
#[cfg(feature = "ariadne")]
pub fn pretty_unwrap<T>(result: Result<T>) -> T {
    result.unwrap_or_else(|err| {
        err.details().map_or_else(
            || {
                err.eprint().unwrap();
                panic!("failed due to above errors");
            },
            |details| panic!("{details}"),
        )
    })
}

/// A sequence of tokens.
///
/// This is the return type of
/// [`Group::token_stream`][group::Group::into_token_stream], and can be created from a
/// [`proc_macro::TokenStream`][proc-macro] or [`proc_macro2::TokenStream`][proc-macro2].
///
/// [proc-macro]: https://doc.rust-lang.org/proc_macro/struct.TokenStream.html
/// [proc-macro2]: https://docs.rs/proc-macro2/latest/proc_macro2/struct.TokenStream.html
#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub struct TokenStream {
    tokens: Vec<Entry>,
    source: Option<Arc<SourceFile>>,
}

impl TokenStream {
    fn new(tokens: Vec<Entry>, source: Option<Arc<SourceFile>>) -> TokenStream {
        TokenStream { tokens, source }
    }

    fn filter<F: FnMut(&Entry) -> bool>(&mut self, function: F) {
        self.tokens.retain(function);
    }

    /// Removes all whitespace that doesn't come at the start of a line.
    ///
    /// Note that the `parse*` functions remove all whitespace.
    pub fn prepare_whitespace(&mut self) {
        let mut post_newline = true;
        self.filter(|entry| {
            match entry {
                Entry::WhiteSpace(WhiteSpace::NewLine(_)) => post_newline = true,
                Entry::WhiteSpace(_) => return post_newline,
                _ => post_newline = false,
            }
            true
        });
    }

    /// Removes all non-newline whitespace from `self`.
    ///
    /// Note that the `parse*` functions will remove all whitespace.
    pub fn remove_blank_space(&mut self) {
        self.filter(|entry| match entry {
            Entry::WhiteSpace(WhiteSpace::NewLine(_)) => true,
            Entry::WhiteSpace(_) => false,
            _ => true,
        });
    }

    /// Removes all whitespace tokens from this stream.
    ///
    /// This method is automatically called by the `parse*` functions.
    pub fn remove_whitespace(&mut self) {
        self.filter(|entry| !matches!(entry, Entry::WhiteSpace(_)));
    }

    /// Returns true if there are no tokens in `self`.
    pub fn is_empty(&self) -> bool {
        self.tokens.len() == 0
    }

    fn push(&mut self, entry: Entry) {
        if self.source.is_none() {
            self.source = Some(Arc::clone(&entry.span().source));
        }
        self.tokens.push(entry);
    }

    /// Add another [`TokenStream`] to the end of `self`.
    pub fn append(&mut self, other: &mut TokenStream) {
        self.tokens.append(&mut other.tokens);
    }
}

impl TryFrom<Arc<SourceFile>> for TokenStream {
    type Error = Error;

    fn try_from(value: Arc<SourceFile>) -> Result<Self> {
        let (tokens, error) = scanner::scan(value, 0, None);
        error.map_or(Ok(tokens), Err)
    }
}

impl<A: ToTokens> Extend<A> for TokenStream {
    fn extend<T: IntoIterator<Item = A>>(&mut self, iter: T) {
        for item in iter {
            self.tokens.append(&mut item.into_token_stream().tokens);
        }
    }
}

/// Creates a new error in the given source file, at the given location, and with the given message
/// and code.
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
    error: RefCell<Error>,
}

impl<'a> ParseBuffer<'a> {
    fn new(cursor: Cursor<'a>, source: Arc<SourceFile>) -> ParseBuffer<'a> {
        ParseBuffer {
            cursor,
            source,
            error: RefCell::new(Error::empty()),
        }
    }

    /// Attempts to parse `self` into the given syntax tree node, using `T`'s default parsing
    /// implementation.
    ///
    /// ## Errors
    ///
    /// Returns an error if `T`'s `Parse` implementation fails.
    pub fn parse<T: Parse>(&self) -> Result<T> {
        T::parse(self)
    }

    /// Returns true if this stream has been exhausted.
    pub fn is_empty(&self) -> bool {
        self.cursor.eof()
    }

    /// Creates a new error at the given location with the given message and code.
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
        self.error.borrow_mut().add(error);
    }

    /// Returns an error consisting of all errors from [`ParseBuffer::add_error`], if it has been called.
    #[allow(clippy::missing_panics_doc)] // Will not panic.
    pub fn get_error(&self) -> Option<Error> {
        let error = self.error.borrow();
        if error.is_empty() {
            None
        } else {
            Some(error.to_owned())
        }
    }

    /// Repeatedly skips tokens until `function` returns true or `self` is empty.
    pub fn synchronise<F: FnMut(ParseStream<'_>) -> bool>(&self, mut function: F) {
        while !self.is_empty() && !function(self) {
            let _ = self.next();
        }
        let _ = self.next();
    }

    fn try_parse<T: Parse>(&self) -> Result<T> {
        let offset = self.cursor.offset.get();
        T::parse(self).map_err(move |err| {
            self.cursor.offset.set(offset);
            err
        })
    }

    /// Parses `T1` and `T2`, with no whitespace allowed between them.
    ///
    /// ## Errors
    ///
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

    /// Attempts to parse `self` into `Vec<T>`, with no separating punctuation, fully consuming
    /// `self`.
    ///
    /// To parse separated instances of `T`, see [Punctuated][punctuated::Punctuated].
    ///
    /// ## Errors
    ///
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
    /// Note that for the purposes of this function, multi-character punctuation like `+=` is
    /// considered to be two tokens, and float literals are considered to be three tokens
    /// (start, `.`, end).
    pub fn peek2<T: Peek>(&self, token: T) -> bool {
        let buffer = self.fork();
        let _ = buffer.next();
        buffer.peek::<T>(token)
    }

    fn parse_undo<T: Parse>(&self) -> Result<T> {
        let offset = self.cursor.offset.get();
        let val = T::parse(self);
        self.cursor.offset.set(offset);
        val
    }

    fn report_error_tokens(&self) -> Result<()> {
        if self.cursor.eof() {
            return Ok(());
        }
        let mut error = false;
        while let Entry::Error(_) = self.cursor.current() {
            error = true;
            self.cursor.next();
        }
        if error {
            Err(Error::new(Arc::clone(&self.source), ErrorKind::Silent))
        } else {
            Ok(())
        }
    }

    fn next(&'a self) -> Result<&'a Entry> {
        self.report_error_tokens()?;
        self.next_raw().map_or_else(
            || {
                Err(Error::new(
                    Arc::clone(&self.source),
                    ErrorKind::EndOfFile(self.source.contents().len()),
                ))
            },
            Ok,
        )
    }

    fn next_raw(&'a self) -> Option<&'a Entry> {
        self.cursor.next()
    }

    fn current(&'a self) -> Result<&'a Entry> {
        self.report_error_tokens()?;
        if self.cursor.eof() {
            Err(Error::new(
                Arc::clone(&self.source),
                ErrorKind::EndOfFile(self.source.contents().len()),
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
            ErrorKind::EndOfFile(self.source.contents().len()),
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
    ///
    /// This function will panic if `fork` wasn't forked from `self` or if `self` is further ahead
    /// than `fork`.
    pub fn commit(&self, fork: &Self) {
        if !ptr::eq(self.cursor.stream.as_ptr(), fork.cursor.stream.as_ptr()) {
            panic!("cannot commit ParseBuffer that wasn't forked from this buffer");
        } else if fork.cursor.offset.get() < self.cursor.offset.get() {
            panic!("cannot commit original ParseBuffer into fork");
        }
        self.cursor.offset.set(fork.cursor.offset.get());
    }

    /// Creates an error with the message `Unexpected token` and the given expected tokens.
    ///
    /// Use of this function is generally discouraged in favour of [`Lookahead::error`].
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
        while let Entry::WhiteSpace(whitespace) = self.cursor.current() {
            if matches!(whitespace, WhiteSpace::NewLine(_)) {
                break;
            }
            if self.next_raw().is_none() {
                break;
            }
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
            .field("tokens", &&self.cursor.stream[self.cursor.offset.get()..])
            .field("source", &self.source)
            .field("error", &self.error)
            .finish()
    }
}

impl<'a> From<TokenStream> for ParseBuffer<'a> {
    fn from(value: TokenStream) -> Self {
        let len = value.tokens.len();
        let cursor = Cursor {
            stream: Cow::Owned(value.tokens),
            offset: Cell::new(0),
            len,
        };
        ParseBuffer::new(
            cursor,
            value
                .source
                .unwrap_or_else(|| Arc::clone(&DEFAULT_SOURCE_FILE)),
        )
    }
}

impl<'a> From<&'a TokenStream> for ParseBuffer<'a> {
    fn from(value: &'a TokenStream) -> Self {
        let cursor = Cursor {
            stream: Cow::Borrowed(&value.tokens),
            offset: Cell::new(0),
            len: value.tokens.len(),
        };
        let source = Arc::clone(value.source.as_ref().unwrap_or(&DEFAULT_SOURCE_FILE));
        ParseBuffer::new(cursor, source)
    }
}

/// Returns true if [`ParseBuffer::peek`] would return true for any types passed.
///
/// Accepts a `ParseStream` followed by one or more types.
#[macro_export]
macro_rules! peek_any {
    ( $input:expr, $( $ty:tt ),+ $(,)? ) => {
        $( $input.peek($ty) || )+ false
    };
}

/// Returns true if [`ParseBuffer::peek2`] would return true for any types passed.
///
/// Accepts a `ParseStream` followed by one or more types.
#[macro_export]
macro_rules! peek2_any {
    ( $input:expr, $( $ty:tt ),+ $(,)? ) => {
        $( $input.peek2($ty) || )+ false
    };
}

/// The input type for all parsing functions.
pub type ParseStream<'a> = &'a ParseBuffer<'a>;

#[derive(Debug)]
struct Cursor<'a> {
    stream: Cow<'a, [Entry]>,
    offset: Cell<usize>,
    len: usize,
}

impl<'a> Cursor<'a> {
    fn bump(&self) -> Option<usize> {
        let offset = self.offset.get();
        if self.eof() {
            None
        } else {
            Some(offset + 1)
        }
    }

    fn current(&'a self) -> &'a Entry {
        &self.stream[self.offset.get()]
    }

    pub fn eof(&self) -> bool {
        self.offset.get() == self.len
    }

    fn next(&'a self) -> Option<&'a Entry> {
        self.bump().map(|next| {
            let token = self.current();
            self.offset.set(next);
            token
        })
    }

    fn get_relative(&'a self, offset: isize) -> Option<&'a Entry> {
        let current_offset = self.offset.get();
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
            offset: Cell::new(self.offset.get()),
            len: self.len,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
enum Entry {
    Error(Span),
    Ident(Ident),
    Punct(SingleCharPunct),
    WhiteSpace(WhiteSpace),
    #[cfg(feature = "scan-strings")]
    LitStrDoubleQuote(LitStrDoubleQuote),
    #[cfg(feature = "scan-strings")]
    LitStrSingleQuote(LitStrSingleQuote),
}

impl Entry {
    fn span(&self) -> &Span {
        match self {
            Entry::Error(span) => span,
            Entry::Ident(ident) => &ident.span,
            Entry::Punct(punct) => &punct.span,
            Entry::WhiteSpace(whitespace) => whitespace.span(),
            #[cfg(feature = "scan-strings")]
            Entry::LitStrDoubleQuote(str) => str.span(),
            #[cfg(feature = "scan-strings")]
            Entry::LitStrSingleQuote(str) => str.span(),
        }
    }

    #[cfg(feature = "proc-macro2")]
    fn set_span(&mut self, span: Span) {
        match self {
            Entry::Error(current_span) => *current_span = span,
            Entry::Ident(ident) => ident.span = span,
            Entry::Punct(punct) => punct.span = span,
            Entry::WhiteSpace(whitespace) => whitespace.set_span(span),
            #[cfg(feature = "scan-strings")]
            Entry::LitStrDoubleQuote(str) => str.set_span(span),
            #[cfg(feature = "scan-strings")]
            Entry::LitStrSingleQuote(str) => str.set_span(span),
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

/// Attempts to parse a [`TokenStream`] into an implementor of [`Parse`], producing a compiler error
/// if it fails.
///
/// Must be used in a function that returns a type that implements
/// [`Into<proc_macro::TokenStream>`][tokenstream].
///
/// Note that this macro may not interact well with functions with a generic return type.
///
/// [tokenstream]: https://doc.rust-lang.org/stable/proc_macro/struct.TokenStream.html
#[cfg(feature = "proc-macro")]
#[macro_export]
macro_rules! parse_macro_input {
    ($expr:expr) => {
        match $crate::parse($expr) {
            ::core::result::Result::Ok(v) => v,
            ::core::result::Result::Err(e) => {
                return $crate::error::Error::to_compile_error(&e).into()
            }
        }
    };
}

#[doc(hidden)]
pub mod private {
    pub trait Sealed {}

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub enum Marker {}
}

#[cfg(test)]
mod tests;
