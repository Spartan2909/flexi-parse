//! flexi-parse is a crate for parsing arbitrary syntax into a syntax tree. It
//! is intended to be more flexible than a parser generator or parser
//! combinator, while still being simple to use.

#![cfg_attr(all(doc, not(doctest)), feature(doc_auto_cfg))]
#![warn(clippy::cast_lossless)]
#![warn(clippy::semicolon_if_nothing_returned)]
#![warn(clippy::semicolon_outside_block)]
#![warn(clippy::significant_drop_tightening)]
#![warn(missing_docs)]
#![forbid(clippy::dbg_macro)]
#![forbid(unsafe_op_in_unsafe_fn)]
#![forbid(clippy::multiple_unsafe_ops_per_block)]
#![forbid(clippy::todo)]
#![forbid(clippy::undocumented_unsafe_blocks)]

use std::cell::Cell;
use std::collections::HashSet;
use std::fs;
use std::io;
use std::marker::PhantomData;
use std::ops::Range;
use std::path::PathBuf;
use std::rc::Rc;
use std::result;

pub mod error;
pub mod group;
pub mod lookahead;
pub mod punctuated;
mod scanner;
mod to_string;
pub mod token;
use error::Error;
use error::ErrorKind;
use lookahead::Lookahead;
use token::Ident;
use token::SingleCharPunct;
use token::Token;
use token::WhiteSpace;

#[cfg(feature = "proc-macro2")]
mod proc_macro;

/// A struct representing a file of source code.
///
/// This type is the input to [`parse_source`].
#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    name: String,
    path: Option<String>,
    contents: String,
}

impl SourceFile {
    /// Reads the file at the given path into a `SourceFile`.
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

    /// Creates a new `SourceFile` with the given name and contents.
    pub fn new(name: String, contents: String) -> SourceFile {
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

/// A region of source code.
///
/// Note that unlike [`proc_macro::Span`], this struct contains a reference to
/// the file containing it.
///
/// [`proc_macro::Span`]: https://doc.rust-lang.org/stable/proc_macro/struct.Span.html
#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    start: usize,
    end: usize,
    source: Rc<SourceFile>,
}

impl Span {
    fn new(start: usize, end: usize, source: Rc<SourceFile>) -> Span {
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
            source: Rc::clone(&start.source),
        }
    }

    #[doc(hidden)]
    pub fn source(&self) -> &Rc<SourceFile> {
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
}

/// Parsing interface for types with a default parsing method.
pub trait Parse: Sized {
    /// Parses the input into this type.
    fn parse(input: ParseStream<'_>) -> Result<Self>;
}

/// A parser that can parse a stream of tokens into a syntax tree node.
pub trait Parser: Sized {
    /// The return type of this parser.
    type Output;

    /// Parses the a tokenstream into the relevant syntax tree node.
    fn parse(self, tokens: TokenStream) -> Result<Self::Output>;
}

impl<F: FnOnce(ParseStream<'_>) -> Result<T>, T> Parser for F {
    type Output = T;

    fn parse(self, tokens: TokenStream) -> Result<Self::Output> {
        let cursor = Cursor {
            original_stream: tokens.original_tokens.as_slice(),
            stream: tokens.tokens.as_slice(),
            offset: Cell::new(0),
            last: tokens.tokens.len() - 1,
            _marker: PhantomData,
        };
        self(&ParseBuffer::new(cursor, Rc::clone(&tokens.source)))
    }
}

/// Parses the given tokens into the syntax tree node `T`.
///
/// This function ignores all whitespace.
pub fn parse<T: Parse>(mut tokens: TokenStream) -> Result<T> {
    tokens.remove_whitespace();
    Parser::parse(T::parse, tokens)
}

/// Scans and parses the given source file into the syntax tree node `T`.
///
/// This function ignores all whitespace.
pub fn parse_source<T: Parse>(source: Rc<SourceFile>) -> Result<T> {
    let (tokens, error) = scanner::scan(source);
    parse(tokens).map_err(|mut err| {
        if let Some(error) = error {
            err.add(error);
        }
        err
    })
}

/// Scans and parses the given string into the syntax tree node `T`.
///
/// This function ignores all whitespace.
pub fn parse_string<T: Parse>(source: String) -> Result<T> {
    let source = Rc::new(SourceFile {
        name: "str".to_string(),
        path: None,
        contents: source,
    });
    parse_source(source)
}

/// Gets the `Ok` value, panicking with a formatted error message if the value
/// is `Err`.
/// ## Panics
/// Panics if the contained value is `Err`.
#[cfg(feature = "ariadne")]
pub fn pretty_unwrap<T>(result: Result<T>) -> T {
    result.unwrap_or_else(|err| {
        err.eprint().unwrap();
        panic!("failed to parse due to above errors");
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
    original_tokens: Vec<Entry>,
    tokens: Vec<(usize, Entry)>,
    source: Rc<SourceFile>,
}

impl TokenStream {
    fn new(tokens: Vec<Entry>, source: Rc<SourceFile>) -> TokenStream {
        TokenStream {
            original_tokens: tokens.clone(),
            tokens: tokens.into_iter().enumerate().collect(),
            source,
        }
    }

    fn filter<F: FnMut(&TokenStream) -> Vec<usize>>(&mut self, mut function: F) {
        let mut indices = function(self);
        indices.sort_unstable();
        indices.reverse();
        for index in indices {
            self.tokens.remove(index);
        }
    }

    /// Removes all whitespace tokens from this stream.
    ///
    /// This method is automatically called by the `parse*` functions.
    pub fn remove_whitespace(&mut self) {
        self.filter(|tokens| {
            let mut indices = vec![];
            for (index, (_, token)) in tokens.tokens.iter().enumerate() {
                if let Entry::WhiteSpace(_) = token {
                    indices.push(index);
                }
            }
            indices
        });
    }
}

/// A cursor position within a token stream.
///
/// This struct is unstable, and should only be used through the stable alias
/// [`ParseStream`].
pub struct ParseBuffer<'a> {
    cursor: Cursor<'a>,
    source: Rc<SourceFile>,
}

impl<'a> ParseBuffer<'a> {
    fn new(cursor: Cursor<'a>, source: Rc<SourceFile>) -> ParseBuffer<'a> {
        ParseBuffer { cursor, source }
    }

    /// Attempts to parse `self` into the given syntax tree node, using `T`'s
    /// default parsing implementation.
    pub fn parse<T: Parse>(&self) -> Result<T> {
        T::parse(self)
    }

    /// Attempts to parse `self` into the given syntax tree node, using
    /// `function`.
    pub fn parse_with<T, F: FnOnce(ParseStream<'_>) -> Result<T>>(&self, function: F) -> Result<T> {
        function(self)
    }

    /// Returns true if this stream has been exhausted.
    pub fn is_empty(&self) -> bool {
        self.cursor.eof()
    }

    fn try_parse<T: Parse>(&self) -> Result<T> {
        let offset = self.cursor.offset.get();
        T::parse(self).map_err(move |err| {
            self.cursor.offset.set(offset);
            err
        })
    }

    /// Returns true if the next token is an instance of `T`.
    pub fn peek<T: Token>(&self) -> bool {
        self.parse_undo::<T>().is_ok()
    }

    fn parse_undo<T: Parse>(&self) -> Result<T> {
        let offset = self.cursor.offset.get();
        let val = T::parse(self);
        self.cursor.offset.set(offset);
        val
    }

    fn report_error_tokens(&self) -> Result<()> {
        let mut error = false;
        while let (Entry::Error(_), offset) = self.cursor.next() {
            self.cursor.offset.set(offset);
            error = true;
        }
        if error {
            Err(Error::new(Rc::clone(&self.source), ErrorKind::Silent))
        } else {
            Ok(())
        }
    }

    fn next(&self) -> Result<&'a Entry> {
        self.report_error_tokens()?;
        if self.cursor.eof() {
            Err(Error::new(
                Rc::clone(&self.source),
                ErrorKind::EndOfFile(self.source.contents.len()),
            ))
        } else {
            let (token, offset) = self.cursor.next();
            self.cursor.offset.set(offset);
            Ok(token)
        }
    }

    fn current(&self) -> Result<&'a (usize, Entry)> {
        self.report_error_tokens()?;
        if self.cursor.eof() {
            Err(Error::new(
                Rc::clone(&self.source),
                ErrorKind::EndOfFile(self.source.contents.len()),
            ))
        } else {
            Ok(self.cursor.current())
        }
    }

    fn get_relative(&self, offset: isize) -> Result<&'a (usize, Entry)> {
        self.cursor.get_relative(offset).ok_or(Error::new(
            Rc::clone(&self.source),
            ErrorKind::EndOfFile(self.source.contents.len()),
        ))
    }

    fn get_absolute_range_original(&self, range: Range<usize>) -> Result<&'a [Entry]> {
        self.cursor
            .get_absolute_range_original(range)
            .ok_or(Error::new(
                Rc::clone(&self.source),
                ErrorKind::EndOfFile(self.source.contents.len()),
            ))
    }

    fn fork(&self) -> ParseBuffer<'a> {
        ParseBuffer::new(self.cursor.clone(), Rc::clone(&self.source))
    }

    /// Creates an error with the message `Unexpected token` and the given
    /// expected tokens.
    ///
    /// Use of this function is generally discouraged in favour of
    /// [`Lookahead::error`].
    pub fn error(&self, expected: HashSet<String>) -> Error {
        let current = match self.current() {
            Ok(current) => current,
            Err(err) => return err,
        };
        Error::new(
            Rc::clone(&self.source),
            ErrorKind::UnexpectedToken {
                expected,
                span: current.1.span().clone(),
            },
        )
    }

    /// Creates a helper struct for peeking at the next token.
    pub fn lookahead(&self) -> Lookahead<'a> {
        Lookahead::new(self.fork())
    }
}

/// The input type for all parsing functions. This is a stable alias for
/// [`ParseBuffer`].
pub type ParseStream<'a> = &'a ParseBuffer<'a>;

#[derive(Debug, Clone)]
struct Cursor<'a> {
    original_stream: *const [Entry],
    stream: *const [(usize, Entry)],
    offset: Cell<usize>,
    last: usize,
    _marker: PhantomData<&'a TokenStream>,
}

impl<'a> Cursor<'a> {
    fn bump(&self) -> usize {
        let offset = self.offset.get();
        if offset == self.last {
            offset
        } else {
            offset + 1
        }
    }

    fn stream(&self) -> &'a [(usize, Entry)] {
        // SAFETY: `stream` is live for 'a
        unsafe { &*self.stream }
    }

    fn current(&self) -> &'a (usize, Entry) {
        &self.stream()[self.offset.get()]
    }

    pub fn eof(&self) -> bool {
        self.offset.get() == self.last
    }

    fn next(&self) -> (&'a Entry, usize) {
        let (_, token_tree) = self.current();
        let offset = self.bump();
        (token_tree, offset)
    }

    fn get_relative(&self, offset: isize) -> Option<&'a (usize, Entry)> {
        self.stream()
            .get((self.offset.get() as isize + offset) as usize)
    }

    fn original_stream(&self) -> &'a [Entry] {
        // SAFETY: `original_stream` is live for 'a
        unsafe { &*self.original_stream }
    }

    fn get_absolute_range_original(&self, range: Range<usize>) -> Option<&'a [Entry]> {
        self.original_stream().get(range)
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

/// A macro to get the type of a punctuation token.
///
/// To avoid ambiguity, whitespace tokens are not available through this this
/// macro. Instead, use them directly, such as in `token::Space4`.
///
/// If the punctuation you want is not recognised by this macro, split it into
/// its constituent parts, e.g. `Punct!["£", "$"]` for `£$` or
/// `Punct!["++", "-"]` for `++-`.
///
/// Note that unlike [`syn::Token`], this macro accepts the token as a quoted
/// string. This allows tokens not recognised by the Rust scanner to be
/// accessed with this macro.
///
/// [`syn::Token`]: https://docs.rs/syn/latest/syn/macro.Token.html
#[macro_export]
macro_rules! Punct {
    ["&"] => { $crate::token::Ampersand };
    ["&&"] => { $crate::token::AmpersandAmpersand };
    ["*"] => { $crate::token::Asterisk };
    ["*="] => { $crate::token::AsteriskEqual };
    ["@"] => { $crate::token::At };
    ["\\"] => { $crate::token::Backslash };
    ["`"] => { $crate::token::BackTick };
    ["!"] => { $crate::token::Bang };
    ["!="] => { $crate::token::BangEqual };
    ["^"] => { $crate::token::Caret };
    [":"] => { $crate::token::Colon };
    ["::"] => { $crate::token::ColonColon };
    ["::"] => { $crate::token::ColonColonEqual };
    [","] => { $crate::token::Comma };
    ["-"] => { $crate::token::Dash };
    ["--"] => { $crate::token::DashDash };
    ["-="] => { $crate::token::DashEqual };
    ["$"] => { $crate::token::Dollar };
    ["."] => { $crate::token::Dot };
    ["\""] => { $crate::token::DoubleQuote };
    ["="] => { $crate::token::Equal };
    ["=>"] => { $crate::token::FatArrow };
    ["#"] => { $crate::token::Hash };
    ["##"] => { $crate::token::HashHash };
    ["###"] => { $crate::token::HashHashHash };
    ["<"] => { $crate::token::LAngle };
    ["<="] => { $crate::token::LAngleEqual };
    ["<<"] => { $crate::token::LAngleLAngle };
    ["<<="] => { $crate::token::LAngleLAngleEqual };
    ["<-"] => { $crate::token::LThinArrow };
    ["{"] => { $crate::token::LeftBrace };
    ["["] => { $crate::token::LeftBracket };
    ["("] => { $crate::token::LeftParen };
    ["\n"] => { $crate::token::NewLine };
    ["%"] => { $crate::token::Percent };
    ["%="] => { $crate::token::PercentEqual };
    ["|"] => { $crate::token::Pipe };
    ["||"] => { $crate::token::PipePipe };
    ["+"] => { $crate::token::Plus };
    ["+="] => { $crate::token::PlusEqual };
    ["++"] => { $crate::token::PlusPlus };
    ["£"] => { $crate::token::Pound };
    ["?"] => { $crate::token::Question };
    [">"] => { $crate::token::RAngle };
    [">="] => { $crate::token::RAngleEqual };
    [">>"] => { $crate::token::RAngleRAngle };
    [">>="] => { $crate::token::RAngleRAngleEqual };
    ["->"] => { $crate::token::RThinArrow };
    ["}"] => { $crate::token::RightBrace };
    ["]"] => { $crate::token::RightBracket };
    [")"] => { $crate::token::RightParen };
    [";"] => { $crate::token::SemiColon };
    ["'"] => { $crate::token::SingleQuote };
    ["/"] => { $crate::token::Slash };
    ["/="] => { $crate::token::SlashEqual };
    ["//"] => { $crate::token::SlashSlash };
    ["//="] => { $crate::token::SlashSlashEqual };
    ["  "] => { $crate::token::Space2 };
    ["    "] => { $crate::token::Space4 };
    ["\t"] => { $crate::token::Tab };
    ["~"] => { $crate::token::Tilde };
    ["¬"] => { $crate::token::Tilde2 };
    ["_"] => { $crate::token::UnderScore };
    [$l:tt, $( $r:tt ),+] => {
        ($crate::Punct![impl $l, $( $r ),+], $crate::Span)
    };
    [impl $l:tt] => { ($crate::Punct![$l],) };
    [impl $l:tt, $( $r:tt ),+] => {
        (($crate::Punct![$l],), $crate::Punct![impl $( $r ),+])
    };
}

#[doc(hidden)]
pub mod private {
    pub trait Sealed {}
}

#[cfg(test)]
mod tests;
