#![warn(clippy::cast_lossless)]
#![warn(clippy::semicolon_if_nothing_returned)]
#![warn(clippy::semicolon_outside_block)]
#![warn(clippy::significant_drop_tightening)]
#![forbid(clippy::dbg_macro)]
#![forbid(unsafe_op_in_unsafe_fn)]
#![forbid(clippy::multiple_unsafe_ops_per_block)]
#![forbid(clippy::todo)]
#![forbid(clippy::undocumented_unsafe_blocks)]

use std::cell::Cell;
use std::fs;
use std::io;
use std::marker::PhantomData;
use std::ops::Range;
use std::path::PathBuf;
use std::rc::Rc;
use std::result;

pub mod error;
pub mod lookahead;
pub mod punctuated;
mod scanner;
mod to_string;
pub mod token;
use error::Error;
use error::ErrorKind;
use lookahead::Lookahead;
use token::Ident;
use token::Literal;
use token::SingleCharPunct;
use token::Token;
use token::WhiteSpace;

#[cfg(feature = "proc-macro2")]
mod proc_macro;

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

    fn dummy() -> Span {
        const DUMMY_FILE: SourceFile = SourceFile {
            name: String::new(),
            path: None,
            contents: String::new(),
        };
        Span {
            start: 0,
            end: 0,
            source: Rc::new(DUMMY_FILE),
        }
    }

    #[doc(hidden)]
    pub fn source(&self) -> &Rc<SourceFile> {
        &self.source
    }
}

/// Parsing interface for types with a default parsing method.
pub trait Parse: Sized {
    fn parse(input: ParseStream<'_>) -> Result<Self>;
}

/// A parser that can parse a stream of tokens into a syntax tree node.
pub trait Parser: Sized {
    type Output;

    /// Parses the a tokenstream into the relevant syntax tree node.
    fn parse(self, tokens: TokenStream) -> Result<Self::Output>;
}

impl<F: FnOnce(ParseStream<'_>) -> Result<T>, T> Parser for F {
    type Output = T;

    fn parse(self, tokens: TokenStream) -> Result<Self::Output> {
        let cursor = Cursor {
            stream: tokens.original_tokens.as_slice(),
            start: &tokens.tokens[0],
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
    tokens.skip_whitespace();
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

#[derive(Debug, Clone, PartialEq)]
pub struct TokenStream {
    original_tokens: Vec<TokenTree>,
    tokens: Vec<(usize, TokenTree)>,
    source: Rc<SourceFile>,
}

impl TokenStream {
    fn new(tokens: Vec<TokenTree>, source: Rc<SourceFile>) -> TokenStream {
        TokenStream {
            original_tokens: tokens.clone(),
            tokens: tokens.into_iter().enumerate().collect(),
            source,
        }
    }

    pub fn filter<F: FnMut(&TokenStream) -> Vec<usize>>(&mut self, mut function: F) {
        let mut indices = function(self);
        indices.sort_unstable();
        indices.reverse();
        for index in indices {
            self.tokens.remove(index);
        }
    }

    pub fn skip_whitespace(&mut self) {
        self.filter(|tokens| {
            let mut indices = vec![];
            for (index, (_, token)) in tokens.tokens.iter().enumerate() {
                if let TokenTree::WhiteSpace(_) = token {
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
        ParseBuffer {
            cursor,
            source,
        }
    }

    pub fn parse<T: Parse>(&self) -> Result<T> {
        T::parse(self)
    }

    pub fn parse_with<T, F: FnOnce(ParseStream<'_>) -> Result<T>>(&self, function: F) -> Result<T> {
        function(self)
    }

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
        while let (TokenTree::Error(_), offset) = self.cursor.next() {
            self.cursor.offset.set(offset);
            error = true;
        }
        if error {
            Err(Error::new(Rc::clone(&self.source), ErrorKind::Silent))
        } else {
            Ok(())
        }
    }

    fn next(&self) -> Result<&'a TokenTree> {
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

    fn current(&self) -> Result<&'a (usize, TokenTree)> {
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

    fn get_relative(&self, offset: isize) -> Result<&'a (usize, TokenTree)> {
        self.cursor.get_relative(offset).ok_or(Error::new(
            Rc::clone(&self.source),
            ErrorKind::EndOfFile(self.source.contents.len()),
        ))
    }

    fn get_absolute_range_original(&self, range: Range<usize>) -> Result<&'a [TokenTree]> {
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

    pub fn lookahead(&self) -> Lookahead<'a> {
        Lookahead::new(self.fork())
    }
}

/// The input type for all parsing functions. This is a stable alias for
/// [`ParseBuffer`].
pub type ParseStream<'a> = &'a ParseBuffer<'a>;

#[derive(Debug, Clone)]
struct Cursor<'a> {
    stream: *const [TokenTree],
    start: *const (usize, TokenTree),
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

    fn ptr(&self) -> *const (usize, TokenTree) {
        // SAFETY: `offset` is always less than `len`
        unsafe { self.start.add(self.offset.get()) }
    }

    fn current(&self) -> &'a (usize, TokenTree) {
        // SAFETY: `ptr` is valid for 'a.
        unsafe { &*self.ptr() }
    }

    pub fn eof(&self) -> bool {
        self.offset.get() == self.last
    }

    fn next(&self) -> (&'a TokenTree, usize) {
        // SAFETY: `ptr` is valid for 'a.
        let (_, token_tree) = unsafe { &*self.ptr() };
        let offset = self.bump();
        (token_tree, offset)
    }

    fn get_relative(&self, offset: isize) -> Option<&'a (usize, TokenTree)> {
        if ((self.offset.get() as isize + offset) as usize) < self.last {
            // SAFETY: guaranteed by condition
            let ptr = unsafe { self.ptr().offset(offset) };
            // SAFETY: `ptr` is live for 'a and is guaranteed by condition
            // to be valid.
            Some(unsafe { &*self.ptr().offset(offset) })
        } else {
            None
        }
    }

    fn stream(&self) -> &'a [TokenTree] {
        // SAFETY: `stream` is live for 'a
        unsafe { &*self.stream }
    }

    fn get_absolute_range_original(&self, range: Range<usize>) -> Option<&'a [TokenTree]> {
        self.stream().get(range)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum TokenTree {
    Error(Span),
    Ident(Ident),
    Punct(SingleCharPunct),
    Literal(Literal),
    WhiteSpace(WhiteSpace),
    End,
}

impl TokenTree {
    fn span(&self) -> &Span {
        match self {
            TokenTree::Error(span) => span,
            TokenTree::Ident(ident) => &ident.span,
            TokenTree::Punct(punct) => &punct.span,
            TokenTree::Literal(literal) => &literal.span,
            TokenTree::WhiteSpace(whitespace) => whitespace.span(),
            TokenTree::End => panic!("called `span` on `TokenTree::End`"),
        }
    }

    fn set_span(&mut self, span: Span) {
        match self {
            TokenTree::Error(current_span) => *current_span = span,
            TokenTree::Ident(ident) => ident.span = span,
            TokenTree::Punct(punct) => punct.span = span,
            TokenTree::Literal(literal) => literal.span = span,
            TokenTree::WhiteSpace(whitespace) => whitespace.set_span(span),
            TokenTree::End => {}
        }
    }
}

impl From<Ident> for TokenTree {
    fn from(value: Ident) -> Self {
        Self::Ident(value)
    }
}

impl From<SingleCharPunct> for TokenTree {
    fn from(value: SingleCharPunct) -> Self {
        Self::Punct(value)
    }
}

impl From<Literal> for TokenTree {
    fn from(value: Literal) -> Self {
        Self::Literal(value)
    }
}

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
