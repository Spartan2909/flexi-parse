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
use std::mem;
use std::path::PathBuf;
use std::rc::Rc;
use std::result;

pub mod error;
mod scanner;
pub mod token;
use error::Error;
use error::ErrorKind;
use token::Ident;
use token::Literal;
use token::PunctKind;
use token::SingleCharPunct;
use token::Token;

#[cfg(any(feature = "proc-macro", feature = "proc-macro2"))]
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

    #[doc(hidden)]
    pub fn source(&self) -> &Rc<SourceFile> {
        &self.source
    }
}

pub trait Parse: Sized {
    fn parse(input: ParseStream<'_>) -> Result<Self>;
}

pub trait Parser: Sized {
    type Output;

    fn parse(self, tokens: TokenStream) -> Result<Self::Output>;
}

impl<F, T> Parser for F
where
    F: FnOnce(ParseStream<'_>) -> Result<T>,
{
    type Output = T;

    fn parse(self, tokens: TokenStream) -> Result<T> {
        let cursor = Cursor {
            start: &tokens.tokens[0],
            ptr: &tokens.tokens[0],
            end: tokens.tokens.last().unwrap(),
            _marker: PhantomData,
        };
        let parse_buffer = ParseBuffer::new(cursor, tokens.source);
        self(&parse_buffer)
    }
}

pub fn parse<T: Parse>(tokens: TokenStream) -> Result<T> {
    Parser::parse(T::parse, tokens)
}

pub fn parse_source<T: Parse>(source: Rc<SourceFile>) -> Result<T> {
    let (tokens, error) = scanner::scan(source);
    if let Some(error) = error {
        return Err(error);
    }
    parse(tokens)
}

pub fn parse_string<T: Parse>(source: String) -> Result<T> {
    let source = Rc::new(SourceFile {
        name: "str".to_string(),
        path: None,
        contents: source,
    });
    parse_source(source)
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenStream {
    tokens: Vec<TokenTree>,
    source: Rc<SourceFile>,
}

impl TokenStream {
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
            for (index, token) in tokens.tokens.iter().enumerate() {
                if matches!(
                    token,
                    TokenTree::Punct(SingleCharPunct {
                        kind: PunctKind::Space,
                        ..
                    })
                ) {
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
    // Instead of Cell<Cursor<'a>> so that ParseBuffer<'a> is covariant in 'a.
    cursor: Cell<Cursor<'static>>,
    source: Rc<SourceFile>,
    _marker: PhantomData<Cursor<'a>>,
}

impl<'a> ParseBuffer<'a> {
    fn new(cursor: Cursor<'a>, source: Rc<SourceFile>) -> ParseBuffer<'a> {
        ParseBuffer {
            // SAFETY: See comment on struct field.
            cursor: Cell::new(unsafe { mem::transmute::<Cursor<'a>, Cursor<'static>>(cursor) }),
            source,
            _marker: PhantomData,
        }
    }

    pub fn parse<T: Parse>(&self) -> Result<T> {
        T::parse(self)
    }

    pub fn call<T, F: FnOnce(ParseStream<'_>) -> Result<T>>(&self, function: F) -> Result<T> {
        function(self)
    }

    fn try_parse<T: Parse>(&self) -> Result<T> {
        let cursor = self.cursor.get();
        T::parse(self).map_err(move |err| {
            self.cursor.set(cursor);
            err
        })
    }

    pub fn peek<T: Token>(&self) -> bool {
        self.parse_undo::<T>().is_ok()
    }

    fn parse_undo<T: Parse>(&self) -> Result<T> {
        let cursor = self.cursor.get();
        let val = T::parse(self)?;
        self.cursor.set(cursor);
        Ok(val)
    }

    fn next(&self) -> Result<&TokenTree> {
        if self.cursor.get().eof() {
            Err(Error::new(
                Rc::clone(&self.source),
                ErrorKind::EndOfFile(self.source.contents.len()),
            ))
        } else {
            let (token, cursor) = self.cursor.get().next();
            self.cursor.set(cursor);
            Ok(token)
        }
    }

    fn current(&self) -> Result<&'a TokenTree> {
        if self.cursor.get().eof() {
            Err(Error::new(
                Rc::clone(&self.source),
                ErrorKind::EndOfFile(self.source.contents.len()),
            ))
        } else {
            Ok(self.cursor.get().current())
        }
    }

    fn get(&self, offset: isize) -> Result<&'a TokenTree> {
        self.cursor.get().get(offset).ok_or(Error::new(
            Rc::clone(&self.source),
            ErrorKind::EndOfFile(self.source.contents.len()),
        ))
    }
}

/// The input type for all parsing functions. This is a stable alias for
/// [`ParseBuffer`].
pub type ParseStream<'a> = &'a ParseBuffer<'a>;

#[derive(Debug, Clone, Copy)]
struct Cursor<'a> {
    start: *const TokenTree,
    ptr: *const TokenTree,
    end: *const TokenTree,
    _marker: PhantomData<&'a TokenTree>,
}

impl<'a> Cursor<'a> {
    /// # Safety
    /// Must not be at the end of the buffer.
    unsafe fn bump(self) -> Cursor<'a> {
        if self.ptr == self.end {
            self
        } else {
            // SAFETY: Must be upheld by caller.
            let ptr = unsafe { self.ptr.add(1) };
            Cursor {
                start: self.start,
                ptr,
                end: self.end,
                _marker: PhantomData,
            }
        }
    }

    fn current(self) -> &'a TokenTree {
        // SAFETY: `ptr` is valid for 'a.
        unsafe { &*self.ptr }
    }

    pub fn eof(self) -> bool {
        self.ptr == self.end
    }

    fn next(self) -> (&'a TokenTree, Cursor<'a>) {
        // SAFETY: `ptr` is valid for 'a.
        let token_tree = unsafe { &*self.ptr };
        // SAFETY: Guaranteed by condition.
        let cursor = unsafe { self.bump() };
        (token_tree, cursor)
    }

    fn get(self, offset: isize) -> Option<&'a TokenTree> {
        let ptr = self.ptr as isize + offset;
        if self.start as isize <= ptr && self.end as isize > ptr {
            // SAFETY: `ptr` is live for 'a and is guaranteed by condition
            // to be valid.
            Some(unsafe { &*(ptr as *const TokenTree) })
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum TokenTree {
    Error(Span),
    Ident(Ident),
    Punct(SingleCharPunct),
    Literal(Literal),
    End,
}

impl TokenTree {
    fn span(&self) -> &Span {
        match self {
            TokenTree::Error(span) => span,
            TokenTree::Ident(ident) => &ident.span,
            TokenTree::Punct(punct) => &punct.span,
            TokenTree::Literal(literal) => &literal.span,
            TokenTree::End => panic!("called `span` on `TokenTree::End`"),
        }
    }

    fn is_whitespace(&self) -> bool {
        if let TokenTree::Punct(punct) = self {
            if punct.kind == PunctKind::Space {
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn set_span(&mut self, span: Span) {
        match self {
            TokenTree::Error(current_span) => *current_span = span,
            TokenTree::Ident(ident) => ident.span = span,
            TokenTree::Punct(punct) => punct.span = span,
            TokenTree::Literal(literal) => literal.span = span,
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

#[doc(hidden)]
pub mod private {
    pub trait Sealed {}
}
