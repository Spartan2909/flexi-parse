//! Streams of tokens delimited by punctuation.
//!
//! The core of this module is the [`Group`] type, which represents a delimited
//! group of tokens.
//!
//! # Basic usage
//! ```
//! # use flexi_parse::group::Group;
//! # use flexi_parse::group::Parentheses;
//! # use flexi_parse::token::LitInt;
//! # use flexi_parse::parse;
//! # use flexi_parse::parse_string;
//! #
//! # fn main () {
//! let group: Group<Parentheses> = parse_string("(3)".to_string()).unwrap();
//! let lit: LitInt = parse(group.into_token_stream()).unwrap();
//! assert_eq!(lit.value(), 3);
//! # }
//! ```
//!
//! # The `Delimiter` trait
//! The `Delimiter` trait is used to provide information to [`Group`] on how to
//! parse the delimiters. It is typically implemented for newtype structs
//! wrapping a span.
//!
//! ## Example
//! Below is an example of a custom set of delimiters.
//!
//! ```
//! # use flexi_parse::group::Delimiters;
//! # use flexi_parse::group::Group;
//! # use flexi_parse::token::Ident;
//! # use flexi_parse::parse;
//! # use flexi_parse::parse_string;
//! # use flexi_parse::Punct;
//! # use flexi_parse::Span;
//! #
//! struct AtDelimiters(Span);
//!
//! impl From<Span> for AtDelimiters {
//!     fn from(value: Span) -> Self {
//!         AtDelimiters(value)
//!     }
//! }
//!
//! impl Delimiters for AtDelimiters {
//!     type Start = Punct!["@"];
//!     type End = Punct!["@"];
//!     const CAN_NEST: bool = false;
//!
//!     fn span(&self) -> &Span {
//!         &self.0
//!     }
//! }
//!
//! # fn main () {
//! let group: Group<AtDelimiters> = parse_string("@hello@".to_string()).unwrap();
//! let hello: Ident = parse(group.into_token_stream()).unwrap();
//! assert_eq!(hello.string(), "hello");
//! # }
//! ```

use crate::scanner;
use crate::token::DoubleQuote;
use crate::token::LAngle;
use crate::token::LeftBrace;
use crate::token::LeftBracket;
use crate::token::LeftParen;
use crate::token::Punct;
use crate::token::RAngle;
use crate::token::RightBrace;
use crate::token::RightBracket;
use crate::token::RightParen;
use crate::token::SingleQuote;
use crate::token::Token;
use crate::Parse;
use crate::ParseStream;
use crate::Result;
use crate::Span;
use crate::TokenStream;

use std::marker::PhantomData;
use std::rc::Rc;

/// A trait for types that represent the delimiters of a group.
///
/// For more information, see the [module documentation][module].
///
/// [module]: crate::group#the-delimiter-trait
pub trait Delimiters: From<Span> {
    /// The opening delimiter, e.g. `(`.
    type Start: Punct;
    /// The closing delimiter, e.g. `)`.
    type End: Punct;
    /// Whether groups delimited by this pair can nest. Typically, this will be
    /// false if the opening and closing delimiters are the same, and true
    /// otherwise.
    const CAN_NEST: bool;

    /// The span covered by these delimiters.
    fn span(&self) -> &Span;
}

/// The delimiters `(` `)`.
#[derive(Debug, Clone)]
pub struct Parentheses(pub Span);

impl PartialEq for Parentheses {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl From<Span> for Parentheses {
    fn from(value: Span) -> Self {
        Parentheses(value)
    }
}

impl Delimiters for Parentheses {
    type Start = LeftParen;
    type End = RightParen;
    const CAN_NEST: bool = true;

    fn span(&self) -> &Span {
        &self.0
    }
}

/// The delimiters `[` `]`.
#[derive(Debug, Clone)]
pub struct Brackets(pub Span);

impl PartialEq for Brackets {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl From<Span> for Brackets {
    fn from(value: Span) -> Self {
        Brackets(value)
    }
}

impl Delimiters for Brackets {
    type Start = LeftBracket;
    type End = RightBracket;
    const CAN_NEST: bool = true;

    fn span(&self) -> &Span {
        &self.0
    }
}

/// The delimiters `{` `}`.
#[derive(Debug, Clone)]
pub struct Braces(pub Span);

impl PartialEq for Braces {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl From<Span> for Braces {
    fn from(value: Span) -> Self {
        Braces(value)
    }
}

impl Delimiters for Braces {
    type Start = LeftBrace;
    type End = RightBrace;
    const CAN_NEST: bool = true;

    fn span(&self) -> &Span {
        &self.0
    }
}

/// The delimiters `<` `>`.
#[derive(Debug, Clone)]
pub struct AngleBrackets(pub Span);

impl From<Span> for AngleBrackets {
    fn from(value: Span) -> Self {
        AngleBrackets(value)
    }
}

impl Delimiters for AngleBrackets {
    type Start = LAngle;
    type End = RAngle;
    const CAN_NEST: bool = true;

    fn span(&self) -> &Span {
        &self.0
    }
}

/// The delimiters `'` `'`.
///
/// See also [`LitStrSingleQuote`] and [`LitChar`].
///
/// [`LitStrSingleQuote`]: struct@crate::token::LitStrSingleQuote
/// [`LitChar`]: struct@crate::token::LitChar
#[derive(Debug, Clone)]
pub struct SingleQuotes(pub Span);

impl PartialEq for SingleQuotes {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl From<Span> for SingleQuotes {
    fn from(value: Span) -> Self {
        SingleQuotes(value)
    }
}

impl Delimiters for SingleQuotes {
    type Start = SingleQuote;
    type End = SingleQuote;
    const CAN_NEST: bool = false;

    fn span(&self) -> &Span {
        &self.0
    }
}

/// The delimiters `"` `"`.
#[derive(Debug, Clone)]
pub struct DoubleQuotes(pub Span);

impl PartialEq for DoubleQuotes {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl From<Span> for DoubleQuotes {
    fn from(value: Span) -> Self {
        DoubleQuotes(value)
    }
}

impl Delimiters for DoubleQuotes {
    type Start = DoubleQuote;
    type End = DoubleQuote;
    const CAN_NEST: bool = false;

    fn span(&self) -> &Span {
        &self.0
    }
}

pub(crate) fn delimited_string<D: Delimiters>(
    input: ParseStream<'_>,
) -> Result<(D::Start, D::End)> {
    let start: D::Start = input.parse()?;
    if D::CAN_NEST {
        let mut open = 1;
        loop {
            if D::End::peek(input) {
                open -= 1;
                if open == 0 {
                    break;
                } else {
                    input.next()?;
                }
            } else if D::Start::peek(input) {
                open += 1;
                input.next()?;
            } else {
                input.next()?;
            }
        }
    } else {
        while !D::End::peek(input) {
            input.next()?;
        }
    }
    let end: D::End = input.parse().map_err(|mut err| {
        err.eof_to_group(start.span().clone(), D::Start::display());
        err
    })?;
    Ok((start, end))
}

/// A delimited group.
///
/// For more information, see the [module documentation][module].
///
/// [module]: (crate::group).
#[derive(Debug, Clone)]
pub struct Group<D: Delimiters> {
    pub(crate) token_stream: TokenStream,
    pub(crate) span: Span,
    _marker: PhantomData<D>,
}

impl<D: Delimiters> Group<D> {
    /// Returns the contained [`TokenStream`].
    pub fn into_token_stream(self) -> TokenStream {
        self.token_stream
    }

    /// Returns a token representing the delimiters of this group.
    pub fn delimiters(&self) -> D {
        D::from(self.span.clone())
    }

    /// Removes whitespace from the tokenstream in `self`.
    pub fn remove_whitespace(&mut self) {
        self.token_stream.remove_whitespace();
    }

    /// Consumes `self`, returning a new [`Group<D>`] with all whitespace
    /// removed.
    pub fn without_whitespace(mut self) -> Group<D> {
        self.remove_whitespace();
        self
    }
}

impl<D: Delimiters> Parse for Group<D> {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let (start, end) = delimited_string::<D>(input)?;
        let (start, end) = (start.span(), end.span());
        let (token_stream, _) = scanner::scan(Rc::clone(&input.source), start.end, Some(end.start));
        let span = Span::new(start.start, end.end, Rc::clone(&input.source));
        Ok(Group {
            token_stream,
            span,
            _marker: PhantomData,
        })
    }
}

/// A macro to easily parse a delimited group.
/// 
/// Similar in concept to [`syn::parenthesized`][parenthesized] and family, but
/// compatible with anything implementing the [`Delimiters`] trait.
/// 
/// [parenthesized]: https://docs.rs/syn/latest/syn/macro.parenthesized.html
///
/// ```
/// # use flexi_parse::group;
/// # use flexi_parse::group::Braces;
/// # use flexi_parse::group::Group;
/// # use flexi_parse::token::LitInt;
/// # use flexi_parse::Parse;
/// # use flexi_parse::ParseStream;
/// # use flexi_parse::Punct;
/// # use flexi_parse::Result;
/// # use flexi_parse::parse_string;
///
/// enum Statement {
///     Block(Block),
///     Number(LitInt, Punct![";"]),
///     // Other nodes
/// }
///
/// impl Parse for Statement {
///     fn parse(input: ParseStream<'_>) -> Result<Self> {
///         if input.peek(Punct!["{"]) {
///             Ok(Statement::Block(input.parse()?))
///         } else if input.peek(LitInt) {
///             Ok(Statement::Number(input.parse()?, input.parse()?))
///         } else {
///             // Other nodes
///             # todo!()
///         }
///     }
/// }
///
/// struct Block {
///     braces: Braces,
///     statements: Vec<Statement>,
/// }
///
/// impl Parse for Block {
///     fn parse(input: ParseStream<'_>) -> Result<Self> {
///         let content: Group<Braces>;
///         Ok(Block {
///             braces: group!(content in input),
///             statements: input.parse_repeated()?,
///         })
///     }
/// }
///
/// # fn main () {
/// let ast: Statement = parse_string("{
///     3; 5;
/// }".to_string()).unwrap();
/// # }
///
/// ```
#[macro_export]
macro_rules! group {
    ($tokens:ident in $input:expr) => {{
        $tokens = $crate::group::Group::without_whitespace($input.parse()?);
        $tokens.delimiters()
    }};
}
