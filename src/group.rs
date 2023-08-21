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
//! }
//!
//! # fn main () {
//! let group: Group<AtDelimiters> = parse_string("@hello@".to_string()).unwrap();
//! let hello: Ident = parse(group.into_token_stream()).unwrap();
//! assert_eq!(hello.string(), "hello");
//! # }
//! ```

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
use crate::Entry;
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
}

/// The delimiters `(` `)`.
#[derive(Debug, Clone)]
pub struct Parentheses(pub Span);

impl From<Span> for Parentheses {
    fn from(value: Span) -> Self {
        Parentheses(value)
    }
}

impl Delimiters for Parentheses {
    type Start = LeftParen;
    type End = RightParen;
    const CAN_NEST: bool = true;
}

/// The delimiters `[` `]`.
#[derive(Debug, Clone)]
pub struct Brackets(pub Span);

impl From<Span> for Brackets {
    fn from(value: Span) -> Self {
        Brackets(value)
    }
}

impl Delimiters for Brackets {
    type Start = LeftBracket;
    type End = RightBracket;
    const CAN_NEST: bool = true;
}

/// The delimiters `{` `}`.
#[derive(Debug, Clone)]
pub struct Braces(pub Span);

impl From<Span> for Braces {
    fn from(value: Span) -> Self {
        Braces(value)
    }
}

impl Delimiters for Braces {
    type Start = LeftBrace;
    type End = RightBrace;
    const CAN_NEST: bool = true;
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
}

/// The delimiters `'` `'`.
///
/// See also [`LitStrSingleQuote`] and [`LitChar`].
///
/// [`LitStrSingleQuote`]: crate::token::LitStrSingleQuote
/// [`LitChar`]: crate::token::LitChar
#[derive(Debug, Clone)]
pub struct SingleQuotes(pub Span);

impl From<Span> for SingleQuotes {
    fn from(value: Span) -> Self {
        SingleQuotes(value)
    }
}

impl Delimiters for SingleQuotes {
    type Start = SingleQuote;
    type End = SingleQuote;
    const CAN_NEST: bool = false;
}

/// The delimiters `"` `"`.
#[derive(Debug, Clone)]
pub struct DoubleQuotes(pub Span);

impl From<Span> for DoubleQuotes {
    fn from(value: Span) -> Self {
        DoubleQuotes(value)
    }
}

impl Delimiters for DoubleQuotes {
    type Start = DoubleQuote;
    type End = DoubleQuote;
    const CAN_NEST: bool = false;
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
}

impl<D: Delimiters> Parse for Group<D> {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let group_start = input.parse::<D::Start>()?.span().start;
        let start = input.current()?.0;

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

        let end_of_last_token = input.get_relative(-1)?.1.span().end;
        let end = input
            .current()
            .map_err(|mut err| {
                err.eof_to_group(
                    Span::new(group_start, end_of_last_token, Rc::clone(&input.source)),
                    D::Start::display(),
                );
                err
            })?
            .0;
        let end_token: D::End = input.parse()?;
        let group_end = end_token.span().end;
        let mut tokens = input.get_absolute_range_original(start..end)?.to_vec();
        tokens.push(Entry::End);
        let token_stream = TokenStream::new(tokens, Rc::clone(&input.source));
        let span = Span::new(group_start, group_end, Rc::clone(&input.source));
        Ok(Group {
            token_stream,
            span,
            _marker: PhantomData,
        })
    }
}
