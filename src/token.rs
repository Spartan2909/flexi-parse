//! Tokens representing punctuation, identifiers, keywords, and whitespace.
//!
//! Types for parsing delimited groups can be found in the
//! [`group`](mod@crate::group) module.
//!
//! The punctuation tokens can be most easily accessed using the
//! [`Punct`](crate::Punct) macro.

use crate::error::Error;
use crate::error::ErrorKind;
use crate::group::parse_delimiters;
use crate::group::DoubleQuotes;
use crate::group::SingleQuotes;
use crate::private::Marker;
use crate::private::Sealed;
use crate::to_tokens::ToTokens;
use crate::Entry;
use crate::Parse;
use crate::ParseStream;
use crate::Result;
use crate::Span;
use crate::TokenStream;

use std::cmp::Ordering;
use std::collections::HashSet;
use std::fmt;
use std::hash;
use std::result;
use std::sync::Arc;

/// A trait for types that can be represented by a single token.
///
/// This trait is sealed, and cannot be implemented by types outside of this
/// crate.
pub trait Token: Parse + fmt::Debug + Sealed {
    /// Returns the span covered by this token.
    fn span(&self) -> &Span;

    #[doc(hidden)]
    fn set_span(&mut self, span: Span);

    #[doc(hidden)]
    fn display() -> String;
}

impl<T: Token> Parse for Option<T> {
    fn parse(input: ParseStream) -> Result<Self> {
        input
            .try_parse()
            .map_or_else(|_| Ok(None), |value| Ok(Some(value)))
    }
}

impl<T: Token> From<&T> for Span {
    fn from(value: &T) -> Self {
        value.span().to_owned()
    }
}

/// A trait for punctuation tokens.
///
/// This trait is sealed, and cannot be implemented by types outside of this
/// crate.
pub trait Punct: Token {
    #[doc(hidden)]
    fn peek(input: ParseStream<'_>) -> bool;
}

/// A string literal delimited by double quotes.
///
/// See also [`DoubleQuotes`].
#[derive(Debug, Clone)]
pub struct LitStrDoubleQuote {
    string: String,
    span: Span,
}

impl LitStrDoubleQuote {
    /// Returns the string within the quotes of this literal.
    pub const fn string(&self) -> &String {
        &self.string
    }

    /// Constructs a new `LitStrDoubleQuote` with the given value and span.
    pub const fn new(string: String, span: Span) -> LitStrDoubleQuote {
        LitStrDoubleQuote { string, span }
    }
}

impl PartialEq for LitStrDoubleQuote {
    fn eq(&self, other: &Self) -> bool {
        self.string == other.string
    }
}

impl Eq for LitStrDoubleQuote {}

impl PartialOrd for LitStrDoubleQuote {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LitStrDoubleQuote {
    fn cmp(&self, other: &Self) -> Ordering {
        self.string.cmp(&other.string)
    }
}

impl hash::Hash for LitStrDoubleQuote {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.string.hash(state);
    }
}

impl Parse for LitStrDoubleQuote {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        #[cfg(feature = "scan-strings")]
        if let Entry::LitStrDoubleQuote(string) = input.current()? {
            input.next_raw();
            return Ok(string.clone());
        }

        let (start, end, _) = parse_delimiters::<DoubleQuotes>(input).map_err(|mut err| {
            err.group_to_string();
            err
        })?;
        let (start, end) = (start.span(), end.span());
        let string = input.source.contents()[start.end()..end.start()].to_owned();
        let span = Span::across(start, end).unwrap();
        Ok(LitStrDoubleQuote { string, span })
    }
}

impl Sealed for LitStrDoubleQuote {}

impl Token for LitStrDoubleQuote {
    fn span(&self) -> &Span {
        &self.span
    }

    fn set_span(&mut self, span: Span) {
        self.span = span;
    }

    fn display() -> String {
        "a string literal".to_string()
    }
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub const fn LitStrDoubleQuote(marker: Marker) -> LitStrDoubleQuote {
    match marker {}
}

/// A string literal delimited by single quotes.
///
/// See also [`SingleQuotes`].
#[derive(Debug, Clone)]
pub struct LitStrSingleQuote {
    string: String,
    span: Span,
}

impl LitStrSingleQuote {
    /// Returns the string within the quotes of this literal.
    pub const fn string(&self) -> &String {
        &self.string
    }

    /// Constructs a new `LitStrSingleQuote` with the given value and span.
    pub const fn new(string: String, span: Span) -> LitStrSingleQuote {
        LitStrSingleQuote { string, span }
    }
}

impl PartialEq for LitStrSingleQuote {
    fn eq(&self, other: &Self) -> bool {
        self.string == other.string
    }
}

impl Eq for LitStrSingleQuote {}

impl PartialOrd for LitStrSingleQuote {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LitStrSingleQuote {
    fn cmp(&self, other: &Self) -> Ordering {
        self.string.cmp(&other.string)
    }
}

impl hash::Hash for LitStrSingleQuote {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.string.hash(state);
    }
}

impl Parse for LitStrSingleQuote {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        #[cfg(feature = "scan-strings")]
        if let Entry::LitStrSingleQuote(string) = input.current()? {
            input.next_raw();
            return Ok(string.clone());
        }

        let (start, end, _) = parse_delimiters::<SingleQuotes>(input).map_err(|mut err| {
            err.group_to_string();
            err
        })?;
        let (start, end) = (start.span(), end.span());
        let string = input.source.contents()[start.end()..end.start()].to_owned();
        let span = Span::across(start, end).unwrap();
        Ok(LitStrSingleQuote { string, span })
    }
}

impl Sealed for LitStrSingleQuote {}

impl Token for LitStrSingleQuote {
    fn span(&self) -> &Span {
        &self.span
    }

    fn set_span(&mut self, span: Span) {
        self.span = span;
    }

    fn display() -> String {
        "a string literal".to_string()
    }
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub const fn LitStrSingleQuote(marker: Marker) -> LitStrSingleQuote {
    match marker {}
}

/// A character literal delimited by single quotes.
///
/// See also [`SingleQuotes`].
#[derive(Debug, Clone)]
pub struct LitChar {
    ch: char,
    span: Span,
}

impl LitChar {
    /// Returns the character within this literal.
    pub const fn ch(&self) -> char {
        self.ch
    }

    /// Constructs a new `LitChar` with the given value and span.
    pub const fn new(ch: char, span: Span) -> LitChar {
        LitChar { ch, span }
    }
}

impl PartialEq for LitChar {
    fn eq(&self, other: &Self) -> bool {
        self.ch == other.ch
    }
}

impl Eq for LitChar {}

impl PartialOrd for LitChar {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LitChar {
    fn cmp(&self, other: &Self) -> Ordering {
        self.ch.cmp(&other.ch)
    }
}

impl hash::Hash for LitChar {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.ch.hash(state);
    }
}

impl Parse for LitChar {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let string: LitStrSingleQuote = input.parse().map_err(|mut err| {
            err.string_to_char();
            err
        })?;
        if string.string.len() != 1 {
            return Err(Error::new(
                Arc::clone(&input.source),
                ErrorKind::LongChar(string.span),
            ));
        }
        let ch = string.string.chars().next().unwrap();
        Ok(LitChar {
            ch,
            span: string.span,
        })
    }
}

impl Sealed for LitChar {}

impl Token for LitChar {
    fn span(&self) -> &Span {
        &self.span
    }

    fn set_span(&mut self, span: Span) {
        self.span = span;
    }

    fn display() -> String {
        "a character literal".to_string()
    }
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub const fn LitChar(marker: Marker) -> LitChar {
    match marker {}
}

/// An identifier consisting of alphanumeric characters and underscores, and
/// starting with an alphabetic character or an underscore.
#[derive(Debug, Clone)]
pub struct Ident {
    pub(crate) string: String,
    pub(crate) span: Span,
}

impl Ident {
    /// Returns the text that makes up the identifier.
    pub const fn string(&self) -> &String {
        &self.string
    }

    /// Creates a new identifier in the given [`ParseStream`] with the given
    /// string.
    pub const fn new(string: String, span: Span) -> Ident {
        Ident { string, span }
    }

    fn parse_simple(input: ParseStream) -> Result<Self> {
        let token = input.next()?;
        Self::try_from(token.to_owned()).map_or_else(
            |_| {
                Err(Error::new(
                    Arc::clone(&input.source),
                    ErrorKind::UnexpectedToken {
                        expected: HashSet::from_iter(["an identifier".to_string()]),
                        span: token.span().clone(),
                    },
                ))
            },
            Ok,
        )
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.string == other.string
    }
}

impl Eq for Ident {}

impl PartialOrd for Ident {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Ident {
    fn cmp(&self, other: &Self) -> Ordering {
        self.string.cmp(&other.string)
    }
}

impl hash::Hash for Ident {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.string.hash(state);
    }
}

impl TryFrom<Entry> for Ident {
    type Error = Entry;

    fn try_from(value: Entry) -> result::Result<Self, Self::Error> {
        if let Entry::Ident(token) = value {
            Ok(token)
        } else {
            Err(value)
        }
    }
}

impl Parse for Ident {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = Self::parse_simple(input)?;
        if ident.string.chars().next().unwrap().is_ascii_digit() {
            Err(Error::new(
                Arc::clone(&input.source),
                ErrorKind::UnexpectedToken {
                    expected: HashSet::from_iter(["an identifier".to_string()]),
                    span: ident.span().clone(),
                },
            ))
        } else {
            Ok(ident)
        }
    }
}

impl Sealed for Ident {}

impl Token for Ident {
    fn span(&self) -> &Span {
        &self.span
    }

    fn set_span(&mut self, span: Span) {
        self.span = span;
    }

    fn display() -> String {
        "an identifier".to_string()
    }
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub const fn Ident(marker: Marker) -> Ident {
    match marker {}
}

#[derive(Debug, Clone)]
pub(crate) struct SingleCharPunct {
    pub(super) kind: PunctKind,
    pub(super) spacing: Spacing,
    pub(super) span: Span,
}

impl SingleCharPunct {
    pub(crate) const fn new(kind: PunctKind, spacing: Spacing, span: Span) -> SingleCharPunct {
        SingleCharPunct {
            kind,
            spacing,
            span,
        }
    }
}

impl PartialEq for SingleCharPunct {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.spacing == other.spacing
    }
}

impl Eq for SingleCharPunct {}

impl PartialOrd for SingleCharPunct {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SingleCharPunct {
    fn cmp(&self, other: &Self) -> Ordering {
        self.kind
            .cmp(&other.kind)
            .then(self.spacing.cmp(&other.spacing))
    }
}

impl hash::Hash for SingleCharPunct {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
        self.spacing.hash(state);
    }
}

impl<'a> TryFrom<&'a Entry> for &'a SingleCharPunct {
    type Error = ();

    fn try_from(value: &'a Entry) -> result::Result<Self, Self::Error> {
        if let Entry::Punct(token) = value {
            Ok(token)
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Spacing {
    Alone,
    Joint,
}

impl From<Result<char>> for Spacing {
    fn from(value: Result<char>) -> Self {
        value.map_or(Spacing::Alone, |c| {
            if PunctKind::try_from(c).is_ok() {
                Spacing::Joint
            } else {
                Spacing::Alone
            }
        })
    }
}

/// An integer literal.
///
/// The default parsing implementation accepts either a string of ascii digits,
/// or `0b`, `0o`, or `0x`, followed by a number in base 2, 8, or 16
/// respectively.
#[derive(Debug, Clone)]
pub struct LitInt {
    value: u64,
    span: Span,
}

impl LitInt {
    /// Returns the value of this literal.
    pub const fn value(&self) -> u64 {
        self.value
    }

    fn parse_decimal_impl(input: ParseStream) -> Result<Self> {
        let ident = Ident::parse_simple(input)?;
        Ok(LitInt {
            value: ident.string.parse().map_err(|_| Error::empty())?,
            span: ident.span,
        })
    }

    /// Parses a base 10 integer.
    ///
    /// ## Errors
    /// Returns an error if the input is not a base 10 integer.
    pub fn parse_decimal(input: ParseStream) -> Result<Self> {
        Self::parse_decimal_impl(input).map_err(|_| {
            input.unexpected_token(HashSet::from_iter(["an integer literal".to_string()]))
        })
    }

    /// Constructs a new `LitInt` with the given value and span.
    pub const fn new(value: u64, span: Span) -> LitInt {
        LitInt { value, span }
    }

    fn parse_impl(input: ParseStream) -> Result<Self> {
        let ident = Ident::parse_simple(input)?;
        let mut chars = ident.string().chars();
        let start: u8 = chars
            .next()
            .unwrap()
            .to_string()
            .parse()
            .map_err(|_| Error::empty())?;
        if start == 0 && ident.string.len() >= 3 {
            let ch = chars.next().unwrap();
            let result = match ch {
                'b' | 'B' => Some(u64::from_str_radix(&ident.string[2..], 2)),
                'o' | 'O' => Some(u64::from_str_radix(&ident.string[2..], 8)),
                'x' | 'X' => Some(u64::from_str_radix(&ident.string[2..], 16)),
                _ => None,
            };
            if let Some(result) = result {
                return Ok(LitInt {
                    value: result.map_err(|_| Error::empty())?,
                    span: ident.span,
                });
            }
        }

        Ok(LitInt {
            value: ident.string.parse().map_err(|_| Error::empty())?,
            span: ident.span,
        })
    }
}

impl PartialEq for LitInt {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl Eq for LitInt {}

impl PartialOrd for LitInt {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LitInt {
    fn cmp(&self, other: &Self) -> Ordering {
        self.value.cmp(&other.value)
    }
}

impl hash::Hash for LitInt {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl Parse for LitInt {
    fn parse(input: ParseStream) -> Result<Self> {
        Self::parse_impl(input).map_or_else(
            |_| Err(input.unexpected_token(HashSet::from_iter(["an integer literal".to_string()]))),
            Ok,
        )
    }
}

impl Sealed for LitInt {}

impl Token for LitInt {
    fn span(&self) -> &Span {
        &self.span
    }

    fn set_span(&mut self, span: Span) {
        self.span = span;
    }

    fn display() -> String {
        "an integer".to_string()
    }
}

fn int_to_decimal(int: u64) -> f64 {
    // FIXME: This is horrendous
    format!("0.{int}").parse().unwrap()
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub const fn LitInt(marker: Marker) -> LitInt {
    match marker {}
}

/// A string of ascii digits followed by a `.`, and then another string of
/// ascii digits.
#[derive(Debug, Clone)]
pub struct LitFloat {
    value: f64,
    span: Span,
}

impl LitFloat {
    /// Returns the value of this literal.
    pub const fn value(&self) -> f64 {
        self.value
    }

    /// Constructs a new `LitFloat` with the given value and span.
    pub const fn new(value: f64, span: Span) -> LitFloat {
        LitFloat { value, span }
    }

    #[allow(clippy::cast_precision_loss)] // Should probably warn on this.
    fn parse_impl(input: ParseStream) -> Result<Self> {
        let start = LitInt::parse_decimal(input)?;
        let _: Dot = input.parse()?;
        let end = LitInt::parse_decimal(input)?;
        Ok(LitFloat {
            value: start.value as f64 + int_to_decimal(end.value),
            span: Span::new(start.span.start, end.span.end, Arc::clone(&input.source)),
        })
    }
}

impl PartialEq for LitFloat {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl PartialOrd for LitFloat {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

impl Parse for LitFloat {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        Self::parse_impl(input).map_or_else(
            |_| Err(input.unexpected_token(HashSet::from_iter(["a float literal".to_string()]))),
            Ok,
        )
    }
}

impl Sealed for LitFloat {}

impl Token for LitFloat {
    fn span(&self) -> &Span {
        &self.span
    }

    fn set_span(&mut self, span: Span) {
        self.span = span;
    }

    fn display() -> String {
        "a floating point literal".to_string()
    }
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub const fn LitFloat(marker: Marker) -> LitFloat {
    match marker {}
}

macro_rules! tokens {
    {
        {
            $( ($t1:ident, $name1:literal) )+
        }
        {
            $( ($t2:ident, $t21:ident, $t22:ident, $name2:literal) )+
        }
        {
            $( ($t3:ident, $t31:ident, $t32:ident, $t33:ident, $name3:literal) )+
        }
    } => {
        $(
            #[derive(Debug, Clone)]
            #[doc = concat!("`` ", $name1, " ``")]
            pub struct $t1 {
                /// The span covered by this token.
                pub span: Span,
            }

            impl $t1 {
                #[doc = concat!("Constructs a new `", stringify!($t1), "` with the given span.")]
                pub const fn new(span: Span) -> $t1 {
                    $t1 { span }
                }
            }

            impl Parse for $t1 {
                fn parse(input: ParseStream) -> Result<Self> {
                    let token = input.next()?.to_owned();
                    if let Entry::Punct(SingleCharPunct { kind: PunctKind::$t1, span, .. }) = token {
                        Ok(Self { span })
                    } else {
                        Err(Error::new(Arc::clone(&input.source), ErrorKind::UnexpectedToken {
                            expected: HashSet::from_iter(vec![format!("'{}'", $name1)]),
                            span: token.span().clone(),
                        }))
                    }
                }
            }

            impl Sealed for $t1 {}

            impl Token for $t1 {
                fn span(&self) -> &Span {
                    &self.span
                }

                fn set_span(&mut self, span: Span) {
                    self.span = span;
                }

                fn display() -> String {
                    $name1.to_string()
                }
            }

            impl fmt::Display for $t1 {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(f, "{}", $name1)
                }
            }

            impl Punct for $t1 {
                fn peek(input: ParseStream<'_>) -> bool {
                    input.peek($t1)
                }
            }

            impl PartialEq for $t1 {
                fn eq(&self, _other: &Self) -> bool {
                    true
                }
            }

            impl Eq for $t1 {}

            impl PartialOrd for $t1 {
                fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                    Some(self.cmp(other))
                }
            }

            impl Ord for $t1 {
                fn cmp(&self, _: &Self) -> Ordering {
                    Ordering::Equal
                }
            }

            impl hash::Hash for $t1 {
                fn hash<H: hash::Hasher>(&self, _: &mut H) {}
            }

            impl ToTokens for $t1 {
                fn to_tokens(&self, tokens: &mut TokenStream) {
                    tokens.push(Entry::Punct(SingleCharPunct::new(
                        PunctKind::$t1,
                        Spacing::Alone,
                        self.span.clone(),
                    )));
                }
            }

            #[cfg(feature = "proc-macro2")]
            impl quote::ToTokens for $t1 {
                fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
                    use quote::TokenStreamExt as _;
                    tokens.append_all(proc_macro2::TokenStream::try_from(ToTokens::to_token_stream(self)).unwrap());
                }

                fn to_token_stream(&self) -> proc_macro2::TokenStream {
                    ToTokens::to_token_stream(self).try_into().unwrap()
                }

                fn into_token_stream(self) -> proc_macro2::TokenStream {
                    ToTokens::into_token_stream(self).try_into().unwrap()
                }
            }

            #[doc(hidden)]
            #[allow(non_snake_case)]
            pub const fn $t1(marker: Marker) -> $t1 {
                match marker {}
            }
        )+

        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub(crate) enum PunctKind {
            $( $t1 ),+
        }

        impl TryFrom<char> for PunctKind {
            type Error = char;

            fn try_from(value: char) -> result::Result<Self, Self::Error> {
                match value {
                    $( $name1 => Ok(PunctKind::$t1), )+
                    _ => Err(value),
                }
            }
        }

        impl From<PunctKind> for char {
            fn from(value: PunctKind) -> char {
                match value {
                    $( PunctKind::$t1 => $name1, )+
                }
            }
        }

        $(
            #[derive(Debug, Clone)]
            #[doc = concat!("`", $name2, "`")]
            pub struct $t2 {
                /// The span covered by this token.
                pub span: Span,
            }

            impl $t2 {
                #[doc = concat!("Constructs a new `", stringify!($t2), "` with the given span.")]
                pub const fn new(span: Span) -> $t2 {
                    $t2 { span }
                }

                fn from_tokens_impl(input: ParseStream) -> Result<Self> {
                    if let Entry::Punct(SingleCharPunct { spacing: Spacing::Joint, .. }) = input.current()? {

                    } else {
                        return Err(Error::empty());
                    }

                    let start: $t21 = input.parse()?;
                    let end: $t22 = input.parse()?;
                    Ok(Self {
                        span: Span::new(start.span.start, end.span.end, start.span.source)
                    })
                }
            }

            impl Parse for $t2 {
                fn parse(input: ParseStream) -> Result<Self> {
                    let span = input.current()?.span();
                    Self::from_tokens_impl(input).map_err(|_| {
                        Error::new(Arc::clone(&input.source), ErrorKind::UnexpectedToken {
                            expected: HashSet::from_iter(vec![format!("'{}'", $name2)]),
                            span: span.clone(),
                        })
                    })
                }
            }

            impl Sealed for $t2 {}

            impl Token for $t2 {
                fn span(&self) -> &Span {
                    &self.span
                }

                fn set_span(&mut self, span: Span) {
                    self.span = span;
                }

                fn display() -> String {
                    $name2.to_string()
                }
            }

            impl fmt::Display for $t2 {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(f, $name2)
                }
            }

            impl Punct for $t2 {
                fn peek(input: ParseStream<'_>) -> bool {
                    input.peek($t2)
                }
            }

            impl PartialEq for $t2 {
                fn eq(&self, _other: &Self) -> bool {
                    true
                }
            }

            impl Eq for $t2 {}

            impl PartialOrd for $t2 {
                fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                    Some(self.cmp(other))
                }
            }

            impl Ord for $t2 {
                fn cmp(&self, _: &Self) -> Ordering {
                    Ordering::Equal
                }
            }

            impl hash::Hash for $t2 {
                fn hash<H: hash::Hasher>(&self, _: &mut H) {}
            }

            impl ToTokens for $t2 {
                fn to_tokens(&self, tokens: &mut TokenStream) {
                    let start_span = Span::new(
                        self.span.start,
                        self.span.start + 1,
                        Arc::clone(&self.span.source),
                    );
                    tokens.push(Entry::Punct(SingleCharPunct::new(
                        PunctKind::$t21,
                        Spacing::Joint,
                        start_span,
                    )));

                    let end_span = Span::new(
                        self.span.end - 1,
                        self.span.end,
                        Arc::clone(&self.span.source),
                    );
                    tokens.push(Entry::Punct(SingleCharPunct::new(
                        PunctKind::$t22,
                        Spacing::Alone,
                        end_span,
                    )));
                }
            }

            #[cfg(feature = "proc-macro2")]
            impl quote::ToTokens for $t2 {
                fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
                    use quote::TokenStreamExt as _;
                    tokens.append_all(proc_macro2::TokenStream::try_from(ToTokens::to_token_stream(self)).unwrap());
                }

                fn to_token_stream(&self) -> proc_macro2::TokenStream {
                    ToTokens::to_token_stream(self).try_into().unwrap()
                }

                fn into_token_stream(self) -> proc_macro2::TokenStream {
                    ToTokens::into_token_stream(self).try_into().unwrap()
                }
            }

            #[doc(hidden)]
            #[allow(non_snake_case)]
            pub const fn $t2(marker: Marker) -> $t2 {
                match marker {}
            }
        )+

        $(
            #[derive(Debug, Clone)]
            #[doc = concat!("`", $name3, "`")]
            pub struct $t3 {
                /// The span covered by this token.
                pub span: Span,
            }

            impl $t3 {
                #[doc = concat!("Constructs a new `", stringify!($t3), "` with the given span.")]
                pub const fn new(span: Span) -> $t3 {
                    $t3 { span }
                }

                fn from_tokens_impl(input: ParseStream) -> Result<Self> {
                    if let Entry::Punct(SingleCharPunct { spacing: Spacing::Joint, .. }) = input.current()? {

                    } else {
                        return Err(Error::empty());
                    }
                    let p1: $t31 = input.parse()?;
                    if let Entry::Punct(SingleCharPunct { spacing: Spacing::Joint, .. }) = input.current()? {

                    } else {
                        return Err(Error::empty());
                    }
                    let _p2: $t32 = input.parse()?;
                    let p3: $t33 = input.parse()?;
                    Ok(Self {
                        span: Span::new(p1.span.start, p3.span.end, p1.span.source)
                    })
                }
            }

            impl Parse for $t3 {
                fn parse(input: ParseStream) -> Result<Self> {
                    let span = input.current()?.span();
                    Self::from_tokens_impl(input).map_err(|_| {
                        Error::new(Arc::clone(&input.source), ErrorKind::UnexpectedToken {
                            expected: HashSet::from_iter(vec![format!("'{}'", $name3)]),
                            span: span.clone(),
                        })
                    })
                }
            }

            impl Sealed for $t3 {}

            impl Token for $t3 {
                fn span(&self) -> &Span {
                    &self.span
                }

                fn set_span(&mut self, span: Span) {
                    self.span = span;
                }

                fn display() -> String {
                    $name3.to_string()
                }
            }

            impl fmt::Display for $t3 {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(f, $name3)
                }
            }

            impl Punct for $t3 {
                fn peek(input: ParseStream<'_>) -> bool {
                    input.peek($t3)
                }
            }

            impl PartialEq for $t3 {
                fn eq(&self, _other: &Self) -> bool {
                    true
                }
            }

            impl Eq for $t3 {}

            impl PartialOrd for $t3 {
                fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                    Some(self.cmp(other))
                }
            }

            impl Ord for $t3 {
                fn cmp(&self, _: &Self) -> Ordering {
                    Ordering::Equal
                }
            }

            impl hash::Hash for $t3 {
                fn hash<H: hash::Hasher>(&self, _: &mut H) {}
            }

            impl ToTokens for $t3 {
                fn to_tokens(&self, tokens: &mut TokenStream) {
                    let start_span = Span::new(
                        self.span.start,
                        self.span.start + 1,
                        Arc::clone(&self.span.source),
                    );
                    tokens.push(Entry::Punct(SingleCharPunct::new(
                        PunctKind::$t31,
                        Spacing::Joint,
                        start_span,
                    )));

                    let mid_span = Span::new(
                        self.span.start + 1,
                        self.span.start + 2,
                        Arc::clone(&self.span.source),
                    );
                    tokens.push(Entry::Punct(SingleCharPunct::new(
                        PunctKind::$t32,
                        Spacing::Joint,
                        mid_span,
                    )));

                    let end_span = Span::new(
                        self.span.end - 1,
                        self.span.end,
                        Arc::clone(&self.span.source),
                    );
                    tokens.push(Entry::Punct(SingleCharPunct::new(
                        PunctKind::$t33,
                        Spacing::Alone,
                        end_span,
                    )));
                }
            }

            #[cfg(feature = "proc-macro2")]
            impl quote::ToTokens for $t3 {
                fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
                    use quote::TokenStreamExt as _;
                    tokens.append_all(proc_macro2::TokenStream::try_from(ToTokens::to_token_stream(self)).unwrap());
                }

                fn to_token_stream(&self) -> proc_macro2::TokenStream {
                    ToTokens::to_token_stream(self).try_into().unwrap()
                }

                fn into_token_stream(self) -> proc_macro2::TokenStream {
                    ToTokens::into_token_stream(self).try_into().unwrap()
                }
            }

            #[doc(hidden)]
            #[allow(non_snake_case)]
            pub const fn $t3(marker: Marker) -> $t3 {
                match marker {}
            }
        )+
    };
}

tokens! {
    {
        (Bang, '!')
        (Colon, ':')
        (Equal, '=')
        (SemiColon, ';')
        (LAngle, '<')
        (RAngle, '>')
        (Plus, '+')
        (Dash, '-')
        (Asterisk, '*')
        (Slash, '/')
        (Percent, '%')
        (Dot, '.')
        (Comma, ',')
        (LeftParen, '(')
        (RightParen, ')')
        (LeftBracket, '[')
        (RightBracket, ']')
        (LeftBrace, '{')
        (RightBrace, '}')
        (At, '@')
        (Caret, '^')
        (BackTick, '`')
        (Pipe, '|')
        (Ampersand, '&')
        (Tilde, '~')
        (Negate, '¬')
        (Backslash, '\\')
        (Question, '?')
        (Hash, '#')
        (Pound, '£')
        (Dollar, '$')
        (UnderScore, '_')
        (SingleQuote, '\'')
        (DoubleQuote, '"')
    }
    {
        (BangEqual, Bang, Equal, "!=")
        (EqualEqual, Equal, Equal, "==")
        (RAngleEqual, RAngle, Equal, ">=")
        (LAngleEqual, LAngle, Equal, "<=")
        (PlusEqual, Plus, Equal, "+=")
        (DashEqual, Dash, Equal, "-=")
        (AsteriskEqual, Asterisk, Equal, "*=")
        (SlashEqual, Slash, Equal, "/=")
        (PercentEqual, Percent, Equal, "%=")
        (LAngleLAngle, LAngle, LAngle, "<<")
        (RAngleRAngle, RAngle, RAngle, ">>")
        (LThinArrow, LAngle, Dash, "<-")
        (RThinArrow, Dash, RAngle, "->")
        (FatArrow, Equal, RAngle, "=>")
        (SlashSlash, Slash, Slash, "//")
        (ColonColon, Colon, Colon, "::")
        (HashHash, Hash, Hash, "##")
        (AmpersandAmpersand, Ampersand, Ampersand, "&&")
        (PipePipe, Pipe, Pipe, "||")
        (PlusPlus, Plus, Plus, "++")
        (DashDash, Dash, Dash, "--")
    }
    {
        (HashHashHash, Hash, Hash, Hash, "###")
        (SlashSlashEqual, Slash, Slash, Equal, "//=")
        (LAngleLAngleEqual, LAngle, LAngle, Equal, "<<=")
        (RAngleRAngleEqual, RAngle, RAngle, Equal, ">>=")
        (ColonColonEqual, Colon, Colon, Equal, "::=")
    }
}

trait JoinedPunct: Sized + fmt::Debug {
    fn display() -> String;

    fn parse(input: ParseStream) -> Result<Self>;
}

impl<T1: JoinedPunct, T2: JoinedPunct> JoinedPunct for (T1, T2) {
    fn display() -> String {
        T1::display() + &T2::display()
    }

    fn parse(input: ParseStream) -> Result<Self> {
        Ok((T1::parse(input)?, T2::parse(input)?))
    }
}

impl<T: Punct> JoinedPunct for (T,) {
    fn display() -> String {
        T::display()
    }

    fn parse(input: ParseStream) -> Result<Self> {
        T::parse(input).map(|value| (value,))
    }
}

impl<T: JoinedPunct> Parse for (T, Span) {
    fn parse(input: ParseStream) -> Result<Self> {
        let span = input.current()?.span();
        let value = T::parse(input).map_err(|_| {
            Error::new(
                Arc::clone(&input.source),
                ErrorKind::UnexpectedToken {
                    expected: HashSet::from_iter(vec![T::display()]),
                    span: span.clone(),
                },
            )
        })?;
        let span = Span::new(
            span.start,
            input.get_relative(-1)?.span().end,
            Arc::clone(&input.source),
        );
        Ok((value, span))
    }
}

impl<T: JoinedPunct> Sealed for (T, Span) {}

#[doc(hidden)]
impl<T: JoinedPunct> Token for (T, Span) {
    fn span(&self) -> &Span {
        &self.1
    }

    fn set_span(&mut self, span: Span) {
        self.1 = span;
    }

    fn display() -> String {
        T::display()
    }
}

impl<T: JoinedPunct> Punct for (T, Span) {
    fn peek(input: ParseStream) -> bool {
        input.parse_undo::<(T, Span)>().is_ok()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum WhiteSpace {
    Space2(Space2),
    Tab(Tab),
    NewLine(NewLine),
    CarriageReturn(CarriageReturn),
}

impl WhiteSpace {
    pub(crate) const fn span(&self) -> &Span {
        match self {
            WhiteSpace::Space2(Space2 { span })
            | WhiteSpace::Tab(Tab { span })
            | WhiteSpace::NewLine(NewLine { span })
            | WhiteSpace::CarriageReturn(CarriageReturn { span }) => span,
        }
    }

    #[cfg(feature = "proc-macro2")]
    pub(crate) fn set_span(&mut self, span: Span) {
        match self {
            WhiteSpace::Space2(Space2 {
                span: original_span,
            })
            | WhiteSpace::Tab(Tab {
                span: original_span,
            })
            | WhiteSpace::NewLine(NewLine {
                span: original_span,
            })
            | WhiteSpace::CarriageReturn(CarriageReturn {
                span: original_span,
            }) => *original_span = span,
        }
    }

    pub(crate) fn display(&self) -> String {
        match self {
            WhiteSpace::Space2(_) => Space2::display(),
            WhiteSpace::Tab(_) => Tab::display(),
            WhiteSpace::NewLine(_) => NewLine::display(),
            WhiteSpace::CarriageReturn(_) => CarriageReturn::display(),
        }
    }
}

/// `  `
#[derive(Debug, Clone)]
pub struct Space2 {
    /// The span covered by this token.
    pub span: Span,
}

impl Parse for Space2 {
    fn parse(input: ParseStream) -> Result<Self> {
        let token = input.next()?;
        if let Entry::WhiteSpace(WhiteSpace::Space2(value)) = token {
            Ok(value.clone())
        } else {
            Err(input.unexpected_token(HashSet::from_iter(["a two-space tab".to_string()])))
        }
    }
}

impl Sealed for Space2 {}

impl Token for Space2 {
    fn span(&self) -> &Span {
        &self.span
    }

    fn set_span(&mut self, span: Span) {
        self.span = span;
    }

    fn display() -> String {
        "  ".to_string()
    }
}

impl PartialEq for Space2 {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl Eq for Space2 {}

impl PartialOrd for Space2 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Space2 {
    fn cmp(&self, _: &Self) -> Ordering {
        Ordering::Equal
    }
}

impl hash::Hash for Space2 {
    fn hash<H: hash::Hasher>(&self, _: &mut H) {}
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub const fn Space2(marker: Marker) -> Space2 {
    match marker {}
}

/// `    `
#[derive(Debug, Clone)]
pub struct Space4 {
    /// The span covered by this token.
    pub span: Span,
}

impl Space4 {
    fn parse_impl(input: ParseStream) -> Result<Self> {
        let start: Space2 = input.parse()?;
        let end: Space2 = input.parse()?;
        if start.span.end != end.span.start {
            return Err(Error::empty());
        }
        Ok(Space4 {
            span: Span::across(start.span(), end.span()).unwrap(),
        })
    }
}

impl Parse for Space4 {
    fn parse(input: ParseStream) -> Result<Self> {
        Self::parse_impl(input).map_err(|_| {
            input.unexpected_token(HashSet::from_iter(["a four-space tab".to_string()]))
        })
    }
}

impl Sealed for Space4 {}

impl Token for Space4 {
    fn span(&self) -> &Span {
        &self.span
    }

    fn set_span(&mut self, span: Span) {
        self.span = span;
    }

    fn display() -> String {
        "    ".to_string()
    }
}

impl PartialEq for Space4 {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl Eq for Space4 {}

impl PartialOrd for Space4 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Space4 {
    fn cmp(&self, _: &Self) -> Ordering {
        Ordering::Equal
    }
}

impl hash::Hash for Space4 {
    fn hash<H: hash::Hasher>(&self, _: &mut H) {}
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub const fn Space4(marker: Marker) -> Space4 {
    match marker {}
}

/// `\t`
#[derive(Debug, Clone)]
pub struct Tab {
    /// The span covered by this token.
    pub span: Span,
}

impl Parse for Tab {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Entry::WhiteSpace(WhiteSpace::Tab(value)) = input.next()? {
            Ok(value.clone())
        } else {
            Err(input.unexpected_token(HashSet::from_iter(["a tab".to_string()])))
        }
    }
}

impl Sealed for Tab {}

impl Token for Tab {
    fn span(&self) -> &Span {
        &self.span
    }

    fn set_span(&mut self, span: Span) {
        self.span = span;
    }

    fn display() -> String {
        "\t".to_string()
    }
}

impl PartialEq for Tab {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl Eq for Tab {}

impl PartialOrd for Tab {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Tab {
    fn cmp(&self, _: &Self) -> Ordering {
        Ordering::Equal
    }
}

impl hash::Hash for Tab {
    fn hash<H: hash::Hasher>(&self, _: &mut H) {}
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub const fn Tab(marker: Marker) -> Tab {
    match marker {}
}

/// `\n`
#[derive(Debug, Clone)]
pub struct NewLine {
    /// The span covered by this token.
    pub span: Span,
}

impl Parse for NewLine {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Entry::WhiteSpace(WhiteSpace::NewLine(value)) = input.next()? {
            Ok(value.clone())
        } else {
            Err(input.unexpected_token(HashSet::from_iter(["\\n".to_string()])))
        }
    }
}

impl Sealed for NewLine {}

impl Token for NewLine {
    fn span(&self) -> &Span {
        &self.span
    }

    fn set_span(&mut self, span: Span) {
        self.span = span;
    }

    fn display() -> String {
        "\\n".to_string()
    }
}

impl PartialEq for NewLine {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl Eq for NewLine {}

impl PartialOrd for NewLine {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for NewLine {
    fn cmp(&self, _: &Self) -> Ordering {
        Ordering::Equal
    }
}

impl hash::Hash for NewLine {
    fn hash<H: hash::Hasher>(&self, _: &mut H) {}
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub const fn NewLine(marker: Marker) -> NewLine {
    match marker {}
}

/// `u+000D`
#[derive(Debug, Clone)]
pub struct CarriageReturn {
    /// The span covered by this token.
    pub span: Span,
}

impl Parse for CarriageReturn {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Entry::WhiteSpace(WhiteSpace::CarriageReturn(value)) = input.next()? {
            Ok(value.clone())
        } else {
            Err(input.unexpected_token(HashSet::from_iter(["a carriage return".to_string()])))
        }
    }
}

impl Sealed for CarriageReturn {}

impl Token for CarriageReturn {
    fn span(&self) -> &Span {
        &self.span
    }

    fn set_span(&mut self, span: Span) {
        self.span = span;
    }

    fn display() -> String {
        "a carriage return".to_string()
    }
}

impl PartialEq for CarriageReturn {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl Eq for CarriageReturn {}

impl PartialOrd for CarriageReturn {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CarriageReturn {
    fn cmp(&self, _: &Self) -> Ordering {
        Ordering::Equal
    }
}

impl hash::Hash for CarriageReturn {
    fn hash<H: hash::Hasher>(&self, _: &mut H) {}
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub const fn CarriageReturn(marker: Marker) -> CarriageReturn {
    match marker {}
}

/// Generate types for keywords.
///
/// ## Usage
/// ```
/// use flexi_parse::parse_string;
/// mod kw {
///     use flexi_parse::keywords;
///     keywords![var, then];
/// }
///
/// # fn main() {
/// let kw1: kw::var = parse_string("var".to_string()).unwrap();
/// let kw2: kw::then = parse_string("then".to_string()).unwrap();
/// # }
/// ```
#[macro_export]
macro_rules! keywords {
    [ $( $kw:ident $(as $name:ident)? ),+ $(,)? ] => {
        $(
            $crate::keywords!(@ $kw $(as $name)?);
        )+

        /// Parses non-keyword identifiers.
        #[allow(dead_code)]
        pub fn ident(input: $crate::ParseStream) -> $crate::Result<$crate::token::Ident> {
            let ident: $crate::token::Ident = input.parse()?;
            if [$( ::core::stringify!($kw) ),+].contains(&ident.string().as_str()) {
                $crate::Result::Err(input.unexpected_token(
                    ::std::collections::HashSet::from_iter(["an identifier".to_string()]),
                ))
            } else {
                $crate::Result::Ok(ident)
            }
        }
    };
    (@ $kw:ident) => {
        $crate::keywords!(@ $kw as $kw);
    };
    (@ $kw:ident as $name:ident) => {
        #[derive(Debug, Clone)]
        #[allow(non_camel_case_types)]
        pub struct $name {
            ident: $crate::token::Ident
        }

        impl $name {
            #[allow(dead_code)]
            pub fn new(input: $crate::ParseStream) -> Self {
                Self {
                    ident: $crate::token::Ident::new(::core::stringify!($kw).to_string(), input.empty_span()),
                }
            }

            #[allow(dead_code)]
            pub const fn ident(&self) -> &$crate::token::Ident {
                &self.ident
            }
        }

        impl $crate::Parse for $name {
            fn parse(input: $crate::ParseStream) -> $crate::Result<Self> {
                let ident: $crate::token::Ident = $crate::Parse::parse(input)?;
                if ident.string() == ::core::stringify!($kw) {
                    $crate::Result::Ok(Self {
                        ident
                    })
                } else {
                    $crate::Result::Err(input.unexpected_token(
                        ::std::collections::HashSet::from_iter([::core::stringify!($kw).to_string()]),
                    ))
                }
            }
        }

        impl $crate::private::Sealed for $name {}

        impl $crate::token::Token for $name {
            fn span(&self) -> &$crate::Span {
                self.ident.span()
            }

            fn set_span(&mut self, span: $crate::Span) {
                self.ident.set_span(span);
            }

            fn display() -> String {
                ::core::stringify!($kw).to_string()
            }
        }

        impl ::core::cmp::PartialEq for $name {
            fn eq(&self, _: &Self) -> bool {
                true
            }
        }

        impl ::core::cmp::Eq for $name {}

        impl ::core::cmp::PartialOrd for $name {
            fn partial_cmp(&self, other: &Self) -> ::core::option::Option<::core::cmp::Ordering> {
                ::core::option::Option::Some(::core::cmp::Ord::cmp(self, other))
            }
        }

        impl ::core::cmp::Ord for $name {
            fn cmp(&self, _: &Self) -> ::core::cmp::Ordering {
                ::core::cmp::Ordering::Equal
            }
        }

        #[doc(hidden)]
        #[allow(dead_code)]
        pub const fn $name(marker: $crate::private::Marker) -> $name {
            match marker {}
        }
    };
}

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
    ["=="] => { $crate::token::EqualEqual };
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
