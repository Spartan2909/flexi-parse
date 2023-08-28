//! Tokens representing punctuation, identifiers, keywords, and whitespace.
//!
//! Types for parsing delimited groups can be found in the
//! [`group`](crate::group) module.
//!
//! The punctuation tokens can be most easily accessed using the
//! [`Punct`](crate::Punct) macro.

use crate::error::Error;
use crate::error::ErrorKind;
use crate::group::delimited_string;
use crate::group::DoubleQuotes;
use crate::group::Group;
use crate::group::SingleQuotes;
use crate::private::Sealed;
use crate::Entry;
use crate::Marker;
use crate::Parse;
use crate::ParseStream;
use crate::Result;
use crate::Span;

use std::cmp::Ordering;
use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;
use std::result;

#[doc(hidden)]
pub use concat_idents::concat_idents;

/// A trait for types that can be represented by a single token.
///
/// This trait is sealed, and cannot be implemented by types outside of this
/// crate.
pub trait Token: Parse + Sealed {
    /// Returns the span covered by this token.
    fn span(&self) -> &Span;

    #[doc(hidden)]
    fn set_span(&mut self, span: Span);

    #[doc(hidden)]
    fn display() -> String;
}

impl<T: Token> Parse for Option<T> {
    fn parse(input: ParseStream) -> Result<Self> {
        match input.try_parse() {
            Ok(value) => Ok(Some(value)),
            Err(_) => Ok(None),
        }
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
    pub fn string(&self) -> &String {
        &self.string
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

impl Parse for LitStrDoubleQuote {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let (start, end) = delimited_string::<DoubleQuotes>(input)?;
        let (start, end) = (start.span(), end.span());
        let string = input.source.contents[start.end..end.start].to_owned();
        let span = Span::across(start, end);
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
pub fn LitStrDoubleQuote(marker: Marker) -> LitStrDoubleQuote {
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
    pub fn string(&self) -> &String {
        &self.string
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

impl Parse for LitStrSingleQuote {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let (start, end) = delimited_string::<SingleQuotes>(input)?;
        let (start, end) = (start.span(), end.span());
        let string = input.source.contents[start.end..end.start].to_owned();
        let span = Span::across(start, end);
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
pub fn LitStrSingleQuote(marker: Marker) -> LitStrSingleQuote {
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
    pub fn ch(&self) -> char {
        self.ch
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

impl Parse for LitChar {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let group: Group<SingleQuotes> = input.parse().map_err(|mut err| {
            err.group_to_char();
            err
        })?;
        let span = group.span;
        let string = group.token_stream.to_string();
        if string.len() != 1 {
            return Err(Error::new(
                Rc::clone(&input.source),
                ErrorKind::LongChar(span),
            ));
        }
        let ch = string.chars().next().unwrap();
        Ok(LitChar { ch, span })
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
pub fn LitChar(marker: Marker) -> LitChar {
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
    pub fn string(&self) -> &String {
        &self.string
    }

    /// Creates a new identifier in the given [`ParseStream`] with the given
    /// string.
    pub fn new(string: String, span: Span) -> Ident {
        Ident { string, span }
    }

    fn parse_simple(input: ParseStream<'_>) -> Result<Self> {
        let token = input.next()?;
        if let Ok(ident) = Self::try_from(token.to_owned()) {
            Ok(ident)
        } else {
            Err(Error::new(
                Rc::clone(&input.source),
                ErrorKind::UnexpectedToken {
                    expected: HashSet::from_iter(["an identifier".to_string()]),
                    span: token.span().clone(),
                },
            ))
        }
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.string == other.string
    }
}

impl Eq for Ident {}

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
                Rc::clone(&input.source),
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
pub fn Ident(marker: Marker) -> Ident {
    match marker {}
}

#[derive(Debug, Clone)]
pub(crate) struct SingleCharPunct {
    pub(super) kind: PunctKind,
    pub(super) spacing: Spacing,
    pub(super) span: Span,
}

impl PartialEq for SingleCharPunct {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.spacing == other.spacing
    }
}

impl Eq for SingleCharPunct {}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Spacing {
    Alone,
    Joint,
}

impl From<Result<char>> for Spacing {
    fn from(value: Result<char>) -> Self {
        if let Ok(c) = value {
            if PunctKind::try_from(c).is_ok() {
                Spacing::Joint
            } else {
                Spacing::Alone
            }
        } else {
            Spacing::Alone
        }
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
    pub fn value(&self) -> u64 {
        self.value
    }

    fn parse_decimal_impl(input: ParseStream<'_>) -> Result<Self> {
        let ident = Ident::parse_simple(input)?;
        Ok(LitInt {
            value: ident.string.parse().map_err(|_| Error::empty())?,
            span: ident.span,
        })
    }

    /// Accepts a string of ascii digits.
    pub fn parse_decimal(input: ParseStream<'_>) -> Result<Self> {
        Self::parse_decimal_impl(input).map_err(|_| {
            input.unexpected_token(HashSet::from_iter(["an integer literal".to_string()]))
        })
    }

    fn parse_impl(input: ParseStream<'_>) -> Result<Self> {
        let ident = Ident::parse_simple(input)?;
        let mut chars = ident.string().chars();
        let start: u8 = chars
            .next()
            .unwrap()
            .try_into()
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

impl Parse for LitInt {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        if let Ok(lit) = Self::parse_impl(input) {
            Ok(lit)
        } else {
            Err(input.unexpected_token(HashSet::from_iter(["an integer literal".to_string()])))
        }
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
    let mut value = int as f64;
    while value >= 1.0 {
        value /= 10.0;
    }
    value
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub fn LitInt(marker: Marker) -> LitInt {
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
    pub fn value(&self) -> f64 {
        self.value
    }

    fn parse_impl(input: ParseStream<'_>) -> Result<Self> {
        let start = LitInt::parse_decimal(input)?;
        let _: Dot = input.parse()?;
        let end = LitInt::parse_decimal(input)?;
        Ok(LitFloat {
            value: start.value as f64 + int_to_decimal(end.value),
            span: Span::new(start.span.start, end.span.end, Rc::clone(&input.source)),
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
        if let Ok(value) = Self::parse_impl(input) {
            Ok(value)
        } else {
            Err(input.unexpected_token(HashSet::from_iter(["a float literal".to_string()])))
        }
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
pub fn LitFloat(marker: Marker) -> LitFloat {
    match marker {}
}

macro_rules! tokens {
    {
        {
            $( ($t1:ident, $name1:literal, $doc1:literal) )+
        }
        {
            $( ($t2:ident, $t21:ident, $t22:ident, $name2:literal, $doc2:literal) )+
        }
        {
            $( ($t3:ident, $t31:ident, $t32:ident, $t33:ident, $name3:literal, $doc3:literal) )+
        }
    } => {
        $(
            #[derive(Debug, Clone)]
            #[doc = $doc1]
            pub struct $t1 {
                /// The span covered by this token.
                pub span: Span,
            }

            impl Parse for $t1 {
                fn parse(input: ParseStream<'_>) -> Result<Self> {
                    let token = input.next()?.to_owned();
                    if let Entry::Punct(SingleCharPunct { kind: PunctKind::$t1, span, .. }) = token {
                        Ok(Self { span })
                    } else {
                        Err(Error::new(Rc::clone(&input.source), ErrorKind::UnexpectedToken {
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

            #[doc(hidden)]
            #[allow(non_snake_case)]
            pub fn $t1(marker: Marker) -> $t1 {
                match marker {}
            }
        )+

        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
            #[doc = $doc2]
            pub struct $t2 {
                /// The span covered by this token.
                pub span: Span,
            }

            impl $t2 {
                fn from_tokens_impl(input: ParseStream<'_>) -> Result<Self> {
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
                fn parse(input: ParseStream<'_>) -> Result<Self> {
                    let span = input.current()?.span();
                    Self::from_tokens_impl(input).map_err(|_| {
                        Error::new(Rc::clone(&input.source), ErrorKind::UnexpectedToken {
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

            #[doc(hidden)]
            #[allow(non_snake_case)]
            pub fn $t2(marker: Marker) -> $t2 {
                match marker {}
            }
        )+

        $(
            #[derive(Debug, Clone)]
            #[doc = $doc3]
            pub struct $t3 {
                /// The span covered by this token.
                pub span: Span,
            }

            impl $t3 {
                fn from_tokens_impl(input: ParseStream<'_>) -> Result<Self> {
                    if let Entry::Punct(SingleCharPunct { spacing: Spacing::Joint, .. }) = input.current()? {

                    } else {
                        return Err(Error::empty());
                    }
                    if let Entry::Punct(SingleCharPunct { spacing: Spacing::Joint, .. }) = input.current()? {

                    } else {
                        return Err(Error::empty());
                    }
                    let p1: $t31 = input.parse()?;
                    let _p2: $t32 = input.parse()?;
                    let p3: $t33 = input.parse()?;
                    Ok(Self {
                        span: Span::new(p1.span.start, p3.span.end, p1.span.source)
                    })
                }
            }

            impl Parse for $t3 {
                fn parse(input: ParseStream<'_>) -> Result<Self> {
                    let span = input.current()?.span();
                    Self::from_tokens_impl(input).map_err(|_| {
                        Error::new(Rc::clone(&input.source), ErrorKind::UnexpectedToken {
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

            #[doc(hidden)]
            #[allow(non_snake_case)]
            pub fn $t3(marker: Marker) -> $t3 {
                match marker {}
            }
        )+
    };
}

tokens! {
    {
        (Bang, '!', "`!`")
        (Colon, ':', "`:`")
        (Equal, '=', "`=`")
        (SemiColon, ';', "`;`")
        (LAngle, '<', "`<`")
        (RAngle, '>', "`>`")
        (Plus, '+', "`+`")
        (Dash, '-', "`-`")
        (Asterisk, '*', "`*`")
        (Slash, '/', "`/`")
        (Percent, '%', "`%`")
        (Dot, '.', "`.`")
        (Comma, ',', "`,`")
        (LeftParen, '(', "`(`")
        (RightParen, ')', "`)`")
        (LeftBracket, '[', "`[`")
        (RightBracket, ']', "`]`")
        (LeftBrace, '{', "`{`")
        (RightBrace, '}', "`}`")
        (At, '@', "`@`")
        (Caret, '^', "`^`")
        (BackTick, '`', "`` ` ``")
        (Pipe, '|', "`|`")
        (Ampersand, '&', "`&`")
        (Tilde, '~', "`~`")
        (Tilde2, '¬', "`¬`")
        (Backslash, '\\', "`\\`")
        (Question, '?', "`?`")
        (Hash, '#', "`#`")
        (Pound, '£', "`£`")
        (Dollar, '$', "`$`")
        (UnderScore, '_', "`_`")
        (SingleQuote, '\'', "`'`")
        (DoubleQuote, '"', "`\"`")
    }
    {
        (BangEqual, Bang, Equal, "!=", "`!=`")
        (EqualEqual, Equal, Equal, "==", "`==`")
        (RAngleEqual, RAngle, Equal, ">=", "`>=`")
        (LAngleEqual, LAngle, Equal, "<=", "`<=`")
        (PlusEqual, Plus, Equal, "+=", "`+=`")
        (DashEqual, Dash, Equal, "-=", "`-=`")
        (AsteriskEqual, Asterisk, Equal, "*=", "`*=`")
        (SlashEqual, Slash, Equal, "/=", "`/=`")
        (PercentEqual, Percent, Equal, "%=", "`%=`")
        (LAngleLAngle, LAngle, LAngle, "<<", "`<<`")
        (RAngleRAngle, RAngle, RAngle, ">>", "`>>`")
        (LThinArrow, LAngle, Dash, "<-", "`<-`")
        (RThinArrow, Dash, RAngle, "->", "`->`")
        (FatArrow, Equal, RAngle, "=>", "`=>`")
        (SlashSlash, Slash, Slash, "//", "`//`")
        (ColonColon, Colon, Colon, "::", "`::`")
        (HashHash, Hash, Hash, "##", "`##`")
        (AmpersandAmpersand, Ampersand, Ampersand, "&&", "`&&`")
        (PipePipe, Pipe, Pipe, "||", "`||`")
        (PlusPlus, Plus, Plus, "++", "`++`")
        (DashDash, Dash, Dash, "--", "`--`")
    }
    {
        (HashHashHash, Hash, Hash, Hash, "###", "`###`")
        (SlashSlashEqual, Slash, Slash, Equal, "//=", "`//=`")
        (LAngleLAngleEqual, LAngle, LAngle, Equal, "<<=", "`<<=`")
        (RAngleRAngleEqual, RAngle, RAngle, Equal, ">>=", "`>>=`")
        (ColonColonEqual, Colon, Colon, Equal, "::=", "`::=`")
    }
}

trait JoinedPunct: Sized {
    fn display() -> String;

    fn parse(input: ParseStream<'_>) -> Result<Self>;
}

impl<T1: JoinedPunct, T2: JoinedPunct> JoinedPunct for (T1, T2) {
    fn display() -> String {
        T1::display() + &T2::display()
    }

    fn parse(input: ParseStream<'_>) -> Result<Self> {
        Ok((T1::parse(input)?, T2::parse(input)?))
    }
}

impl<T: Punct> JoinedPunct for (T,) {
    fn display() -> String {
        T::display()
    }

    fn parse(input: ParseStream<'_>) -> Result<Self> {
        T::parse(input).map(|value| (value,))
    }
}

impl<T: JoinedPunct> Parse for (T, Span) {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let span = input.current()?.span();
        let value = T::parse(input).map_err(|_| {
            Error::new(
                Rc::clone(&input.source),
                ErrorKind::UnexpectedToken {
                    expected: HashSet::from_iter(vec![T::display()]),
                    span: span.clone(),
                },
            )
        })?;
        let span = Span::new(
            span.start,
            input.get_relative(-1)?.span().end,
            Rc::clone(&input.source),
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
    fn peek(input: ParseStream<'_>) -> bool {
        input.parse_undo::<(T, Span)>().is_ok()
    }
}

#[derive(Debug, Clone)]
pub(crate) enum WhiteSpace {
    Space2(Space2),
    Tab(Tab),
    NewLine(NewLine),
    CarriageReturn(CarriageReturn),
}

impl WhiteSpace {
    pub(crate) fn span(&self) -> &Span {
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

impl PartialEq for WhiteSpace {
    fn eq(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (WhiteSpace::Space2(_), WhiteSpace::Space2(_))
                | (WhiteSpace::Tab(_), WhiteSpace::Tab(_))
                | (WhiteSpace::NewLine(_), WhiteSpace::NewLine(_))
                | (WhiteSpace::CarriageReturn(_), WhiteSpace::CarriageReturn(_))
        )
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

#[doc(hidden)]
#[allow(non_snake_case)]
pub fn Space2(marker: Marker) -> Space2 {
    match marker {}
}

/// `    `
#[derive(Debug, Clone)]
pub struct Space4 {
    /// The span covered by this token.
    pub span: Span,
}

impl Space4 {
    fn parse_impl(input: ParseStream<'_>) -> Result<Self> {
        let start: Space2 = input.parse()?;
        let end: Space2 = input.parse()?;
        if start.span.end != end.span.start {
            return Err(Error::empty());
        }
        Ok(Space4 {
            span: Span::across(start.span(), end.span()),
        })
    }
}

impl Parse for Space4 {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
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

#[doc(hidden)]
#[allow(non_snake_case)]
pub fn Space4(marker: Marker) -> Space4 {
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

#[doc(hidden)]
#[allow(non_snake_case)]
pub fn Tab(marker: Marker) -> Tab {
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

#[doc(hidden)]
#[allow(non_snake_case)]
pub fn NewLine(marker: Marker) -> NewLine {
    match marker {}
}

/// `u+000D`
#[derive(Debug, Clone)]
pub struct CarriageReturn {
    /// The span covered by this token.
    pub span: Span,
}

impl Parse for CarriageReturn {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
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

#[doc(hidden)]
#[allow(non_snake_case)]
pub fn CarriageReturn(marker: Marker) -> CarriageReturn {
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
    [ $( $kw:ident ),+ $(,)? ] => {
        use $crate::token::Token as _;
        $(
            #[derive(Debug, Clone)]
            #[allow(non_camel_case_types)]
            pub struct $kw {
                ident: $crate::token::Ident
            }

            impl $kw {
                #[allow(dead_code)]
                pub fn new(input: $crate::ParseStream<'_>) -> Self {
                    Self {
                        ident: $crate::token::Ident::new(stringify!($kw).to_string(), input),
                    }
                }

                #[allow(dead_code)]
                pub fn ident(&self) -> &$crate::token::Ident {
                    &self.ident
                }
            }

            impl $crate::Parse for $kw {
                fn parse(input: $crate::ParseStream<'_>) -> $crate::Result<Self> {
                    let ident: $crate::token::Ident = input.parse()?;
                    if ident.string() == stringify!($kw) {
                        $crate::Result::Ok(Self {
                            ident: ident
                        })
                    } else {
                        $crate::Result::Err(input.unexpected_token(
                            ::std::collections::HashSet::from_iter([stringify!($kw).to_string()]),
                        ))
                    }
                }
            }

            impl $crate::private::Sealed for $kw {}

            impl $crate::token::Token for $kw {
                fn span(&self) -> &$crate::Span {
                    self.ident.span()
                }

                fn set_span(&mut self, span: $crate::Span) {
                    self.ident.set_span(span);
                }

                fn display() -> String {
                    stringify!($kw).to_string()
                }
            }

            impl ::std::cmp::PartialEq for $kw {
                fn eq(&self, _other: &Self) -> bool {
                    true
                }
            }

            impl ::std::cmp::Eq for $kw {}

            #[doc(hidden)]
            #[allow(dead_code)]
            pub fn $kw(marker: $crate::Marker) -> $kw {
                match marker {}
            }
        )+

        /// Parses non-keyword identifiers.
        #[allow(dead_code)]
        pub fn ident(input: $crate::ParseStream<'_>) -> $crate::Result<$crate::token::Ident> {
            let ident: $crate::token::Ident = input.parse()?;
            if [$( stringify!($kw) ),+].contains(&ident.string().as_str()) {
                $crate::Result::Err(input.unexpected_token(
                    ::std::collections::HashSet::from_iter(["an identifier".to_string()]),
                ))
            } else {
                $crate::Result::Ok(ident)
            }
        }
    };
}

pub use keywords;

/// Generate types for keywords, with the type names prefixed with `keyword_`.
///
/// ## Usage
/// ```
/// use flexi_parse::parse_string;
/// mod kw {
///     use flexi_parse::keywords_prefixed;
///     keywords_prefixed!["let", "if"];
/// }
///
/// # fn main() {
/// let kw1: kw::keyword_let = parse_string("let".to_string()).unwrap();
/// let kw2: kw::keyword_if = parse_string("if".to_string()).unwrap();
/// # }
/// ```
#[macro_export]
macro_rules! keywords_prefixed {
    [ $( $kw:tt ),+ $(,)? ] => {
        $(
            $crate::token::concat_idents!(struct_name = keyword_, $kw {
                #[derive(Debug, Clone)]
                #[allow(non_camel_case_types)]
                pub struct struct_name {
                    ident: $crate::token::Ident
                }

                impl struct_name {
                    #[allow(dead_code)]
                    pub fn new(input: $crate::ParseStream<'_>) -> Self {
                        Self {
                            ident: $crate::token::Ident::new($kw.to_string(), input.empty_span()),
                        }
                    }

                    #[allow(dead_code)]
                    pub fn ident(&self) -> &$crate::token::Ident {
                        &self.ident
                    }
                }

                impl $crate::Parse for struct_name {
                    fn parse(input: $crate::ParseStream<'_>) -> $crate::Result<Self> {
                        let ident: $crate::token::Ident = input.parse()?;
                        if ident.string() == $kw {
                            $crate::Result::Ok(Self {
                                ident,
                            })
                        } else {
                            $crate::Result::Err(input.unexpected_token(
                                ::std::collections::HashSet::from_iter([$kw.to_string()]),
                            ))
                        }
                    }
                }

                impl $crate::private::Sealed for struct_name {}

                impl $crate::token::Token for struct_name {
                    fn span(&self) -> &$crate::Span {
                        self.ident.span()
                    }

                    fn set_span(&mut self, span: $crate::Span) {
                        self.ident.set_span(span);
                    }

                    fn display() -> String {
                        $kw.to_string()
                    }
                }

                impl ::std::cmp::PartialEq for struct_name {
                    fn eq(&self, _other: &Self) -> bool {
                        true
                    }
                }

                #[doc(hidden)]
                #[allow(dead_code)]
                pub fn struct_name(marker: $crate::Marker) -> struct_name {
                    match marker {}
                }
            });
        )+

        /// Parses non-keyword identifiers.
        #[allow(dead_code)]
        pub fn ident(input: $crate::ParseStream<'_>) -> $crate::Result<$crate::token::Ident> {
            let ident: $crate::token::Ident = input.parse()?;
            if [$( $kw ),+].contains(&ident.string().as_str()) {
                $crate::Result::Err(input.unexpected_token(
                    ::std::collections::HashSet::from_iter(["an identifier".to_string()]),
                ))
            } else {
                $crate::Result::Ok(ident)
            }
        }
    };
}

pub use keywords_prefixed;
