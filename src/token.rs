//! Tokens representing punctuation, identifiers, keywords, and whitespace.
//!
//! Types for parsing delimited groups can be found in the
//! [`group`](crate::group) module.
//!
//! The punctuation tokens can be most easily accessed using the
//! [`Punct`](crate::Punct) macro.

use crate::error::Error;
use crate::error::ErrorKind;
use crate::group::DoubleQuotes;
use crate::group::Group;
use crate::group::SingleQuotes;
use crate::private::Sealed;
use crate::Entry;
use crate::Parse;
use crate::ParseStream;
use crate::Result;
use crate::Span;

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

/// A trait for punctuation tokens.
///
/// This trait is sealed, and cannot be implemented by types outside of this
/// crate.
pub trait Punct: Token {}

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

impl Parse for LitStrDoubleQuote {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let group: Group<DoubleQuotes> = input.parse().map_err(|mut err| {
            err.group_to_string();
            err
        })?;
        let string = group.token_stream.to_string();
        let span = group.span;
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

impl Parse for LitStrSingleQuote {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let group: Group<SingleQuotes> = input.parse().map_err(|mut err| {
            err.group_to_string();
            err
        })?;
        let string = group.token_stream.to_string();
        let span = group.span;
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

#[derive(Debug, Clone, Copy, PartialEq)]
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
        Self::parse_decimal_impl(input)
            .map_err(|_| input.error(HashSet::from_iter(["an integer literal".to_string()])))
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

impl Parse for LitInt {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        if let Ok(lit) = Self::parse_impl(input) {
            Ok(lit)
        } else {
            Err(input.error(HashSet::from_iter(["an integer literal".to_string()])))
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

impl Parse for LitFloat {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        if let Ok(value) = Self::parse_impl(input) {
            Ok(value)
        } else {
            Err(input.error(HashSet::from_iter(["a float literal".to_string()])))
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
            pub struct $t1(Span);

            impl Parse for $t1 {
                fn parse(input: ParseStream<'_>) -> Result<Self> {
                    let token = input.next()?.to_owned();
                    if let Entry::Punct(SingleCharPunct { kind: PunctKind::$t1, span, .. }) = token {
                        Ok(Self(span))
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
                    &self.0
                }

                fn set_span(&mut self, span: Span) {
                    self.0 = span;
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

            impl Punct for $t1 {}
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
            pub struct $t2(Span);

            impl $t2 {
                fn from_tokens_impl(input: ParseStream<'_>) -> Result<Self> {
                    if let Entry::Punct(SingleCharPunct { spacing: Spacing::Joint, .. }) = input.current()?.1 {

                    } else {
                        return Err(Error::empty());
                    }

                    let start: $t21 = input.parse()?;
                    let end: $t22 = input.parse()?;
                    Ok(Self(Span::new(start.0.start, end.0.end, start.0.source)))
                }
            }

            impl Parse for $t2 {
                fn parse(input: ParseStream<'_>) -> Result<Self> {
                    let span = input.current()?.1.span();
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
                    &self.0
                }

                fn set_span(&mut self, span: Span) {
                    self.0 = span;
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

            impl Punct for $t2 {}
        )+

        $(
            #[derive(Debug, Clone)]
            #[doc = $doc3]
            pub struct $t3(Span);

            impl $t3 {
                fn from_tokens_impl(input: ParseStream<'_>) -> Result<Self> {
                    if let Entry::Punct(SingleCharPunct { spacing: Spacing::Joint, .. }) = input.current()?.1 {

                    } else {
                        return Err(Error::empty());
                    }
                    if let Entry::Punct(SingleCharPunct { spacing: Spacing::Joint, .. }) = input.current()?.1 {

                    } else {
                        return Err(Error::empty());
                    }
                    let p1: $t31 = input.parse()?;
                    let _p2: $t32 = input.parse()?;
                    let p3: $t33 = input.parse()?;
                    Ok(Self(Span::new(p1.0.start, p3.0.end, p1.0.source)))
                }
            }

            impl Parse for $t3 {
                fn parse(input: ParseStream<'_>) -> Result<Self> {
                    let span = input.current()?.1.span();
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
                    &self.0
                }

                fn set_span(&mut self, span: Span) {
                    self.0 = span;
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

            impl Punct for $t3 {}
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
        let span = input.current()?.1.span();
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
            input.get_relative(-1)?.1.span().end,
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

impl<T: JoinedPunct> Punct for (T, Span) {}

#[derive(Debug, Clone)]
pub(crate) enum WhiteSpace {
    Space2(Space2),
    Space4(Space4),
    Tab(Tab),
    NewLine(NewLine),
}

impl WhiteSpace {
    pub(crate) fn span(&self) -> &Span {
        match self {
            WhiteSpace::Space2(Space2(span))
            | WhiteSpace::Space4(Space4(span))
            | WhiteSpace::Tab(Tab(span))
            | WhiteSpace::NewLine(NewLine(span)) => span,
        }
    }

    #[cfg(feature = "proc-macro2")]
    pub(crate) fn set_span(&mut self, span: Span) {
        match self {
            WhiteSpace::Space2(Space2(original_span))
            | WhiteSpace::Space4(Space4(original_span))
            | WhiteSpace::Tab(Tab(original_span))
            | WhiteSpace::NewLine(NewLine(original_span)) => *original_span = span,
        }
    }

    pub(crate) fn display(&self) -> String {
        match self {
            WhiteSpace::Space2(_) => Space2::display(),
            WhiteSpace::Space4(_) => Space4::display(),
            WhiteSpace::Tab(_) => Tab::display(),
            WhiteSpace::NewLine(_) => NewLine::display(),
        }
    }
}

impl PartialEq for WhiteSpace {
    fn eq(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (WhiteSpace::Space2(_), WhiteSpace::Space2(_))
                | (WhiteSpace::Space4(_), WhiteSpace::Space4(_))
                | (WhiteSpace::Tab(_), WhiteSpace::Tab(_))
        )
    }
}

/// ` `
#[derive(Debug, Clone)]
pub struct Space(Span);

/// `  `
#[derive(Debug, Clone)]
pub struct Space2(pub(crate) Span);

impl Parse for Space2 {
    fn parse(input: ParseStream) -> Result<Self> {
        let token = input.next()?;
        if let Entry::WhiteSpace(WhiteSpace::Space2(value)) = token {
            Ok(value.clone())
        } else {
            Err(input.error(HashSet::from_iter(["a two-space tab".to_string()])))
        }
    }
}

impl Sealed for Space2 {}

impl Token for Space2 {
    fn span(&self) -> &Span {
        &self.0
    }

    fn set_span(&mut self, span: Span) {
        self.0 = span;
    }

    fn display() -> String {
        "  ".to_string()
    }
}

/// `    `
#[derive(Debug, Clone)]
pub struct Space4(pub(crate) Span);

impl Parse for Space4 {
    fn parse(input: ParseStream) -> Result<Self> {
        let token = input.next()?;
        if let Entry::WhiteSpace(WhiteSpace::Space4(value)) = token {
            Ok(value.clone())
        } else {
            Err(input.error(HashSet::from_iter(["a four-space tab".to_string()])))
        }
    }
}
impl Sealed for Space4 {}

impl Token for Space4 {
    fn span(&self) -> &Span {
        &self.0
    }

    fn set_span(&mut self, span: Span) {
        self.0 = span;
    }

    fn display() -> String {
        "    ".to_string()
    }
}

#[allow(clippy::tabs_in_doc_comments)]
/// `	`
#[derive(Debug, Clone)]
pub struct Tab(pub(crate) Span);

impl Parse for Tab {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Entry::WhiteSpace(WhiteSpace::Tab(value)) = input.next()? {
            Ok(value.clone())
        } else {
            Err(input.error(HashSet::from_iter(["a tab".to_string()])))
        }
    }
}

impl Sealed for Tab {}

impl Token for Tab {
    fn span(&self) -> &Span {
        &self.0
    }

    fn set_span(&mut self, span: Span) {
        self.0 = span;
    }

    fn display() -> String {
        "\t".to_string()
    }
}

/// `\n`
#[derive(Debug, Clone)]
pub struct NewLine(pub(crate) Span);

impl Parse for NewLine {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Entry::WhiteSpace(WhiteSpace::NewLine(value)) = input.next()? {
            Ok(value.clone())
        } else {
            Err(input.error(HashSet::from_iter(["\\n".to_string()])))
        }
    }
}

impl Sealed for NewLine {}

impl Token for NewLine {
    fn span(&self) -> &Span {
        &self.0
    }

    fn set_span(&mut self, span: Span) {
        self.0 = span;
    }

    fn display() -> String {
        "\\n".to_string()
    }
}

/// Generate types for keywords.
///
/// ## Usage
/// ```
/// use flexi_parse::parse_string;
/// mod kw {
///     use flexi_parse::keywords;
///     keywords!["let", "if"];
/// }
///
/// # fn main() {
/// let kw1: kw::keyword_let = parse_string("let".to_string()).unwrap();
/// let kw2: kw::keyword_if = parse_string("if".to_string()).unwrap();
/// # }
/// ```
#[macro_export]
macro_rules! keywords {
    [ $( $kw:tt ),+ ] => {
        use $crate::token::Token as _;
        $(
            $crate::token::concat_idents!(struct_name = keyword_, $kw {
                #[derive(Debug, Clone)]
                #[allow(non_camel_case_types)]
                pub struct struct_name($crate::Span);

                impl $crate::Parse for struct_name {
                    fn parse(input: $crate::ParseStream<'_>) -> $crate::Result<Self> {
                        let ident: $crate::token::Ident = input.parse()?;
                        if ident.string() == $kw {
                            $crate::Result::Ok(Self(ident.span().to_owned()))
                        } else {
                            $crate::Result::Err(input.error(
                                ::std::collections::HashSet::from_iter([$kw.to_string()]),
                            ))
                        }
                    }
                }

                impl $crate::private::Sealed for struct_name {}

                impl $crate::token::Token for struct_name {
                    fn span(&self) -> &$crate::Span {
                        &self.0
                    }

                    fn set_span(&mut self, span: $crate::Span) {
                        self.0 = span;
                    }

                    fn display() -> String {
                        $kw.to_string()
                    }
                }
            });
        )+

        #[allow(dead_code)]
        pub fn ident(input: $crate::ParseStream<'_>) -> $crate::Result<$crate::token::Ident> {
            use $crate::token::Ident;
            let ident: Ident = input.parse()?;
            if [$( $kw ),+].contains(&ident.string().as_str()) {
                $crate::Result::Err(input.error(
                    ::std::collections::HashSet::from_iter(["an identifier".to_string()]),
                ))
            } else {
                $crate::Result::Ok(ident)
            }
        }
    };
}

pub use keywords;
