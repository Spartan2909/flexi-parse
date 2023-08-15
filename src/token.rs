use crate::error::Error;
use crate::error::ErrorKind;
use crate::private::Sealed;
use crate::Parse;
use crate::ParseStream;
use crate::Result;
use crate::Span;
use crate::TokenStream;
use crate::TokenTree;

use std::collections::HashSet;
use std::fmt;
use std::marker::PhantomData;
use std::rc::Rc;
use std::result;

#[doc(hidden)]
pub use concat_idents::concat_idents;

pub trait Token: Parse + Sealed {
    #[doc(hidden)]
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

pub trait Punct: Token {}

pub trait Delimiter {
    type Start: Punct;
    type End: Punct;
    const CAN_NEST: bool;
}

#[derive(Debug, Clone, Copy)]
pub struct Parenthesis;

impl Delimiter for Parenthesis {
    type Start = LeftParen;
    type End = RightParen;
    const CAN_NEST: bool = true;
}

#[derive(Debug, Clone, Copy)]
pub struct Brackets;

impl Delimiter for Brackets {
    type Start = LeftBracket;
    type End = RightBracket;
    const CAN_NEST: bool = true;
}

#[derive(Debug, Clone, Copy)]
pub struct Braces;

impl Delimiter for Braces {
    type Start = LeftBrace;
    type End = RightBrace;
    const CAN_NEST: bool = true;
}

#[derive(Debug, Clone, Copy)]
pub struct AngleBrackets;

impl Delimiter for AngleBrackets {
    type Start = LAngle;
    type End = RAngle;
    const CAN_NEST: bool = true;
}

#[derive(Debug, Clone, Copy)]
pub struct SingleQuotes;

impl Delimiter for SingleQuotes {
    type Start = SingleQuote;
    type End = SingleQuote;
    const CAN_NEST: bool = false;
}

#[derive(Debug, Clone, Copy)]
pub struct DoubleQuotes;

impl Delimiter for DoubleQuotes {
    type Start = DoubleQuote;
    type End = DoubleQuote;
    const CAN_NEST: bool = false;
}

#[derive(Debug, Clone)]
pub struct Group<D: Delimiter> {
    token_stream: TokenStream,
    span: Span,
    _marker: PhantomData<D>,
}

impl<D: Delimiter> Group<D> {
    pub fn token_stream(self) -> TokenStream {
        self.token_stream
    }
}

impl<D: Delimiter> Parse for Group<D> {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let group_start = input.parse::<D::Start>()?.span().start;
        let start = input.current()?.0;

        if D::CAN_NEST {
            let mut open = 1;
            loop {
                if input.peek::<D::End>() {
                    open -= 1;
                    if open == 0 {
                        break;
                    } else {
                        input.next()?;
                    }
                } else if input.peek::<D::Start>() {
                    open += 1;
                    input.next()?;
                } else {
                    input.next()?;
                }
            }
        } else {
            while !input.peek::<D::End>() {
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
        tokens.push(TokenTree::End);
        let token_stream = TokenStream::new(tokens, Rc::clone(&input.source));
        let span = Span::new(group_start, group_end, Rc::clone(&input.source));
        Ok(Group {
            token_stream,
            span,
            _marker: PhantomData,
        })
    }
}

impl<D: Delimiter> Sealed for Group<D> {}

impl<D: Delimiter> Token for Group<D> {
    fn span(&self) -> &Span {
        &self.span
    }

    fn set_span(&mut self, span: Span) {
        self.span = span;
    }

    fn display() -> String {
        D::Start::display()
    }
}

#[derive(Debug, Clone)]
pub struct LitStrDoubleQuote {
    string: String,
    span: Span,
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

#[derive(Debug, Clone)]
pub struct LitStrSingleQuote {
    string: String,
    span: Span,
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

#[derive(Debug, Clone)]
pub struct LitChar {
    ch: char,
    span: Span,
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

#[derive(Debug, Clone)]
pub struct Ident {
    pub(crate) string: String,
    pub(crate) span: Span,
}

impl Ident {
    pub fn string(&self) -> &String {
        &self.string
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.string == other.string
    }
}

impl TryFrom<TokenTree> for Ident {
    type Error = TokenTree;

    fn try_from(value: TokenTree) -> result::Result<Self, Self::Error> {
        if let TokenTree::Ident(token) = value {
            Ok(token)
        } else {
            Err(value)
        }
    }
}

impl Parse for Ident {
    fn parse(input: ParseStream) -> Result<Self> {
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

impl<'a> TryFrom<&'a TokenTree> for &'a SingleCharPunct {
    type Error = ();

    fn try_from(value: &'a TokenTree) -> result::Result<Self, Self::Error> {
        if let TokenTree::Punct(token) = value {
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

#[derive(Debug, Clone)]
pub(crate) struct Literal {
    pub(crate) value: LiteralValue,
    pub(crate) span: Span,
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<'a> TryFrom<&'a TokenTree> for &'a Literal {
    type Error = &'a TokenTree;

    fn try_from(value: &'a TokenTree) -> result::Result<Self, Self::Error> {
        if let TokenTree::Literal(token) = value {
            Ok(token)
        } else {
            Err(value)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum LiteralValue {
    Int(u64),
    Float(f64),
}

impl fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralValue::Int(int) => write!(f, "{int}"),
            LiteralValue::Float(float) => write!(f, "{float}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LitInt {
    value: u64,
    span: Span,
}

impl LitInt {
    pub fn value(&self) -> u64 {
        self.value
    }
}

impl Parse for LitInt {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let token = input.next()?;
        if let TokenTree::Literal(Literal {
            value: LiteralValue::Int(value),
            span,
        }) = token
        {
            Ok(LitInt {
                value: *value,
                span: span.clone(),
            })
        } else {
            Err(Error::unexpected_token(
                HashSet::from_iter(["an integer literal".to_string()]),
                token.span().clone(),
                Rc::clone(&input.source),
            ))
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

#[derive(Debug, Clone)]
pub struct LitFloat {
    value: f64,
    span: Span,
}

impl LitFloat {
    pub fn value(&self) -> f64 {
        self.value
    }
}

impl Parse for LitFloat {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let token = input.next()?;
        if let TokenTree::Literal(Literal {
            value: LiteralValue::Float(value),
            span,
        }) = token
        {
            Ok(LitFloat {
                value: *value,
                span: span.clone(),
            })
        } else {
            Err(Error::unexpected_token(
                HashSet::from_iter(["a float literal".to_string()]),
                token.span().clone(),
                Rc::clone(&input.source),
            ))
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
                    if let TokenTree::Punct(SingleCharPunct { kind: PunctKind::$t1, span, .. }) = token {
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
                    if let TokenTree::Punct(SingleCharPunct { spacing: Spacing::Joint, .. }) = input.current()?.1 {

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
                    if let TokenTree::Punct(SingleCharPunct { spacing: Spacing::Joint, .. }) = input.current()?.1 {

                    } else {
                        return Err(Error::empty());
                    }
                    if let TokenTree::Punct(SingleCharPunct { spacing: Spacing::Joint, .. }) = input.current()?.1 {

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

impl<T: Punct> Parse for (T,) {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        Ok((T::parse(input)?,))
    }
}

impl<T: Punct> Sealed for (T,) {}

impl<T: Punct> Token for (T,) {
    fn span(&self) -> &Span {
        self.0.span()
    }

    fn set_span(&mut self, span: Span) {
        self.0.set_span(span);
    }

    fn display() -> String {
        T::display()
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
        if let TokenTree::WhiteSpace(WhiteSpace::Space2(value)) = token {
            Ok(value.clone())
        } else {
            Err(Error::unexpected_token(
                HashSet::from_iter(["a two-space tab".to_string()]),
                token.span().clone(),
                Rc::clone(&input.source),
            ))
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
        if let TokenTree::WhiteSpace(WhiteSpace::Space4(value)) = token {
            Ok(value.clone())
        } else {
            Err(Error::unexpected_token(
                HashSet::from_iter(["a four-space tab".to_string()]),
                token.span().clone(),
                Rc::clone(&input.source),
            ))
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
        let token = input.next()?;
        if let TokenTree::WhiteSpace(WhiteSpace::Tab(value)) = token {
            Ok(value.clone())
        } else {
            Err(Error::unexpected_token(
                HashSet::from_iter(["a tab".to_string()]),
                token.span().clone(),
                Rc::clone(&input.source),
            ))
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
        let token = input.next()?;
        if let TokenTree::WhiteSpace(WhiteSpace::NewLine(value)) = token {
            Ok(value.clone())
        } else {
            Err(Error::unexpected_token(
                HashSet::from_iter(["\\n".to_string()]),
                token.span().clone(),
                Rc::clone(&input.source),
            ))
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
                            $crate::Result::Err($crate::error::Error::unexpected_token(
                                ::std::collections::HashSet::from_iter([$kw.to_string()]),
                                ident.span().to_owned(),
                                ::std::rc::Rc::clone(ident.span().source()),
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
                $crate::Result::Err($crate::error::Error::unexpected_token(
                    ::std::collections::HashSet::from_iter(["an identifier".to_string()]),
                    ident.span().to_owned(),
                    ::std::rc::Rc::clone(ident.span().source()),
                ))
            } else {
                $crate::Result::Ok(ident)
            }
        }
    };
}

pub use keywords;
