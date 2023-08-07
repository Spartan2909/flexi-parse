use crate::error::Error;
use crate::error::ErrorKind;
use crate::private::Sealed;
use crate::Parse;
use crate::Span;
use crate::TokenStream;
use crate::TokenTree;

use std::collections::HashSet;
use std::fmt;

pub trait Token<'src>: Parse<'src> + Sealed {}

pub trait Punct<'src>: Token<'src> {
    fn display() -> String;
}

impl<'src, T: Token<'src>> Parse<'src> for Option<T> {
    fn from_tokens(tokens: &mut TokenStream<'src>) -> Result<Self, Error<'src>> {
        match tokens.try_parse() {
            Ok(value) => Ok(Some(value)),
            Err(_) => Ok(None),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Ident<'src> {
    pub(crate) string: String,
    pub(crate) span: Span<'src>,
}

impl PartialEq for Ident<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.string == other.string
    }
}

impl<'src> TryFrom<TokenTree<'src>> for Ident<'src> {
    type Error = ();

    fn try_from(value: TokenTree<'src>) -> Result<Self, Self::Error> {
        if let TokenTree::Ident(token) = value {
            Ok(token)
        } else {
            Err(())
        }
    }
}

impl<'src> Parse<'src> for Ident<'src> {
    fn from_tokens(tokens: &mut TokenStream<'src>) -> Result<Self, Error<'src>> {
        if let Ok(ident) = Self::try_from(tokens.next()?.to_owned()) {
            Ok(ident)
        } else {
            todo!()
        }
    }
}

impl Sealed for Ident<'_> {}

impl<'src> Token<'src> for Ident<'src> {}

#[derive(Debug, Clone, Copy)]
pub(crate) struct SingleCharPunct<'src> {
    pub(super) kind: PunctKind,
    pub(super) spacing: Spacing,
    pub(super) span: Span<'src>,
}

impl PartialEq for SingleCharPunct<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.spacing == other.spacing
    }
}

impl<'src, 'a> TryFrom<&'a TokenTree<'src>> for &'a SingleCharPunct<'src> {
    type Error = ();

    fn try_from(value: &'a TokenTree<'src>) -> Result<Self, Self::Error> {
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

impl From<char> for Spacing {
    fn from(value: char) -> Self {
        if PunctKind::try_from(value).is_ok() {
            Spacing::Joint
        } else {
            Spacing::Alone
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Literal<'src> {
    pub(crate) value: LiteralValue,
    pub(crate) span: Span<'src>,
}

impl PartialEq for Literal<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<'src, 'a> TryFrom<&'a TokenTree<'src>> for &'a Literal<'src> {
    type Error = ();

    fn try_from(value: &'a TokenTree<'src>) -> Result<Self, Self::Error> {
        if let TokenTree::Literal(token) = value {
            Ok(token)
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum LiteralValue {
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
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
            #[derive(Debug, Clone, Copy)]
            #[doc = $doc1]
            pub struct $t1<'src>(Span<'src>);

            impl<'src> $t1<'src> {
                pub fn span(&self) -> Span<'src> {
                    self.0
                }
            }

            impl<'src> Parse<'src> for $t1<'src> {
                fn from_tokens(tokens: &mut TokenStream<'src>) -> Result<Self, Error<'src>> {
                    let token = tokens.next()?.to_owned();
                    if let TokenTree::Punct(SingleCharPunct { kind: PunctKind::$t1, span, .. }) = token {
                        Ok(Self(span))
                    } else {
                        Err(Error::new(tokens.source_file, ErrorKind::UnexpectedToken {
                            expected: HashSet::from_iter(vec![$name1.to_string()]),
                            span: token.span(),
                        }))
                    }
                }
            }

            impl Sealed for $t1<'_> {}

            impl<'src> Token<'src> for $t1<'src> {}

            impl fmt::Display for $t1<'_> {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(f, "{}", $name1)
                }
            }

            impl<'src> Punct<'src> for $t1<'src> {
                fn display() -> String {
                    $name1.to_string()
                }
            }
        )+

        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub(crate) enum PunctKind {
            $( $t1 ),+
        }

        impl TryFrom<char> for PunctKind {
            type Error = char;

            fn try_from(value: char) -> Result<Self, Self::Error> {
                match value {
                    $( $name1 => Ok(PunctKind::$t1), )+
                    _ => Err(value),
                }
            }
        }

        $(
            #[derive(Debug, Clone, Copy)]
            #[doc = $doc2]
            pub struct $t2<'src>(Span<'src>);

            impl<'src> $t2<'src> {
                pub fn span(&self) -> Span<'src> {
                    self.0
                }

                fn from_tokens_impl(tokens: &mut TokenStream<'src>) -> Result<Self, Error<'src>> {
                    if let TokenTree::Punct(SingleCharPunct { spacing: Spacing::Joint, .. }) = tokens.peek(0)? {

                    } else {
                        return Err(Error::empty());
                    }

                    let start: $t21 = tokens.parse()?;
                    let end: $t22 = tokens.parse()?;
                    Ok(Self(Span::new(start.0.start, end.0.end, start.0.source)))
                }
            }

            impl<'src> Parse<'src> for $t2<'src> {
                fn from_tokens(tokens: &mut TokenStream<'src>) -> Result<Self, Error<'src>> {
                    let span = tokens.peek(0)?.span();
                    Self::from_tokens_impl(tokens).map_err(|_| {
                        Error::new(tokens.source_file, ErrorKind::UnexpectedToken {
                            expected: HashSet::from_iter(vec![$name2.to_string()]),
                            span,
                        })
                    })
                }
            }

            impl Sealed for $t2<'_> {}

            impl<'src> Token<'src> for $t2<'src> {}

            impl fmt::Display for $t2<'_> {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(f, $name2)
                }
            }

            impl<'src> Punct<'src> for $t2<'src> {
                fn display() -> String {
                    $name2.to_string()
                }
            }
        )+

        $(
            #[derive(Debug, Clone, Copy)]
            #[doc = $doc3]
            pub struct $t3<'src>(Span<'src>);

            impl<'src> $t3<'src> {
                pub fn span(&self) -> Span<'src> {
                    self.0
                }

                fn from_tokens_impl(tokens: &mut TokenStream<'src>) -> Result<Self, Error<'src>> {
                    if let TokenTree::Punct(SingleCharPunct { spacing: Spacing::Joint, .. }) = tokens.peek(0)? {

                    } else {
                        return Err(Error::empty());
                    }
                    if let TokenTree::Punct(SingleCharPunct { spacing: Spacing::Joint, .. }) = tokens.peek(1)? {

                    } else {
                        return Err(Error::empty());
                    }
                    let p1: $t31 = tokens.parse()?;
                    let _p2: $t32 = tokens.parse()?;
                    let p3: $t33 = tokens.parse()?;
                    Ok(Self(Span::new(p1.0.start, p3.0.end, p1.0.source)))
                }
            }

            impl<'src> Parse<'src> for $t3<'src> {
                fn from_tokens(tokens: &mut TokenStream<'src>) -> Result<Self, Error<'src>> {
                    let span = tokens.peek(0)?.span();
                    Self::from_tokens_impl(tokens).map_err(|_| {
                        Error::new(tokens.source_file, ErrorKind::UnexpectedToken {
                            expected: HashSet::from_iter(vec![$name3.to_string()]),
                            span,
                        })
                    })
                }
            }

            impl Sealed for $t3<'_> {}

            impl<'src> Token<'src> for $t3<'src> {}

            impl fmt::Display for $t3<'_> {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(f, $name3)
                }
            }

            impl<'src> Punct<'src> for $t3<'src> {
                fn display() -> String {
                    $name3.to_string()
                }
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
        (Space, ' ', "` `")
        (UnderScore, '_', "`_`")
        (NewLine, '\n', "`\\n`")
    }
    {
        (BangEqual, Bang, Equal, "!=", "`!=`")
        (GreaterEqual, RAngle, Equal, ">=", "`>=`")
        (LessEqual, LAngle, Equal, "<=", "`<=`")
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
        (LogicalAnd, Ampersand, Ampersand, "&&", "`&&`")
        (LogicalOr, Pipe, Pipe, "||", "`||`")
    }
    {
        (HashHashHash, Hash, Hash, Hash, "###", "`###`")
        (SlashSlashEqual, Slash, Slash, Equal, "//=", "`//=`")
        (LAngleLAngleEqual, LAngle, LAngle, Equal, "<<=", "`<<=`")
        (RAngleRAngleEqual, RAngle, RAngle, Equal, ">>=", "`>>=`")
        (ColonColonEqual, Colon, Colon, Equal, "::=", "`::=`")
    }
}

fn parse_joint_impl<'src, T1: Punct<'src>, T2: Punct<'src>>(
    tokens: &mut TokenStream<'src>,
) -> Result<(T1, T2), Error<'src>> {
    let t1 = tokens.parse()?;
    if let TokenTree::Punct(SingleCharPunct {
        spacing: Spacing::Joint,
        ..
    }) = tokens.peek(-1)?
    {
    } else {
        return Err(Error::empty());
    }
    let t2 = tokens.parse()?;
    Ok((t1, t2))
}

impl<'src, T1: Punct<'src>, T2: Punct<'src>> Parse<'src> for (T1, T2) {
    fn from_tokens(tokens: &mut TokenStream<'src>) -> Result<Self, Error<'src>> {
        let span = tokens.peek(0)?.span();
        parse_joint_impl(tokens).map_err(|_| {
            Error::new(
                tokens.source_file,
                ErrorKind::UnexpectedToken {
                    expected: HashSet::from_iter(vec![<(T1, T2)>::display()]),
                    span,
                },
            )
        })
    }
}

impl<'src, T1: Punct<'src>, T2: Punct<'src>> Sealed for (T1, T2) {}

impl<'src, T1: Punct<'src>, T2: Punct<'src>> Token<'src> for (T1, T2) {}

impl<'src, T1: Punct<'src>, T2: Punct<'src>> Punct<'src> for (T1, T2) {
    fn display() -> String {
        T1::display() + &T2::display()
    }
}

/// `  `
#[derive(Debug, Clone, Copy)]
pub struct Tab2<'src>(Span<'src>);

impl<'src> Parse<'src> for Tab2<'src> {
    fn from_tokens(tokens: &mut TokenStream<'src>) -> Result<Self, Error<'src>> {
        let (s1, s2): (Space, Space) = tokens.parse()?;
        Ok(Tab2(Span::new(
            s1.span().start,
            s2.span().end,
            tokens.source_file,
        )))
    }
}

impl Sealed for Tab2<'_> {} 

impl<'src> Token<'src> for Tab2<'src> {}

impl<'src> Punct<'src> for Tab2<'src> {
    fn display() -> String {
        "  ".to_string()
    }
}

/// `    `
#[derive(Debug, Clone, Copy)]
pub struct Tab4<'src>(Span<'src>);

impl<'src> Parse<'src> for Tab4<'src> {
    fn from_tokens(tokens: &mut TokenStream<'src>) -> Result<Self, Error<'src>> {
        let (s1, s2): (Tab2, Tab2) = tokens.parse()?;
        Ok(Tab4(Span::new(
            s1.0.start,
            s2.0.end,
            tokens.source_file,
        )))
    }
}

impl Sealed for Tab4<'_> {} 

impl<'src> Token<'src> for Tab4<'src> {}

impl<'src> Punct<'src> for Tab4<'src> {
    fn display() -> String {
        "    ".to_string()
    }
}
