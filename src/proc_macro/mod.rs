use crate::error::Error;
use crate::group::parse_delimiters;
use crate::group::Braces;
use crate::group::Brackets;
use crate::group::Parentheses;
use crate::scanner;
use crate::to_tokens::ToTokens;
use crate::token::Ident;
use crate::token::LitChar;
use crate::token::LitFloat;
use crate::token::LitInt;
use crate::token::LitStrDoubleQuote;
use crate::token::LitStrSingleQuote;
use crate::token::PunctKind;
use crate::token::SingleCharPunct;
use crate::token::Spacing;
use crate::Entry;
use crate::ParseBuffer;
use crate::Result;
use crate::SourceFile;
use crate::Span;
use crate::TokenStream;

use std::sync::Arc;

use proc_macro2::Delimiter;
use proc_macro2::Group;
use proc_macro2::Ident as Ident2;
use proc_macro2::Literal;
use proc_macro2::Punct as Punct2;
use proc_macro2::Spacing as Spacing2;
use proc_macro2::Span as Span2;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::TokenTree as TokenTree2;

use quote::TokenStreamExt;

#[cfg(feature = "proc-macro")]
mod proc_macro1;

impl From<Spacing2> for Spacing {
    fn from(value: Spacing2) -> Self {
        match value {
            Spacing2::Alone => Spacing::Alone,
            Spacing2::Joint => Spacing::Joint,
        }
    }
}

impl From<Spacing> for Spacing2 {
    fn from(value: Spacing) -> Self {
        match value {
            Spacing::Alone => Spacing2::Alone,
            Spacing::Joint => Spacing2::Joint,
        }
    }
}

fn tree_to_trees(token: &TokenTree2) -> Vec<Entry> {
    let mut tokens = vec![];
    let span = Span {
        start: 0,
        end: 0,
        source: Arc::new(SourceFile {
            name: String::new(),
            path: None,
            contents: String::new(),
        }),
    };
    match token {
        TokenTree2::Group(group) => {
            let delimiters = match group.delimiter() {
                Delimiter::Parenthesis => Some(('(', ')')),
                Delimiter::Bracket => Some(('[', ']')),
                Delimiter::Brace => Some(('{', '}')),
                Delimiter::None => None,
            };
            if let Some((start, _)) = delimiters {
                tokens.push(Entry::Punct(SingleCharPunct {
                    kind: start.try_into().unwrap(),
                    spacing: Spacing::Alone,
                    span: span.clone(),
                }));
            }
            for token in group.stream() {
                tokens.append(&mut tree_to_trees(&token));
            }
            if let Some((_, end)) = delimiters {
                tokens.push(Entry::Punct(SingleCharPunct {
                    kind: end.try_into().unwrap(),
                    spacing: Spacing::Alone,
                    span: span.clone(),
                }));
            }
        }
        TokenTree2::Ident(ident) => {
            let string = ident.to_string();
            tokens.push(Entry::Ident(Ident {
                string,
                span: span.clone(),
            }));
        }
        TokenTree2::Literal(literal) => {
            tokens.extend(
                scanner::scan(
                    Arc::new(SourceFile {
                        name: String::new(),
                        path: None,
                        contents: literal.to_string(),
                    }),
                    0,
                    None,
                )
                .0
                .tokens
                .into_iter()
                .map(|mut token| {
                    token.set_span(span.clone());
                    token
                }),
            );
        }
        TokenTree2::Punct(punct) => {
            let kind = punct.as_char().try_into().unwrap();
            let spacing = punct.spacing().into();
            tokens.push(Entry::Punct(SingleCharPunct {
                kind,
                spacing,
                span: span.clone(),
            }));
        }
    }
    tokens
}

impl From<TokenStream2> for TokenStream {
    fn from(value: TokenStream2) -> Self {
        let mut tokens = vec![];
        let contents = value.to_string();
        let source = Arc::new(SourceFile::new("<TokenStream>".to_string(), contents));
        for token in value {
            tokens.append(&mut tree_to_trees(&token));
        }
        let span = Span {
            start: 0,
            end: source.contents.len(),
            source: Arc::clone(&source),
        };
        for token in &mut tokens {
            token.set_span(span.clone());
        }
        TokenStream::new(tokens, Some(source))
    }
}

fn token_stream_to_token_stream_2(tokens: &TokenStream) -> Result<TokenStream2> {
    let mut token_trees = vec![];
    let buf = ParseBuffer::from(tokens);
    while let Ok(entry) = buf.current() {
        match entry {
            Entry::Error(_) => {
                panic!("cannot convert a failed scan to a `proc_macro2::TokenStream`")
            }
            Entry::Ident(ident) => {
                if buf.peek(LitFloat) {
                    let float: LitFloat = buf.parse()?;
                    token_trees.push(TokenTree2::Literal(Literal::f64_unsuffixed(float.value())));
                } else if buf.peek(LitInt) {
                    let int: LitInt = buf.parse()?;
                    token_trees.push(TokenTree2::Literal(Literal::u64_unsuffixed(int.value())));
                } else {
                    token_trees.push(TokenTree2::Ident(Ident2::new(
                        &ident.string,
                        Span2::call_site(),
                    )));
                }
            }
            Entry::Punct(punct) => match punct.kind {
                PunctKind::LeftParen => {
                    let (_, _, tokens) = parse_delimiters::<Parentheses>(&buf)?;
                    token_trees.push(TokenTree2::Group(Group::new(
                        Delimiter::Parenthesis,
                        token_stream_to_token_stream_2(&tokens)?,
                    )));
                }
                PunctKind::LeftBracket => {
                    let (_, _, tokens) = parse_delimiters::<Brackets>(&buf)?;
                    token_trees.push(TokenTree2::Group(Group::new(
                        Delimiter::Bracket,
                        token_stream_to_token_stream_2(&tokens)?,
                    )));
                }
                PunctKind::LeftBrace => {
                    let (_, _, tokens) = parse_delimiters::<Braces>(&buf)?;
                    token_trees.push(TokenTree2::Group(Group::new(
                        Delimiter::Brace,
                        token_stream_to_token_stream_2(&tokens)?,
                    )));
                }
                PunctKind::DoubleQuote => {
                    let str: LitStrDoubleQuote = buf.parse()?;
                    token_trees.push(TokenTree2::Literal(Literal::string(str.string())));
                }
                PunctKind::SingleQuote => {
                    let ch: LitChar = buf.parse()?;
                    token_trees.push(TokenTree2::Literal(Literal::character(ch.ch())));
                }
                kind => token_trees.push(TokenTree2::Punct(Punct2::new(
                    kind.into(),
                    punct.spacing.into(),
                ))),
            },
            Entry::WhiteSpace(_) => {}
            Entry::End => break,
        }
        buf.next_raw();
    }
    Ok(token_trees.into_iter().collect())
}

impl<'a> TryFrom<&'a TokenStream> for TokenStream2 {
    type Error = Error;

    fn try_from(value: &'a TokenStream) -> std::prelude::v1::Result<Self, Self::Error> {
        token_stream_to_token_stream_2(value)
    }
}

impl TryFrom<TokenStream> for TokenStream2 {
    type Error = Error;

    fn try_from(value: TokenStream) -> Result<Self> {
        token_stream_to_token_stream_2(&value)
    }
}

impl ToTokens for TokenStream2 {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append(&mut TokenStream::from(self.clone()));
    }

    fn to_token_stream(&self) -> TokenStream {
        TokenStream::from(self.clone())
    }

    fn into_token_stream(self) -> TokenStream {
        TokenStream::from(self)
    }
}

/// A wrapper for types that implement [`crate::to_tokens::ToTokens`] which
/// implements [`quote::ToTokens`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ToTokensWrapper<T: ?Sized>(pub T);

impl<T: ToTokens> quote::ToTokens for ToTokensWrapper<T> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let new_tokens = self.0.to_token_stream();
        tokens.append_all(TokenStream2::try_from(new_tokens).unwrap());
    }

    fn to_token_stream(&self) -> TokenStream2 {
        self.0.to_token_stream().try_into().unwrap()
    }

    fn into_token_stream(self) -> TokenStream2 {
        self.0.into_token_stream().try_into().unwrap()
    }
}

impl quote::ToTokens for TokenStream {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append_all(TokenStream2::try_from(self).unwrap());
    }

    fn to_token_stream(&self) -> TokenStream2 {
        self.try_into().unwrap()
    }

    fn into_token_stream(self) -> TokenStream2 {
        self.try_into().unwrap()
    }
}

macro_rules! impl_literals {
    [$($ty:ty),+ $(,)?] => {
        $(
            impl quote::ToTokens for $ty {
                fn to_tokens(&self, tokens: &mut TokenStream2) {
                    tokens.append_all(
                        TokenStream2::try_from(ToTokens::to_token_stream(self)).unwrap()
                    );
                }

                fn to_token_stream(&self) -> TokenStream2 {
                    TokenStream2::try_from(ToTokens::to_token_stream(self)).unwrap()
                }

                fn into_token_stream(self) -> TokenStream2 {
                    TokenStream2::try_from(ToTokens::into_token_stream(self)).unwrap()
                }
            }
        )+
    };
}

impl_literals![
    LitStrDoubleQuote,
    LitStrSingleQuote,
    LitChar,
    LitInt,
    LitFloat
];
