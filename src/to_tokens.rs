//! A trait for converting types composed of tokens into a [`TokenStream`].
//!
//! # The [`ToTokens`] trait
//! This trait provides a method of converting types into a [`TokenStream`]. This is primarily
//! intended as a means of generating source code from a syntax tree node.
//!
//! # [`quote`][quote]
//! The API of this trait intentionally mirrors that of [`quote::ToTokens`][quote_to_tokens],
//! and so should be familiar to users of that crate.
//!
//! [quote]: https://docs.rs/quote/latest/quote/
//! [quote_to_tokens]: https://docs.rs/quote/latest/quote/trait.ToTokens.html

use std::sync::Arc;

use crate::scanner;
use crate::token::CarriageReturn;
use crate::token::Ident;
use crate::token::LitChar;
use crate::token::LitFloat;
use crate::token::LitInt;
use crate::token::LitStrDoubleQuote;
use crate::token::LitStrSingleQuote;
use crate::token::NewLine;
use crate::token::PunctKind;
use crate::token::SingleCharPunct;
use crate::token::Space2;
use crate::token::Space4;
use crate::token::Spacing;
use crate::token::Tab;
use crate::token::Token;
use crate::token::WhiteSpace;
use crate::Entry;
use crate::Span;
use crate::TokenStream;

/// A trait for converting types into a [`TokenStream`].
///
/// See the [module documentation][module] for more information.
///
/// [module]: (crate::to_tokens)
pub trait ToTokens {
    /// Append `self` to the given [`TokenStream`].
    fn to_tokens(&self, tokens: &mut TokenStream);

    /// Convert `self` directly into a [`TokenStream`].
    fn to_token_stream(&self) -> TokenStream {
        let mut tokens = TokenStream::new(vec![], None);
        self.to_tokens(&mut tokens);
        tokens
    }

    /// Convert `self` directly into a [`TokenStream`].
    fn into_token_stream(self) -> TokenStream
    where
        Self: Sized,
    {
        self.to_token_stream()
    }
}

impl<'a, T: ?Sized + ToTokens> ToTokens for &'a T {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        T::to_tokens(self, tokens);
    }

    fn to_token_stream(&self) -> TokenStream {
        T::to_token_stream(self)
    }
}

impl<'a, T: ?Sized + ToTokens> ToTokens for &'a mut T {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        T::to_tokens(self, tokens);
    }

    fn to_token_stream(&self) -> TokenStream {
        T::to_token_stream(self)
    }
}

impl<T: ToTokens> ToTokens for Option<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if let Some(value) = self {
            value.to_tokens(tokens);
        }
    }

    fn to_token_stream(&self) -> TokenStream {
        self.as_ref()
            .map_or_else(|| TokenStream::new(vec![], None), ToTokens::to_token_stream)
    }

    fn into_token_stream(self) -> TokenStream {
        self.map_or_else(
            || TokenStream::new(vec![], None),
            ToTokens::into_token_stream,
        )
    }
}

impl ToTokens for TokenStream {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.tokens.append(&mut self.tokens.clone());
    }

    fn to_token_stream(&self) -> TokenStream {
        self.clone()
    }

    fn into_token_stream(self) -> TokenStream
    where
        Self: Sized,
    {
        self
    }
}

macro_rules! impl_nums {
    [$($ty:ty),* $(,)?] => {
        $(
            impl ToTokens for $ty {
                fn to_tokens(&self, tokens: &mut TokenStream) {
                    tokens.tokens.push(Entry::Ident(Ident::new(self.to_string(), Span::empty())));
                }
            }
        )*
    };
}

impl_nums![u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize, f32, f64];

impl ToTokens for str {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        LitStrDoubleQuote::new(self.to_string(), Span::empty()).to_tokens(tokens);
    }
}

impl ToTokens for String {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.as_str().to_tokens(tokens);
    }
}

fn str_to_tokens(string: &str, span: &Span, delimiter: PunctKind, tokens: &mut TokenStream) {
    let start_spacing = string.chars().next().map_or(Spacing::Joint, |ch| {
        if PunctKind::try_from(ch).is_ok() {
            Spacing::Joint
        } else {
            Spacing::Alone
        }
    });
    let start_span = Span::new(span.start, span.start + 1, Arc::clone(&span.source));
    let start = Entry::Punct(SingleCharPunct::new(delimiter, start_spacing, start_span));
    tokens.push(start);

    let (
        TokenStream {
            tokens: mut new_tokens,
            source: _,
        },
        _,
    ) = scanner::scan(
        Arc::clone(&span.source),
        (span.start + 1) as usize,
        Some((span.end - 1) as usize),
    );
    tokens.tokens.append(&mut new_tokens);

    let end_span = Span::new(span.end - 1, span.end, Arc::clone(&span.source));
    let end = Entry::Punct(SingleCharPunct::new(delimiter, Spacing::Alone, end_span));
    tokens.push(end);
}

impl ToTokens for LitStrDoubleQuote {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        str_to_tokens(self.string(), self.span(), PunctKind::DoubleQuote, tokens);
    }
}

impl ToTokens for LitStrSingleQuote {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        str_to_tokens(self.string(), self.span(), PunctKind::SingleQuote, tokens);
    }
}

impl ToTokens for LitChar {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let start_spacing = if PunctKind::try_from(self.ch()).is_ok() {
            Spacing::Joint
        } else {
            Spacing::Alone
        };
        let start_span = Span::new(
            self.span().start,
            self.span().start + 1,
            Arc::clone(&self.span().source),
        );
        let start = Entry::Punct(SingleCharPunct::new(
            PunctKind::SingleQuote,
            start_spacing,
            start_span,
        ));
        tokens.push(start);

        tokens.push(Entry::Ident(Ident::new(
            self.ch().to_string(),
            Span::empty(),
        )));

        let end_span = Span::new(
            self.span().end - 1,
            self.span().end,
            Arc::clone(&self.span().source),
        );
        let end = Entry::Punct(SingleCharPunct::new(
            PunctKind::SingleQuote,
            Spacing::Alone,
            end_span,
        ));
        tokens.push(end);
    }
}

impl ToTokens for Ident {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.push(Entry::Ident(self.clone()));
    }

    fn into_token_stream(self) -> TokenStream {
        let source = Arc::clone(&self.span.source);
        TokenStream::new(vec![Entry::Ident(self)], Some(source))
    }
}

impl ToTokens for LitInt {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.push(Entry::Ident(Ident::new(
            self.value().to_string(),
            self.span().clone(),
        )));
    }
}

#[allow(clippy::cast_sign_loss)]
#[allow(clippy::while_float)]
fn fract_part(value: f64) -> u64 {
    let eps = 1e-4;
    let mut value = value.abs().fract();
    if value == 0.0 {
        return 0;
    }

    while (value.round() - value).abs() <= eps {
        value *= 10.0;
    }

    while (value.round() - value).abs() > eps {
        value *= 10.0;
    }

    value.round() as u64
}

impl ToTokens for LitFloat {
    #[allow(clippy::cast_sign_loss)] // There are no signed literals.
    fn to_tokens(&self, tokens: &mut TokenStream) {
        debug_assert!(self.value() >= 0.0, "literals must not be signed");
        let start = (self.value() - self.value().fract()).floor() as u64;
        let num_start_digits = start.checked_ilog10().unwrap_or(0) + 1;
        let fract = fract_part(self.value().fract());
        let num_end_digits = fract.checked_ilog10().unwrap_or(0) + 1;
        tokens.push(Entry::Ident(Ident::new(
            start.to_string(),
            Span::new(
                self.span().start,
                self.span().start + num_start_digits,
                Arc::clone(&self.span().source),
            ),
        )));
        tokens.push(Entry::Punct(SingleCharPunct::new(
            PunctKind::Dot,
            Spacing::Alone,
            Span::new(
                num_start_digits,
                num_start_digits + 1,
                Arc::clone(&self.span().source),
            ),
        )));
        tokens.push(Entry::Ident(Ident::new(
            fract.to_string(),
            Span::new(
                self.span().end - num_end_digits,
                self.span().end,
                Arc::clone(&self.span().source),
            ),
        )));
    }
}

impl ToTokens for WhiteSpace {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.push(Entry::WhiteSpace(self.clone()));
    }
}

impl ToTokens for Space2 {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.push(Entry::WhiteSpace(WhiteSpace::Space2(self.clone())));
    }
}

impl ToTokens for Space4 {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let start_span = Span::new(
            self.span.start,
            self.span.start + 2,
            Arc::clone(&self.span.source),
        );
        tokens.push(Entry::WhiteSpace(WhiteSpace::Space2(Space2 {
            span: start_span,
        })));
        let end_span = Span::new(
            self.span.end - 2,
            self.span.end,
            Arc::clone(&self.span.source),
        );
        tokens.push(Entry::WhiteSpace(WhiteSpace::Space2(Space2 {
            span: end_span,
        })));
    }
}

impl ToTokens for Tab {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.push(Entry::WhiteSpace(WhiteSpace::Tab(self.clone())));
    }
}

impl ToTokens for NewLine {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.push(Entry::WhiteSpace(WhiteSpace::NewLine(self.clone())));
    }
}

impl ToTokens for CarriageReturn {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.push(Entry::WhiteSpace(WhiteSpace::CarriageReturn(self.clone())));
    }
}
