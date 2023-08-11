use crate::scanner;
use crate::token::Ident;
use crate::token::SingleCharPunct;
use crate::token::Spacing;
use crate::SourceFile;
use crate::Span;
use crate::TokenStream;
use crate::TokenTree;

use std::rc::Rc;

use proc_macro2::Delimiter;
use proc_macro2::Spacing as Spacing2;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::TokenTree as TokenTree2;

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

fn tree_to_trees(token: TokenTree2) -> Vec<TokenTree> {
    let mut tokens = vec![];
    let span = Span {
        start: 0,
        end: 0,
        source: Rc::new(SourceFile {
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
                tokens.push(TokenTree::Punct(SingleCharPunct {
                    kind: start.try_into().unwrap(),
                    spacing: Spacing::Alone,
                    span: span.clone(),
                }));
            }
            for token in group.stream() {
                tokens.append(&mut tree_to_trees(token));
            }
            if let Some((_, end)) = delimiters {
                tokens.push(TokenTree::Punct(SingleCharPunct {
                    kind: end.try_into().unwrap(),
                    spacing: Spacing::Alone,
                    span: span.clone(),
                }));
            }
        }
        TokenTree2::Ident(ident) => {
            let string = ident.to_string();
            tokens.push(TokenTree::Ident(Ident {
                string,
                span: span.clone(),
            }));
        }
        TokenTree2::Literal(literal) => {
            tokens.extend(
                scanner::scan(Rc::new(SourceFile {
                    name: String::new(),
                    path: None,
                    contents: literal.to_string(),
                }))
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
            tokens.push(TokenTree::Punct(SingleCharPunct {
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
        let source = Rc::new(SourceFile::new("<TokenStream>".to_string(), contents));
        for token in value {
            tokens.append(&mut tree_to_trees(token));
        }
        let span = Span {
            start: 0,
            end: source.contents.len(),
            source: Rc::clone(&source),
        };
        for token in &mut tokens {
            token.set_span(span.clone());
        }
        tokens.push(TokenTree::End);
        TokenStream { tokens, source }
    }
}
