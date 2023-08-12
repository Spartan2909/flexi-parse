use crate::error::Error;
use crate::error::ErrorKind;
use crate::token::Ident;
use crate::token::Literal;
use crate::token::LiteralValue;
use crate::token::PunctKind;
use crate::token::SingleCharPunct;
use crate::token::Spacing;
use crate::Result;
use crate::SourceFile;
use crate::Span;
use crate::TokenStream;
use crate::TokenTree;

use std::rc::Rc;

fn valid_ident_char(c: Option<char>) -> bool {
    if let Some(c) = c {
        c.is_alphanumeric() || c == '_'
    } else {
        false
    }
}

fn compare_result<T: PartialEq>(l: Result<T>, r: T) -> bool {
    if let Ok(l) = l {
        l == r
    } else {
        false
    }
}

#[derive(Debug)]
struct Scanner {
    current: usize,
    end: usize,
    errors: Error,
    source: Rc<SourceFile>,
}

impl Scanner {
    fn scan(mut self) -> (TokenStream, Option<Error>) {
        let mut tokens = vec![];

        while !self.is_at_end() {
            match self.scan_token() {
                Ok(token) => tokens.push(token),
                Err(err) => {
                    self.errors.add(err);
                    tokens.push(TokenTree::Error(Span::new(0, 0, Rc::clone(&self.source))));
                    break;
                }
            }
        }

        let mut to_remove = vec![];
        let mut i = 0;
        while i < tokens.len() {
            if tokens[i].is_space() {
                if tokens[i + 1].is_space() {
                    i += 2;
                    while tokens[i].is_space() {
                        i += 1;
                    }
                } else {
                    to_remove.push(i);
                    i += 1;
                }
            } else {
                i += 1;
            }
        }
        to_remove.reverse();
        for index in to_remove {
            tokens.remove(index);
        }

        let mut punct_joint = false;
        for token in tokens.iter_mut().rev() {
            if let TokenTree::Punct(punct) = token {
                if punct_joint {
                    punct.spacing = Spacing::Joint;
                } else {
                    punct_joint = true;
                }
            } else {
                punct_joint = false;
            }
        }

        tokens.push(TokenTree::End);

        let errors = if self.errors.is_empty() {
            None
        } else {
            Some(self.errors)
        };

        (TokenStream::new(tokens, self.source), errors)
    }

    fn scan_token(&mut self) -> Result<TokenTree> {
        let token = match self.peek(0)? {
            c if PunctKind::try_from(c).is_ok() => {
                let kind = c.try_into().unwrap();
                let span = Span::new(self.current, self.current + 1, Rc::clone(&self.source));
                self.current += 1;
                TokenTree::Punct(SingleCharPunct {
                    kind,
                    spacing: Spacing::Alone,
                    span,
                })
            }
            c if c.is_ascii_digit() => {
                let start = self.current;
                loop {
                    let c = self.peek(0);
                    if c.is_ok_and(|c| c.is_ascii_digit()) {
                        self.current += 1;
                    } else {
                        break;
                    }
                }
                let value = if compare_result(self.peek(0), '.') && self.peek(1)?.is_ascii_digit() {
                    self.current += 1;
                    while self.peek(0)?.is_ascii_digit() {
                        self.current += 1;
                    }
                    LiteralValue::Float(self.source.contents[start..self.current].parse().unwrap())
                } else {
                    LiteralValue::Int(self.source.contents[start..self.current].parse().unwrap())
                };
                TokenTree::Literal(Literal {
                    value,
                    span: Span::new(start, self.current, Rc::clone(&self.source)),
                })
            }
            c if c.is_alphabetic() || c == '_' => {
                let start = self.current;
                while valid_ident_char(self.peek(0).ok()) {
                    self.current += 1;
                }
                let string = self.source.contents[start..self.current].to_string();
                let span = Span::new(start, self.current, Rc::clone(&self.source));

                TokenTree::Ident(Ident { string, span })
            }
            _ => {
                self.current += 1;
                return Err(Error::new(
                    Rc::clone(&self.source),
                    ErrorKind::UnknownCharacter(Span::new(
                        self.current,
                        self.current + 1,
                        Rc::clone(&self.source),
                    )),
                ));
            }
        };

        Ok(token)
    }

    fn peek(&mut self, offset: usize) -> Result<char> {
        if self.current + offset >= self.source.contents.len() {
            Err(Error::new(
                Rc::clone(&self.source),
                ErrorKind::EndOfFile(self.source.contents.len()),
            ))
        } else {
            Ok(
                self.source.contents[self.current + offset..self.current + offset + 1]
                    .chars()
                    .next()
                    .unwrap(),
            )
        }
    }

    fn is_at_end(&mut self) -> bool {
        self.current >= self.end
    }
}

pub(crate) fn scan(source: Rc<SourceFile>) -> (TokenStream, Option<Error>) {
    let (tokens, errors) = Scanner {
        current: 0,
        end: source.contents.len(),
        errors: Error::empty(),
        source,
    }
    .scan();
    (tokens, errors)
}
