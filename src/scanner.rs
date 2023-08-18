use crate::error::Error;
use crate::error::ErrorKind;
use crate::token::Ident;
use crate::token::NewLine;
use crate::token::PunctKind;
use crate::token::CarriageReturn;
use crate::token::SingleCharPunct;
use crate::token::Space2;
use crate::token::Spacing;
use crate::token::Tab;
use crate::token::WhiteSpace;
use crate::Entry;
use crate::Result;
use crate::SourceFile;
use crate::Span;
use crate::TokenStream;

use std::rc::Rc;

fn valid_ident_char(c: Option<char>) -> bool {
    if let Some(c) = c {
        c.is_alphanumeric() || c == '_'
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
                    tokens.push(Entry::Error(Span::new(0, 0, Rc::clone(&self.source))));
                    break;
                }
            }
        }

        tokens.push(Entry::End);

        let errors = if self.errors.is_empty() {
            None
        } else {
            Some(self.errors)
        };

        (TokenStream::new(tokens, self.source), errors)
    }

    fn scan_token(&mut self) -> Result<Entry> {
        let token = match self.peek(0)? {
            c if PunctKind::try_from(c).is_ok() => {
                let kind = c.try_into().unwrap();
                let span = Span::new(self.current, self.current + 1, Rc::clone(&self.source));
                self.current += 1;
                let spacing = if self.peek(0).is_ok_and(|c| PunctKind::try_from(c).is_ok()) {
                    Spacing::Joint
                } else {
                    Spacing::Alone
                };

                Entry::Punct(SingleCharPunct {
                    kind,
                    spacing,
                    span,
                })
            }
            c if c.is_alphanumeric() || c == '_' => {
                let start = self.current;
                while valid_ident_char(self.peek(0).ok()) {
                    self.current += 1;
                }
                let string = self.source.contents[start..self.current].to_string();
                let span = Span::new(start, self.current, Rc::clone(&self.source));

                Entry::Ident(Ident { string, span })
            }
            ' ' if self.peek(1).is_ok_and(|c| c == ' ') => {
                self.current += 2;
                Entry::WhiteSpace(WhiteSpace::Space2(Space2(Span::new(
                    self.current - 2,
                    self.current,
                    Rc::clone(&self.source),
                ))))
            }
            ' ' => {
                self.current += 1;
                self.scan_token()?
            }
            '\t' => {
                let span = Span::new(self.current, self.current + 1, Rc::clone(&self.source));
                self.current += 1;
                Entry::WhiteSpace(WhiteSpace::Tab(Tab(span)))
            }
            '\n' => {
                let span = Span::new(self.current, self.current + 1, Rc::clone(&self.source));
                self.current += 1;
                Entry::WhiteSpace(WhiteSpace::NewLine(NewLine(span)))
            }
            '\u{000D}' => {
                let span = Span::new(self.current, self.current + 1, Rc::clone(&self.source));
                self.current += 1;
                Entry::WhiteSpace(WhiteSpace::CarriageReturn(CarriageReturn(span)))
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
