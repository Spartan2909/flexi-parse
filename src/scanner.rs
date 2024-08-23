use crate::error::Error;
use crate::error::ErrorKind;
use crate::token::CarriageReturn;
use crate::token::Ident;
use crate::token::LitStrDoubleQuote;
use crate::token::LitStrSingleQuote;
use crate::token::NewLine;
use crate::token::PunctKind;
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

use std::sync::Arc;

fn valid_ident_char(c: Option<char>) -> bool {
    c.map_or(false, |c| c.is_alphanumeric() || c == '_')
}

#[derive(Debug)]
struct Scanner {
    current: usize,
    end: usize,
    errors: Error,
    source: Arc<SourceFile>,
}

impl Scanner {
    fn scan(mut self) -> (TokenStream, Option<Error>) {
        let mut tokens = vec![];

        while !self.is_at_end() {
            match self.scan_token() {
                Ok(Some(token)) => tokens.push(token),
                Ok(None) => {}
                Err(err) => {
                    self.errors.add(err);
                    tokens.push(Entry::Error(Span::new(0, 0, Arc::clone(&self.source))));
                    break;
                }
            }
        }

        let errors = if self.errors.is_empty() {
            None
        } else {
            Some(self.errors)
        };

        (TokenStream::new(tokens, Some(self.source)), errors)
    }

    fn scan_token(&mut self) -> Result<Option<Entry>> {
        let token = match self.peek(0)? {
            #[cfg(feature = "scan-strings")]
            '"' => {
                let start = self.current;
                let mut buf = String::new();
                self.current += 1;
                while self.peek(0)? != '"' {
                    buf.push(self.peek(0)?);
                    self.current += 1;
                }
                self.current += 1;

                let span = Span::try_new(start, self.current, Arc::clone(&self.source)).unwrap();

                Entry::LitStrDoubleQuote(LitStrDoubleQuote::new(buf, span))
            }
            #[cfg(feature = "scan-strings")]
            '\'' => {
                let start = self.current;
                let mut buf = String::new();
                self.current += 1;
                while self.peek(0)? != '\'' {
                    buf.push(self.peek(0)?);
                    self.current += 1;
                }
                self.current += 1;

                let span = Span::try_new(start, self.current, Arc::clone(&self.source)).unwrap();

                Entry::LitStrSingleQuote(LitStrSingleQuote::new(buf, span))
            }
            c if PunctKind::try_from(c).is_ok() => {
                let kind = c.try_into().unwrap();
                let span = Span::try_new(self.current, self.current + 1, Arc::clone(&self.source))
                    .unwrap();
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
                let string = self.source.contents()[start..self.current].to_string();
                let span = Span::try_new(start, self.current, Arc::clone(&self.source)).unwrap();

                Entry::Ident(Ident { string, span })
            }
            ' ' if self.peek(1).is_ok_and(|c| c == ' ') => {
                self.current += 2;
                Entry::WhiteSpace(WhiteSpace::Space2(Space2 {
                    span: Span::try_new(self.current - 2, self.current, Arc::clone(&self.source))
                        .unwrap(),
                }))
            }
            ' ' => {
                self.current += 1;
                return Ok(None);
            }
            '\t' => {
                let span = Span::try_new(self.current, self.current + 1, Arc::clone(&self.source))
                    .unwrap();
                self.current += 1;
                Entry::WhiteSpace(WhiteSpace::Tab(Tab { span }))
            }
            '\n' => {
                let span = Span::try_new(self.current, self.current + 1, Arc::clone(&self.source))
                    .unwrap();
                self.current += 1;
                Entry::WhiteSpace(WhiteSpace::NewLine(NewLine { span }))
            }
            '\u{000D}' => {
                let span = Span::try_new(self.current, self.current + 1, Arc::clone(&self.source))
                    .unwrap();
                self.current += 1;
                Entry::WhiteSpace(WhiteSpace::CarriageReturn(CarriageReturn { span }))
            }
            _ => {
                self.current += 1;
                return Err(Error::new(
                    Arc::clone(&self.source),
                    ErrorKind::UnknownCharacter(
                        Span::try_new(self.current, self.current + 1, Arc::clone(&self.source))
                            .unwrap(),
                    ),
                ));
            }
        };

        Ok(Some(token))
    }

    fn peek(&self, offset: usize) -> Result<char> {
        if self.current + offset >= self.source.contents().len() {
            Err(Error::new(
                Arc::clone(&self.source),
                ErrorKind::EndOfFile(self.source.contents().len()),
            ))
        } else {
            Ok(
                self.source.contents()[self.current + offset..=self.current + offset]
                    .chars()
                    .next()
                    .unwrap(),
            )
        }
    }

    const fn is_at_end(&self) -> bool {
        self.current >= self.end
    }
}

pub(crate) fn scan(
    source: Arc<SourceFile>,
    start: usize,
    end: Option<usize>,
) -> (TokenStream, Option<Error>) {
    let (tokens, errors) = Scanner {
        current: start,
        end: end.unwrap_or_else(|| source.contents().len()),
        errors: Error::empty(),
        source,
    }
    .scan();
    (tokens, errors)
}
