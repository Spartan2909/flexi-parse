use super::token::Ident;
use super::token::Literal;
use super::TokenStream;
use super::token::LiteralValue;
use super::token::SingleCharPunct;
use super::token::PunctKind;
use super::token::Spacing;
use super::TokenTree;
use super::error::Error;
use super::error::ErrorKind;
use super::SourceFile;
use super::Span;

fn valid_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

#[derive(Debug)]
struct Scanner<'src> {
    current: usize,
    errors: Error<'src>,
    source: &'src SourceFile,
}

impl<'src> Scanner<'src> {
    fn scan(mut self) -> (TokenStream<'src>, Option<Error<'src>>) {
        let mut tokens = vec![];

        self.skip_whitespace();

        while !self.is_at_end() {
            if let Some(token) = self.scan_token() {
                tokens.push(token);
            } else {
                tokens.push(TokenTree::Error(todo!()));
                break;
            }

            self.skip_whitespace();
        }

        let errors = if self.errors.is_empty() {
            None
        } else {
            Some(self.errors)
        };

        (TokenStream { tokens, location: 0, source_file: self.source }, errors)
    }

    fn scan_token(&mut self) -> Option<TokenTree<'src>> {
        let token = match self.peek(0)? {
            c if PunctKind::try_from(c).is_ok() => {
                let kind = c.try_into().unwrap();
                let spacing = Spacing::from(self.peek(1)?);
                let span = Span::new(self.current, self.current + 1, self.source);
                self.current += 1;
                TokenTree::Punct(SingleCharPunct {
                    kind,
                    spacing,
                    span,
                })
            }
            c if c.is_ascii_digit() => {
                let start = self.current;
                while self.peek(0)?.is_ascii_digit() {
                    self.current += 1;
                }
                let value = if self.peek(0)? == '.' && self.peek(1)?.is_ascii_digit() {
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
                    span: Span::new(start, self.current, self.source),
                })
            }
            '"' => {
                let open = self.current;
                self.current += 1;
                while let Some(c) = self.peek(0) {
                    if c == '"' {
                        break;
                    } else {
                        self.current += 1;
                    }
                }
                if self.peek(0).is_none() {
                    self.errors.pop();
                    self.errors.pop();
                    self.errors.add(Error::new(
                        self.source,
                        ErrorKind::UnterminatedString(Span::new(
                            open,
                            self.source.contents.len(),
                            self.source,
                        )),
                    ));
                    return None;
                }
                let value =
                    LiteralValue::String(self.source.contents[open + 1..self.current].to_owned());
                self.current += 1;
                TokenTree::Literal(Literal {
                    value,
                    span: Span::new(open, self.current, self.source),
                })
            }
            '\'' => {
                let start = self.current;
                self.current += 1;
                let value = LiteralValue::Char(
                    self.source.contents[self.current..].chars().next().unwrap(),
                );
                self.current += 1;
                let span = Span::new(start, self.current + 1, self.source);
                if self.peek(0)? == '\'' {
                    self.current += 1;
                    TokenTree::Literal(Literal { value, span })
                } else {
                    self.errors
                        .add(Error::new(self.source, ErrorKind::UnterminatedChar(span)));
                    TokenTree::Error(todo!())
                }
            }
            c if c.is_alphabetic() || c == '_' => {
                let start = self.current;
                while valid_ident_char(self.peek(0)?) {
                    self.current += 1;
                }
                let string = &self.source.contents[start..self.current];
                let span = Span::new(start, self.current, self.source);
                
                        let string = if string.starts_with("__lang") {
                            let mut new_string = String::with_capacity(string.len() + 6);
                            new_string.push_str("__user");
                            new_string.push_str(string);
                            new_string
                        } else {
                            string.to_owned()
                        };
                        TokenTree::Ident(Ident { string, span })

            }
            ')' | ']' | '}' => {
                self.errors.add(Error::new(
                    self.source,
                    ErrorKind::UnopenedDelimiter(Span::new(
                        self.current,
                        self.current + 1,
                        self.source,
                    )),
                ));
                self.current += 1;
                TokenTree::Error(todo!())
            }
            _ => {
                self.errors.add(Error::new(
                    self.source,
                    ErrorKind::UnknownCharacter(Span::new(
                        self.current,
                        self.current + 1,
                        self.source,
                    )),
                ));
                self.current += 1;
                TokenTree::Error(todo!())
            }
        };

        Some(token)
    }

    fn peek(&mut self, offset: usize) -> Option<char> {
        if self.current + offset >= self.source.contents.len() {
            self.errors.add(Error::new(
                self.source,
                ErrorKind::EndOfFile(self.source.contents.len()),
            ));
            None
        } else {
            self.source.contents[self.current + offset..self.current + offset + 1]
                .chars()
                .next() // Will always be `Some`
        }
    }

    fn is_at_end(&mut self) -> bool {
        self.current >= self.source.contents.len()
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            let c = self.peek(0).unwrap();
            match c {
                '/' if self.peek(1) == Some('/') => {
                    self.current += 2;
                    while self.peek(0) != Some('\n') {
                        self.current += 1;
                    }
                }
                c if c.is_whitespace() => self.current += 1,
                _ => break,
            }
        }
    }
}

pub(crate) fn scan(source: &SourceFile) -> (TokenStream, Option<Error>) {
    let (tokens, errors) = Scanner {
        current: 0,
        errors: Error::empty(),
        source,
    }
    .scan();
    (tokens, errors)
}
