//! Utilities for parsing types separated by punctuation.

use crate::token::Punct;
use crate::Parse;
use crate::ParseStream;
use crate::Result;

use std::slice;
use std::vec;

/// A series of pairs of `T` and `P`, optionally followed by another `T`.
#[derive(Debug, Clone)]
pub struct Punctuated<T, P: Punct> {
    pairs: Vec<(T, P)>,
    end: Option<T>,
}

impl<T, P: Punct> Punctuated<T, P> {
    fn new() -> Punctuated<T, P> {
        Punctuated {
            pairs: vec![],
            end: None,
        }
    }

    fn push_value(&mut self, value: T) {
        self.end = Some(value);
    }

    fn push_punct(&mut self, punct: P) {
        let value = self.end.take().unwrap();
        self.pairs.push((value, punct));
    }

    /// Parses instances of `T` separated by instances of `P`, with no trailing
    /// punctuation.
    ///
    /// Note that this will stop as soon as it encounters a token that doesn't
    /// fit this pattern.
    pub fn parse_separated(input: ParseStream<'_>) -> Result<Self>
    where
        T: Parse,
    {
        Self::parse_separated_with(input, T::parse)
    }

    /// Parses instances of `T` using `f`, separated by instances of `P`, with
    /// no trailing punctuation.
    ///
    /// Note that this will stop as soon as it encounters a token that doesn't
    /// fit this pattern.
    pub fn parse_separated_with<F: FnMut(ParseStream<'_>) -> Result<T>>(
        input: ParseStream<'_>,
        mut f: F,
    ) -> Result<Self> {
        let mut punctuated = Punctuated::new();
        punctuated.push_value(f(input)?);

        while P::peek(input) {
            punctuated.push_punct(input.parse()?);
            punctuated.push_value(f(input)?);
        }

        Ok(punctuated)
    }

    /// Parses instances of `T` separated by instances of `P`, with optional
    /// trailing punctuation.
    ///
    /// Note that this will stop as soon as it encounters a token that doesn't
    /// fit this pattern.
    pub fn parse_separated_trailing(input: ParseStream<'_>) -> Result<Self>
    where
        T: Parse,
    {
        Self::parse_separated_trailing_with(input, T::parse)
    }

    /// Parses instances of `T` using `f`, separated by instances of `P`, with
    /// optional trailing punctuation.
    ///
    /// Note that this will stop as soon as it encounters a token that doesn't
    /// fit this pattern.
    pub fn parse_separated_trailing_with<F: FnMut(ParseStream<'_>) -> Result<T>>(
        input: ParseStream<'_>,
        mut f: F,
    ) -> Result<Self> {
        let mut punctuated = Punctuated::new();

        loop {
            if input.is_empty() {
                break;
            }
            punctuated.push_value(f(input)?);
            if input.is_empty() {
                break;
            }
            punctuated.push_punct(input.parse()?);
        }

        Ok(punctuated)
    }

    /// Parses instances of `T` separated by instances of `P`, with trailing
    /// punctuation.
    ///
    /// Note that this attempts to consume the entire stream.
    pub fn parse_terminated(input: ParseStream<'_>) -> Result<Self>
    where
        T: Parse,
    {
        Self::parse_terminated_with(input, T::parse)
    }

    // Parses instances of `T` using `f`, separated by instances of `P`, with
    /// trailing punctuation.
    ///
    /// Note that this attempts to consume the entire stream.
    pub fn parse_terminated_with<F: FnMut(ParseStream<'_>) -> Result<T>>(
        input: ParseStream<'_>,
        mut f: F,
    ) -> Result<Self> {
        let mut punctuated = Punctuated::new();

        while !input.is_empty() {
            punctuated.push_value(f(input)?);
            punctuated.push_punct(input.parse()?);
        }

        Ok(punctuated)
    }

    /// Returns an iterator over the values in this struct.
    pub fn iter(&self) -> Iter<T, P> {
        Iter {
            main: self.pairs.iter(),
            end: self.end.as_ref(),
        }
    }

    /// Returns an iterator that allows modifying each value.
    pub fn iter_mut(&mut self) -> IterMut<T, P> {
        IterMut {
            main: self.pairs.iter_mut(),
            end: self.end.as_mut(),
        }
    }

    /// Returns an iterator over the pairs of values and punctuation in this
    /// struct.
    pub fn pairs(&self) -> Pairs<T, P> {
        Pairs {
            main: self.pairs.iter(),
            end: self.end.as_ref(),
        }
    }

    /// Returns an iterator that allows modifying each pair.
    pub fn pairs_mut(&mut self) -> PairsMut<T, P> {
        PairsMut {
            main: self.pairs.iter_mut(),
            end: self.end.as_mut(),
        }
    }

    /// Returns a consuming iterator over the pairs in this struct.
    pub fn into_pairs(self) -> IntoPairs<T, P> {
        IntoPairs {
            main: self.pairs.into_iter(),
            end: self.end,
        }
    }
}

impl<T, P: Punct> IntoIterator for Punctuated<T, P> {
    type Item = T;
    type IntoIter = IntoIter<T, P>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            main: self.pairs.into_iter(),
            end: self.end,
        }
    }
}

/// An iterator over `&T`.
pub struct Iter<'a, T, P> {
    main: slice::Iter<'a, (T, P)>,
    end: Option<&'a T>,
}

impl<'a, T, P> Iterator for Iter<'a, T, P> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((next, _)) = self.main.next() {
            return Some(next);
        }
        self.end.take()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl<'a, T, P> DoubleEndedIterator for Iter<'a, T, P> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.end.take() {
            return Some(next);
        }
        self.main.next_back().map(|(value, _)| value)
    }
}

impl<'a, T, P> ExactSizeIterator for Iter<'a, T, P> {
    fn len(&self) -> usize {
        self.main.len() + usize::from(self.end.is_some())
    }
}

/// An iterator over `&mut T`.
pub struct IterMut<'a, T, P> {
    main: slice::IterMut<'a, (T, P)>,
    end: Option<&'a mut T>,
}

impl<'a, T, P> Iterator for IterMut<'a, T, P> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((next, _)) = self.main.next() {
            return Some(next);
        }
        self.end.take()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl<'a, T, P> DoubleEndedIterator for IterMut<'a, T, P> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.end.take() {
            return Some(next);
        }
        self.main.next_back().map(|(value, _)| value)
    }
}

impl<'a, T, P> ExactSizeIterator for IterMut<'a, T, P> {
    fn len(&self) -> usize {
        self.main.len() + usize::from(self.end.is_some())
    }
}

/// An iterator over `T`.
pub struct IntoIter<T, P> {
    main: vec::IntoIter<(T, P)>,
    end: Option<T>,
}

impl<T, P> Iterator for IntoIter<T, P> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((next, _)) = self.main.next() {
            return Some(next);
        }
        self.end.take()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl<T, P> DoubleEndedIterator for IntoIter<T, P> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.end.take() {
            return Some(next);
        }
        self.main.next_back().map(|(value, _)| value)
    }
}

impl<T, P> ExactSizeIterator for IntoIter<T, P> {
    fn len(&self) -> usize {
        self.main.len() + usize::from(self.end.is_some())
    }
}

/// A punctuated pair.
#[derive(Debug, Clone, Copy)]
pub enum Pair<T, P> {
    /// A value of type `T` followed by a piece of punctuation of type `P`.
    Punctuated(T, P),
    /// A value of type `T`.
    End(T),
}

impl<T, P> Pair<T, P> {
    /// Converts the pair into the inner value.
    pub fn into_value(self) -> T {
        match self {
            Pair::Punctuated(value, _) => value,
            Pair::End(value) => value,
        }
    }
}

/// An iterator over `Pair(&T, &P)`.
pub struct Pairs<'a, T, P> {
    main: slice::Iter<'a, (T, P)>,
    end: Option<&'a T>,
}

impl<'a, T, P> Iterator for Pairs<'a, T, P> {
    type Item = Pair<&'a T, &'a P>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((value, punct)) = self.main.next() {
            return Some(Pair::Punctuated(value, punct));
        }
        self.end.take().map(Pair::End)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl<'a, T, P> DoubleEndedIterator for Pairs<'a, T, P> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if let Some(value) = self.end.take() {
            return Some(Pair::End(value));
        }
        self.main
            .next_back()
            .map(|(value, punct)| Pair::Punctuated(value, punct))
    }
}

impl<'a, T, P> ExactSizeIterator for Pairs<'a, T, P> {
    fn len(&self) -> usize {
        self.main.len() + usize::from(self.end.is_some())
    }
}

/// An iterator over `Pair(&mut T, &mut P)`.
pub struct PairsMut<'a, T, P> {
    main: slice::IterMut<'a, (T, P)>,
    end: Option<&'a mut T>,
}

impl<'a, T, P> Iterator for PairsMut<'a, T, P> {
    type Item = Pair<&'a mut T, &'a mut P>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((value, punct)) = self.main.next() {
            return Some(Pair::Punctuated(value, punct));
        }
        self.end.take().map(Pair::End)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl<'a, T, P> DoubleEndedIterator for PairsMut<'a, T, P> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if let Some(value) = self.end.take() {
            return Some(Pair::End(value));
        }
        self.main
            .next_back()
            .map(|(value, punct)| Pair::Punctuated(value, punct))
    }
}

impl<'a, T, P> ExactSizeIterator for PairsMut<'a, T, P> {
    fn len(&self) -> usize {
        self.main.len() + usize::from(self.end.is_some())
    }
}

/// An iterator over `Pair(T, P)`.
pub struct IntoPairs<T, P> {
    main: vec::IntoIter<(T, P)>,
    end: Option<T>,
}

impl<T, P> Iterator for IntoPairs<T, P> {
    type Item = Pair<T, P>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((value, punct)) = self.main.next() {
            return Some(Pair::Punctuated(value, punct));
        }
        self.end.take().map(Pair::End)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.len(), Some(self.len()))
    }
}

impl<T, P> DoubleEndedIterator for IntoPairs<T, P> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if let Some(value) = self.end.take() {
            return Some(Pair::End(value));
        }
        self.main
            .next_back()
            .map(|(value, punct)| Pair::Punctuated(value, punct))
    }
}

impl<T, P> ExactSizeIterator for IntoPairs<T, P> {
    fn len(&self) -> usize {
        self.main.len() + usize::from(self.end.is_some())
    }
}
