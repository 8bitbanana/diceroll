use std::{iter::Enumerate, ops::Deref};

use super::lexer::Token;
use nom::{Compare, CompareResult, InputIter, InputLength, InputTake, Needed};

#[derive(Debug, Clone, PartialEq)]
pub struct Tokens<'a>(&'a [Token]);

impl<'a> Tokens<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self(tokens)
    }
}

impl<'a> Deref for Tokens<'a> {
    type Target = &'a [Token];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> InputLength for Tokens<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.len()
    }
}

impl<'a> InputTake for Tokens<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        Self(&self[0..count])
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.split_at(count);
        let first = Tokens (prefix);
        let second = Tokens (suffix);
        (second, first)
    }
}

impl<'a> InputIter for Tokens<'a> {
    type Item = &'a Token;

    type Iter = Enumerate<::std::slice::Iter<'a, Token>>;

    type IterElem = ::std::slice::Iter<'a, Token>;

    #[inline]
    fn iter_indices(&self) -> Self::Iter {
        self.iter().enumerate()
    }

    #[inline]
    fn iter_elements(&self) -> Self::IterElem {
        self.iter()
    }

    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
  where
    P: Fn(Self::Item) -> bool {
        self.iter().position(predicate)
    }

    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        if self.len() >= count {
            Ok(count)
        } else {
            Err(Needed::new(count - self.len()))
        }
    }
}

impl<'a> Compare<Tokens<'a>> for Tokens<'a> {
    #[inline]
    fn compare(&self, t: Tokens<'a>) -> nom::CompareResult {
        let pos = self.iter().zip(t.iter()).position(|(a,b)| a != b);
        match pos {
            Some(_) => CompareResult::Error,
            None => {
                if self.len() >= t.len() {
                    CompareResult::Ok
                } else {
                    CompareResult::Incomplete
                }
            }
        }
    }

    #[inline]
    fn compare_no_case(&self, t: Tokens<'a>) -> nom::CompareResult {
        self.compare(t)
    }
}