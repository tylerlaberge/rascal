use std::str::Chars;
use std::iter::{Iterator, Peekable};
use itertools::PeekingNext;

pub struct Source<'a> {
    chars: Peekable<Chars<'a>>,
    current_char: Option<char>
}

impl<'a> Source<'a> {
    pub fn new (raw_text: &'a str) -> Source<'a> {
        return Source {
            chars: raw_text.chars().peekable(),
            current_char: None,
        };
    }

    pub fn current_char(&self) -> Option<char> {
        return self.current_char;
    }

    pub fn peek(&mut self) -> Option<&char> {
        return self.chars.peek();
    }
}

impl<'a> Iterator for Source<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.current_char = self.chars.next();

        return self.current_char;
    }
}

impl<'a> PeekingNext for Source<'a> {
    fn peeking_next<F>(&mut self, accept: F) -> Option<Self::Item>
        where F: FnOnce(&Self::Item) -> bool
    {
        let pass = match self.peek() {
            Some(c) if accept(c) => true,
            _                    => false
        };

        return if pass { self.next() } else { None };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next() {
        let mut source = Source::new("this is a test");

        assert_eq!(source.next(), Some('t'));
        assert_eq!(source.next(), Some('h'));
        assert_eq!(source.next(), Some('i'));
        assert_eq!(source.next(), Some('s'));
        assert_eq!(source.next(), Some(' '));
        assert_eq!(source.next(), Some('i'));
        assert_eq!(source.next(), Some('s'));
        assert_eq!(source.next(), Some(' '));
        assert_eq!(source.next(), Some('a'));
        assert_eq!(source.next(), Some(' '));
        assert_eq!(source.next(), Some('t'));
        assert_eq!(source.next(), Some('e'));
        assert_eq!(source.next(), Some('s'));
        assert_eq!(source.next(), Some('t'));
        assert_eq!(source.next(), None);
    }

    #[test]
    #[allow(unused_variables)]
    fn peeking_next() {
        let mut source = Source::new("test");

        assert_eq!(source.peeking_next(| c | false), None);
        assert_eq!(source.peeking_next(| c | true), Some('t'));
        assert_eq!(source.peeking_next(| c | false), None);
        assert_eq!(source.peeking_next(| c | true), Some('e'));
        assert_eq!(source.peeking_next(| c | true), Some('s'));
        assert_eq!(source.peeking_next(| c | false), None);
        assert_eq!(source.peeking_next(| c | true), Some('t'));
        assert_eq!(source.peeking_next(| c | false), None);
        assert_eq!(source.peeking_next(| c | true), None);
    }

    #[test]
    fn current_char() {
        let mut source = Source::new("test");

        assert_eq!(source.current_char(), None);
        source.next();
        assert_eq!(source.current_char(), Some('t'));
        source.next();
        assert_eq!(source.current_char(), Some('e'));
        source.next();
        assert_eq!(source.current_char(), Some('s'));
        source.next();
        assert_eq!(source.current_char(), Some('t'));
        source.next();
        assert_eq!(source.current_char(), None);
    }

    #[test]
    fn peek() {
        let mut source = Source::new("test");

        assert_eq!(source.peek(), Some(&'t'));
        assert_eq!(source.next(), Some('t'));

        assert_eq!(source.peek(), Some(&'e'));
        assert_eq!(source.next(), Some('e'));

        assert_eq!(source.peek(), Some(&'s'));
        assert_eq!(source.next(), Some('s'));

        assert_eq!(source.peek(), Some(&'t'));
        assert_eq!(source.next(), Some('t'));
    }
}