use std::str::Chars;
use std::iter::Iterator;
use std::iter::Peekable;

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