use super::source::Source;
use super::token::Token;
use super::token::TokenCache;

use itertools::Itertools;

pub struct Lexer<'a> {
    source: Source<'a>,
    token_cache: TokenCache
}

impl<'a> Lexer<'a> {

    pub fn new(source: Source) -> Lexer {
        let token_cache = TokenCache::new();
        let mut lexer = Lexer { source, token_cache} ;
        lexer.init();

        return lexer;
    }

    pub fn peek(&self) -> Option<&Token> {
        return self.token_cache.peek();
    }

    fn init(&mut self) {
        loop {
            match self.compute_next_token() {
                Token::EOF => {
                    self.token_cache.push(Token::EOF);
                    break;
                }
                token      => self.token_cache.push(token)
            }
        }
    }

    fn compute_next_token(&mut self) -> Token {
        return match self.source.next() {
            Some(character) if character.is_whitespace() => self.compute_next_token(),
            Some(character)                              => self.lex(character),
            None                                         => Token::EOF
        };
    }

    fn integer(&mut self) -> u32 {
        let start_int: String = self.source.current_char().expect("Internal Lexer Error").to_string();

        return self.source.by_ref()
            .peeking_take_while(| c: &char | c.is_digit(10))
            .fold(start_int,| mut acc: String, num: char | {
                acc.push(num);
                return acc;
            })
            .parse::<u32>()
            .expect("Internal Lexer Error");
    }

    fn lex(&mut self, character: char) -> Token {
        return match character {
            x if x.is_digit(10) => Token::INTEGER(self.integer()),
            '+'                 => Token::PLUS,
            '-'                 => Token::MINUS,
            '*'                 => Token::MULTIPLY,
            '/'                 => Token::DIVIDE,
            '('                 => Token::LPAREN,
            ')'                 => Token::RPAREN,
            _                   => panic!("Unknown Token: '{}'", character)
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        return self.token_cache.next();
    }
}

