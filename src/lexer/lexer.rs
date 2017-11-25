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

    pub fn peek_ahead(&self, ahead: usize) -> Option<&Token> {
        return self.token_cache.peek_ahead(ahead);
    }

    fn init(&mut self) {
        loop {
            match self.lex() {
                Ok(Token::EOF) => {
                    self.token_cache.push(Token::EOF);
                    break;
                }
                Ok(token)      => self.token_cache.push(token),
                Err(e)         => panic!(e)
            }
        }
    }

    fn number(&mut self) -> Result<i32, String> {
        let start_int: String = match self.source.current_char() {
            Some(c) if c.is_digit(10) => Ok(c),
            _                         => Err("Internal Lexer Error, expected number")
        }?.to_string();

        let final_int = self.source.by_ref()
            .peeking_take_while(| c: &char | c.is_digit(10))
            .fold(start_int,| mut acc: String, next_int: char | {
                acc.push(next_int);
                return acc;
            })
            .parse::<i32>()
            .or(Err("Internal Lexer Error, failed to parse integer"))?;

        return Ok(final_int);
    }

    fn integer(&mut self) -> Result<Token, String> {
        let integer = self.number()?;

        if let Some(&'.') = self.source.peek() {
            self.source.next(); // Eat the period

            let decimal = match self.source.next() {
                Some(c) if c.is_digit(10) => self.number(),
                _                         => Err(String::from("Expected floating point"))
            }?;

            let mut string_real: String = integer.to_string();
            string_real.push('.');
            string_real.push_str(decimal.to_string().as_str());

            let real = string_real.parse::<f32>().or(Err("Internal Lexer Error: Failed to parse float"))?;

            return Ok(Token::REAL_CONST(real));
        }

        else {
            return Ok(Token::INTEGER_CONST(integer));
        }
    }

    fn id(&mut self) -> Result<Token, String> {
        let start_id: String = match self.source.current_char() {
            Some(c) if c.is_alphabetic() => Ok(c),
            _                            => Err("Internal Lexer Error")
        }?.to_string();
        let final_id: String = self.source.by_ref()
            .peeking_take_while(| c: &char | c.is_alphanumeric() || c == &'_')
            .fold(start_id, | mut acc: String, next_id: char | {
                acc.push(next_id);
                return acc;
            });

        return match final_id.to_lowercase().as_str() {
            "program"   => Ok(Token::PROGRAM),
            "procedure" => Ok(Token::PROCEDURE),
            "function"  => Ok(Token::FUNCTION),
            "begin"     => Ok(Token::BEGIN),
            "end"       => Ok(Token::END),
            "var"       => Ok(Token::VAR),
            "integer"   => Ok(Token::INTEGER),
            "real"      => Ok(Token::REAL),
            "string"    => Ok(Token::STRING),
            "boolean"   => Ok(Token::BOOLEAN),
            "true"      => Ok(Token::BOOLEAN_CONST(true)),
            "false"     => Ok(Token::BOOLEAN_CONST(false)),
            "and"       => Ok(Token::AND),
            "or"        => Ok(Token::OR),
            "not"       => Ok(Token::NOT),
            "div"       => Ok(Token::INTEGER_DIV),
            "if"        => Ok(Token::IF),
            "then"      => Ok(Token::THEN),
            "else"      => Ok(Token::ELSE),
            _           => Ok(Token::ID(final_id))
        };
    }

    fn string(&mut self) -> Result<Token, String> {
        match self.source.current_char() {
            Some('\'') => Ok(()),
            _          => Err("Internal Lexer Error")
        }?;

        let final_string: String = self.source.by_ref()
            .peeking_take_while(| c: &char | c != &'\'')
            .fold(String::from(""), | mut acc: String, next_char: char | {
                acc.push(next_char);
                return acc;
            });

        match self.source.next() {
            Some('\'') => Ok(()),
            _          => Err("Internal Lexer Error")
        }?;

        return Ok(Token::STRING_LITERAL(final_string));
    }

    fn assign(&mut self) -> Result<Token, String> {
        return match (self.source.current_char(), self.source.next()) {
            (Some(':'), Some('=')) => Ok(Token::ASSIGN),
            (Some(':'), Some(c))   => Err(format!("Expected '=', found {}", c)),
            _                      => Err(String::from("Internal Lexer Error"))
        };
    }

    fn comparison(&mut self) -> Result<Token, String> {
        return match self.source.current_char() {
            Some('<') => match self.source.peek() {
                Some(&'=') => {
                    self.source.next();
                    Ok(Token::LESS_THAN_OR_EQUAL)
                },
                Some(&'>') => {
                    self.source.next();
                    Ok(Token::NOT_EQUAL)
                },
                _          => Ok(Token::LESS_THAN)
            },
            Some('>') => match self.source.peek() {
                Some(&'=') => {
                    self.source.next();
                    Ok(Token::GREATER_THAN_OR_EQUAL)
                },
                _          => Ok(Token::GREATER_THAN)
            },
            Some('=') => Ok(Token::EQUAL),
            _         => Err(String::from("Internal Lexer Error"))
        };
    }

    fn lex(&mut self) -> Result<Token, String> {
        return match self.source.next() {
            Some(character) if character.is_whitespace() => self.lex(),
            Some(character) if character.is_digit(10)    => self.integer(),
            Some(character) if character.is_alphabetic() => self.id(),
            Some(':')                                    => match self.source.peek() {
                Some(&'=') => self.assign(),
                _          => Ok(Token::COLON)
            },
            Some('\'')                                   => self.string(),
            Some('<')                                    => self.comparison(),
            Some('>')                                    => self.comparison(),
            Some('=')                                    => self.comparison(),
            Some(',')                                    => Ok(Token::COMMA),
            Some('.')                                    => Ok(Token::DOT),
            Some(';')                                    => Ok(Token::SEMI),
            Some('+')                                    => Ok(Token::PLUS),
            Some('-')                                    => Ok(Token::MINUS),
            Some('*')                                    => Ok(Token::MULTIPLY),
            Some('/')                                    => Ok(Token::FLOAT_DIV),
            Some('(')                                    => Ok(Token::LPAREN),
            Some(')')                                    => Ok(Token::RPAREN),
            None                                         => Ok(Token::EOF),
            Some(character)                              => Err(format!("Unknown Token: '{}'", character)),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        return self.token_cache.next();
    }
}

