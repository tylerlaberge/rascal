use itertools::Itertools;
use super::source::Source;
use super::token::{Token, TokenCache};

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
                Some(c)                   => Err(String::from(format!("Lexing Error: Expected floating point number at: {}.{}", integer, c))),
                None                      => Err(String::from(format!("Lexing Error: Expected floating point number at: {}.", integer)))
            }?;

            let mut string_real: String = integer.to_string();
            string_real.push('.');
            string_real.push_str(decimal.to_string().as_str());

            let real = string_real.parse::<f32>().or(Err("Internal Lexer Error: Failed to parse float"))?;

            return Ok(Token::REAL_CONST(real));
        } else {
            return Ok(Token::INTEGER_CONST(integer));
        }
    }

    fn id(&mut self) -> Result<Token, String> {
        let start_id: String = match self.source.current_char() {
            Some(c) if c.is_alphabetic() => Ok(c),
            _                            => Err("Internal Lexer Error, expected alphabetic character")
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
            _          => Err("Internal Lexer Error, expected '\'' character")
        }?;

        let final_string: String = self.source.by_ref()
            .peeking_take_while(| c: &char | c != &'\'')
            .fold(String::from(""), | mut acc: String, next_char: char | {
                acc.push(next_char);
                return acc;
            });

        match self.source.next() {
            Some('\'') => Ok(()),
            _          => Err("Internal Lexer Error, expected '\'' character")
        }?;

        return Ok(Token::STRING_LITERAL(final_string));
    }

    fn assign(&mut self) -> Result<Token, String> {
        return match (self.source.current_char(), self.source.next()) {
            (Some(':'), Some('=')) => Ok(Token::ASSIGN),
            _                      => Err(String::from("Internal Lexer Error: expected ':=' characters"))
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
            _         => Err(String::from("Internal Lexer Error: expected comparison character"))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn program_token() {
        let source = Source::new("program");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::PROGRAM));
    }

    #[test]
    fn procedure_token() {
        let source = Source::new("procedure");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::PROCEDURE));
    }

    #[test]
    fn function_token() {
        let source = Source::new("function");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::FUNCTION));
    }

    #[test]
    fn var_token() {
        let source = Source::new("var");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::VAR));
    }

    #[test]
    fn comma_token() {
        let source = Source::new(",");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::COMMA));
    }

    #[test]
    fn colon_token() {
        let source = Source::new(":");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::COLON));
    }

    #[test]
    fn integer_token() {
        let source = Source::new("integer");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::INTEGER));
    }

    #[test]
    fn real_token() {
        let source = Source::new("real");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::REAL));
    }

    #[test]
    fn string_token() {
        let source = Source::new("string");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::STRING));
    }

    #[test]
    fn boolean_token() {
        let source = Source::new("boolean");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::BOOLEAN));
    }

    #[test]
    fn begin_token() {
        let source = Source::new("begin");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::BEGIN));
    }

    #[test]
    fn end_token() {
        let source = Source::new("end");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::END));
    }

    #[test]
    fn dot_token() {
        let source = Source::new(".");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::DOT));
    }

    #[test]
    fn semi_token() {
        let source = Source::new(";");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::SEMI));
    }

    #[test]
    fn assign_token() {
        let source = Source::new(":=");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::ASSIGN));
    }

    #[test]
    fn id_token() {
        let source = Source::new("foobar");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::ID(String::from("foobar"))));
    }

    #[test]
    fn plus_token() {
        let source = Source::new("+");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::PLUS));
    }

    #[test]
    fn minus_token() {
        let source = Source::new("-");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::MINUS));
    }

    #[test]
    fn multiply_token() {
        let source = Source::new("*");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::MULTIPLY));
    }

    #[test]
    fn integer_div_token() {
        let source = Source::new("div");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::INTEGER_DIV));
    }

    #[test]
    fn float_div_token() {
        let source = Source::new("/");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::FLOAT_DIV));
    }

    #[test]
    fn and_token() {
        let source = Source::new("and");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::AND));
    }

    #[test]
    fn or_token() {
        let source = Source::new("or");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::OR));
    }

    #[test]
    fn not_token() {
        let source = Source::new("not");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::NOT));
    }

    #[test]
    fn less_than_token() {
        let source = Source::new("<");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::LESS_THAN));
    }

    #[test]
    fn greater_than_token() {
        let source = Source::new(">");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::GREATER_THAN));
    }

    #[test]
    fn equal_token() {
        let source = Source::new("=");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::EQUAL));
    }

    #[test]
    fn not_equal_token() {
        let source = Source::new("<>");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::NOT_EQUAL));
    }

    #[test]
    fn less_than_or_equal_token() {
        let source = Source::new("<=");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::LESS_THAN_OR_EQUAL));
    }

    #[test]
    fn greater_than_or_equal_token() {
        let source = Source::new(">=");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::GREATER_THAN_OR_EQUAL));
    }

    #[test]
    fn integer_const_token() {
        let source = Source::new("5");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::INTEGER_CONST(5)));
    }

    #[test]
    fn real_const_token() {
        let source = Source::new("5.5");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::REAL_CONST(5.5)));
    }

    #[test]
    fn string_literal_token() {
        let source = Source::new("'hello world'");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::STRING_LITERAL(String::from("hello world"))));
    }

    #[test]
    fn boolean_const_token() {
        let source = Source::new("true");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::BOOLEAN_CONST(true)));
    }

    #[test]
    fn if_token() {
        let source = Source::new("if");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::IF));
    }

    #[test]
    fn then_token() {
        let source = Source::new("then");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::THEN));
    }

    #[test]
    fn else_token() {
        let source = Source::new("else");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::ELSE));
    }

    #[test]
    fn left_paren_token() {
        let source = Source::new("(");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::LPAREN));
    }

    #[test]
    fn right_paren_token() {
        let source = Source::new(")");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::RPAREN));
    }

    #[test]
    fn eof_token() {
        let source = Source::new("");
        let mut lexer = Lexer::new(source);

        assert_eq!(lexer.next(), Some(Token::EOF));
    }
}