#[derive(Debug, PartialEq)]
#[allow(non_camel_case_types)]
pub enum Token {
    PROGRAM,
    PROCEDURE,
    FUNCTION,
    VAR,
    COMMA,
    COLON,
    INTEGER,
    REAL,
    STRING,
    BEGIN,
    END,
    DOT,
    SEMI,
    ASSIGN,
    ID(String),
    PLUS,
    MINUS,
    MULTIPLY,
    INTEGER_DIV,
    FLOAT_DIV,
    INTEGER_CONST(i32),
    REAL_CONST(f32),
    STRING_LITERAL(String),
    LPAREN,
    RPAREN,
    EOF,
}

#[derive(Debug)]
pub struct TokenCache {
    tokens: Vec<Token>,
}

impl TokenCache {
    pub fn new() -> TokenCache {
        return TokenCache { tokens: vec![] };
    }

    pub fn push(&mut self, token: Token) {
        self.tokens.push(token);
    }

    pub fn peek(&self) -> Option<&Token> {
        return self.tokens.get(0);
    }

    pub fn peek_ahead(&self, ahead: usize) -> Option<&Token> {
        return self.tokens.get(ahead);
    }
}

impl Iterator for TokenCache {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        return if self.tokens.is_empty() { None } else { Some(self.tokens.remove(0)) };
    }
}