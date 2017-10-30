#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum Token {
    PROGRAM,
    VAR,
    COMMA,
    COLON,
    INTEGER,
    REAL,
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
    INTEGER_CONST(u32),
    REAL_CONST(f32),
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
}

impl Iterator for TokenCache {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        return if self.tokens.is_empty() { None } else { Some(self.tokens.remove(0)) };
    }
}