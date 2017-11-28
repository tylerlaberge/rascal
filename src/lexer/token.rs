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
    BOOLEAN,
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
    AND,
    OR,
    NOT,
    LESS_THAN,
    GREATER_THAN,
    EQUAL,
    NOT_EQUAL,
    LESS_THAN_OR_EQUAL,
    GREATER_THAN_OR_EQUAL,
    INTEGER_CONST(i32),
    REAL_CONST(f32),
    STRING_LITERAL(String),
    BOOLEAN_CONST(bool),
    IF,
    THEN,
    ELSE,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn next() {
        let mut token_cache = TokenCache::new();

        token_cache.push(Token::BEGIN);
        token_cache.push(Token::END);

        assert_eq!(token_cache.next(), Some(Token::BEGIN));
        assert_eq!(token_cache.next(), Some(Token::END));
        assert_eq!(token_cache.next(), None)
    }

    #[test]
    fn peek() {
        let mut token_cache = TokenCache::new();

        token_cache.push(Token::BEGIN);
        token_cache.push(Token::END);

        assert_eq!(token_cache.peek(), Some(&Token::BEGIN));
        assert_eq!(token_cache.peek(), Some(&Token::BEGIN));
    }

    #[test]
    fn peek_ahead() {
        let mut token_cache = TokenCache::new();

        token_cache.push(Token::BEGIN);
        token_cache.push(Token::END);
        token_cache.push(Token::DOT);

        assert_eq!(token_cache.peek_ahead(0), Some(&Token::BEGIN));
        assert_eq!(token_cache.peek_ahead(1), Some(&Token::END));
        assert_eq!(token_cache.peek_ahead(2), Some(&Token::DOT));
        assert_eq!(token_cache.peek_ahead(3), None);
        assert_eq!(token_cache.peek_ahead(0), Some(&Token::BEGIN));
    }
}