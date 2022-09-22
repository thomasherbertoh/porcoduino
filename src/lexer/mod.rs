use crate::tokens::{Operator, Token, TokenType, Value};

pub struct Lexer {
    content: String,
    counter: usize,
}

impl Lexer {
    pub fn new(content: String) -> Self {
        Self {
            content,
            counter: 0,
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut depth = 0;

        let mut tokens = Vec::new();

        while self.counter < self.content.len() {
            let c = self.curr_char();

            match c {
                '=' => {
                    tokens.push(Token::new(
                        TokenType::Operator(Operator::Assignment),
                        depth,
                        Some("assign".to_string()),
                        None,
                    ));
                    self.counter += 1;
                }
                '\'' => {
                    self.counter += 1;
                    tokens.push(Token::new(
                        TokenType::Value(Value::Char(self.curr_char())),
                        depth,
                        Some(self.curr_char().to_string()),
                        None,
                    ));

                    // point to char after closing quote
                    self.counter += 2;
                }
                '"' => {
                    self.counter += 1;
                    let mut buff = String::new();
                    while self.curr_char() != '"' {
                        buff.push(self.curr_char());
                        self.counter += 1;
                    }
                    tokens.push(Token::new(
                        TokenType::Value(Value::String(buff.clone())),
                        depth,
                        Some(buff),
                        None,
                    ));
                    self.counter += 1;
                }
                '+' => {
                    if let Some(t) = tokens.last() {
                        if t.t_type != TokenType::Operator(Operator::Subtraction)
                            && t.t_type != TokenType::Operator(Operator::Addition)
                        {
                            tokens.push(Token::new(
                                TokenType::Operator(Operator::Addition),
                                depth,
                                Some("add".to_string()),
                                None,
                            ));
                        }
                    } else {
                        tokens.push(Token::new(
                            TokenType::Operator(Operator::Addition),
                            depth,
                            Some("add".to_string()),
                            None,
                        ));
                    }
                    self.counter += 1;
                }
                '-' => {
                    if let Some(t) = tokens.last() {
                        if t.t_type == TokenType::Operator(Operator::Subtraction) {
                            tokens.pop();
                            tokens.push(Token::new(
                                TokenType::Operator(Operator::Addition),
                                depth,
                                Some("add".to_string()),
                                None,
                            ));
                        } else if t.t_type == TokenType::Operator(Operator::Addition) {
                            tokens.pop();
                            tokens.push(Token::new(
                                TokenType::Operator(Operator::Subtraction),
                                depth,
                                Some("subtract".to_string()),
                                None,
                            ));
                        } else {
                            tokens.push(Token::new(
                                TokenType::Operator(Operator::Subtraction),
                                depth,
                                Some("subtract".to_string()),
                                None,
                            ));
                        }
                    } else {
                        tokens.push(Token::new(
                            TokenType::Operator(Operator::Subtraction),
                            depth,
                            Some("subtract".to_string()),
                            None,
                        ));
                    }
                    self.counter += 1;
                }
                '*' => {
                    tokens.push(Token::new(
                        TokenType::Operator(Operator::Multiplication),
                        depth,
                        Some("multiply".to_string()),
                        None,
                    ));
                    self.counter += 1;
                }
                '/' => {
                    tokens.push(Token::new(
                        TokenType::Operator(Operator::Division),
                        depth,
                        Some("divide".to_string()),
                        None,
                    ));
                    self.counter += 1;
                }
                ';' => {
                    tokens.push(Token::new(
                        TokenType::End,
                        depth,
                        Some("statement_end".to_string()),
                        None,
                    ));
                    self.counter += 1;
                }
                '&' => {
                    tokens.push(Token::new(
                        TokenType::Operator(Operator::BitwiseAnd),
                        depth,
                        Some("bitwise_and".to_string()),
                        None,
                    ));
                    self.counter += 1;
                }
                '|' => {
                    tokens.push(Token::new(
                        TokenType::Operator(Operator::BitwiseOr),
                        depth,
                        Some("bitwise_or".to_string()),
                        None,
                    ));
                    self.counter += 1;
                }
                '{' => {
                    depth += 1;
                    self.counter += 1;
                }
                '}' => {
                    depth -= 1;
                    self.counter += 1;
                }
                _ if c.is_alphabetic() => {
                    let mut buff = String::from(c);
                    self.counter += 1;
                    while self.curr_char().is_alphabetic() || self.curr_char().eq(&'_') {
                        buff.push(self.curr_char());
                        self.counter += 1;
                    }

                    let token_type = match buff.as_str() {
                        "make" => TokenType::Make,
                        "true" => TokenType::Value(Value::Boolean(true)),
                        "false" => TokenType::Value(Value::Boolean(false)),
                        _ => TokenType::Identifier(buff.clone()),
                    };
                    tokens.push(Token::new(token_type, depth, Some(buff), None));
                }
                _ if c.is_numeric() => {
                    // maybe use bignum in future?
                    let mut res: i64 = 0;
                    while self.curr_char().is_numeric() {
                        res = res
                            .checked_mul(10)
                            .expect("[LEX] Integers can only be up to 64 bits")
                            .checked_add(self.curr_char().to_digit(10).unwrap() as i64)
                            .expect("[LEX] Integers can only be up to 64 bits");
                        self.counter += 1;
                    }
                    tokens.push(Token::new(
                        TokenType::Value(Value::Integer(res)),
                        depth,
                        Some("integer".to_string()),
                        Some(res),
                    ));
                }
                _ if c.is_whitespace() => self.counter += 1,
                _ => panic!("[LEX] Unrecognised symbol: `{}`", c),
            }
        }

        if depth > 0 {
            panic!("[LEX] {depth} unclosed block(s) at end of program");
        }
        tokens
    }

    fn curr_char(&self) -> char {
        self.content.chars().nth(self.counter).unwrap()
    }
}
