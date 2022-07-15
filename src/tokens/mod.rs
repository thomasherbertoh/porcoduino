#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Identifier(String),
    Make,
    Value(Value),
    Operator(Operator),
    End,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    String(String),
    Char(char),
    Integer(u64),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Assignment,
}

#[derive(Debug)]
pub enum MathsOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub t_type: TokenType,
    pub code_depth: u64,
    pub wordy: Option<String>,
    pub numeric: Option<u64>,
}

impl Token {
    pub fn new(
        t_type: TokenType,
        code_depth: u64,
        wordy: Option<String>,
        numeric: Option<u64>,
    ) -> Self {
        Self {
            t_type,
            code_depth,
            wordy,
            numeric,
        }
    }
}
