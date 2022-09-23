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
    Integer(i64),
    Boolean(bool),
}

impl Value {
    pub fn get_type(&self) -> String {
        match self {
            Value::String(_) => "String",
            Value::Char(_) => "Char",
            Value::Integer(_) => "Integer",
            Value::Boolean(_) => "Boolean",
        }
        .to_string()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    BitwiseAnd,
    BitwiseOr,
    Equality,
    Inequality,
    Assignment,
}

#[derive(Debug)]
pub enum MathsOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub t_type: TokenType,
    pub code_depth: u64,
    pub wordy: Option<String>,
    pub numeric: Option<i64>,
}

impl Token {
    pub fn new(
        t_type: TokenType,
        code_depth: u64,
        wordy: Option<String>,
        numeric: Option<i64>,
    ) -> Self {
        Self {
            t_type,
            code_depth,
            wordy,
            numeric,
        }
    }
}
