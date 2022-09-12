use std::collections::HashMap;

use crate::tokens::{Operator, Value};

pub trait EvalNode {
    fn eval_node(&self, map: &mut HashMap<String, Value>) -> Value;
}

#[derive(Clone, Debug)]
pub enum ASTNodes {
    ASTNode(ASTNode),
    ASTOpNode(ASTOpNode),
    ASTValNode(ASTValNode),
    ASTIdentifierNode(ASTIdentifierNode),
}

#[derive(Clone, Debug)]
pub struct ASTNode {
    pub left: Box<Option<ASTNodes>>,
    pub right: Box<Option<ASTNodes>>,
}

impl ASTNode {
    pub fn new(left: Box<Option<ASTNodes>>, right: Box<Option<ASTNodes>>) -> Self {
        Self { left, right }
    }

    pub fn default() -> Self {
        Self {
            left: Box::new(None),
            right: Box::new(None),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ASTOpNode {
    pub node: ASTNode,
    pub op: Operator,
}

impl ASTOpNode {
    pub fn new(left: Box<Option<ASTNodes>>, right: Box<Option<ASTNodes>>, op: Operator) -> Self {
        Self {
            node: ASTNode::new(left, right),
            op,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ASTValNode {
    pub val: Value,
}

impl ASTValNode {
    pub fn new(val: Value) -> Self {
        Self { val }
    }
}

#[derive(Clone, Debug)]
pub struct ASTIdentifierNode {
    pub name: String,
}

impl ASTIdentifierNode {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}
