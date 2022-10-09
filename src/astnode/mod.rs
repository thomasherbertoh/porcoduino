use std::collections::HashMap;

use crate::tokens::{Operator, Value};

pub trait EvalNode {
    fn eval_node(&self, vars: &mut Vec<&mut HashMap<String, Value>>) -> Value;
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
    pub depth: u64,
}

impl ASTNode {
    pub fn new(left: Box<Option<ASTNodes>>, right: Box<Option<ASTNodes>>, depth: u64) -> Self {
        Self { left, right, depth }
    }

    pub fn default() -> Self {
        Self {
            left: Box::new(None),
            right: Box::new(None),
            depth: 0,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ASTOpNode {
    pub node: ASTNode,
    pub op: Operator,
    pub depth: u64,
}

impl ASTOpNode {
    pub fn new(
        left: Box<Option<ASTNodes>>,
        right: Box<Option<ASTNodes>>,
        op: Operator,
        depth: u64,
    ) -> Self {
        Self {
            node: ASTNode::new(left, right, depth),
            op,
            depth,
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
    pub depth: u64,
}

impl ASTIdentifierNode {
    pub fn new(name: String, depth: u64) -> Self {
        Self { name, depth }
    }
}
