use crate::{
    evaluator::Evaluator,
    tokens::{Operator, Value},
};

pub trait EvalNode {
    fn eval_node(&self, evaluator: &mut Evaluator) -> Value;
}

#[derive(Clone, Debug)]
pub enum ASTNodes {
    ASTNode(ASTNode),
    ASTOpNode(ASTOpNode),
    ASTValNode(ASTValNode),
    ASTIdentifierNode(ASTIdentifierNode),
    ASTBlockNode(ASTBlockNode),
}

#[derive(Clone, Debug, Default)]
pub struct ASTNode {
    pub left: Box<Option<ASTNodes>>,
    pub right: Box<Option<ASTNodes>>,
    pub depth: u64,
}

impl ASTNode {
    pub fn new(left: Box<Option<ASTNodes>>, right: Box<Option<ASTNodes>>, depth: u64) -> Self {
        Self { left, right, depth }
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
    pub depth: u64,
    pub is_ret: bool,
}

impl ASTValNode {
    pub fn new(val: Value, depth: u64, is_ret: bool) -> Self {
        Self { val, depth, is_ret }
    }
}

#[derive(Clone, Debug)]
pub struct ASTIdentifierNode {
    pub name: String,
    pub depth: u64,
    pub is_declaration: bool,
    pub is_ret: bool,
}

impl ASTIdentifierNode {
    pub fn new(name: String, depth: u64, is_declaration: bool, is_ret: bool) -> Self {
        Self {
            name,
            depth,
            is_declaration,
            is_ret,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ASTBlockNode {
    pub nodes: Vec<ASTNodes>,
    _id: usize, // index of `StartBlock` node denoting this block?
}

impl ASTBlockNode {
    pub fn new(nodes: Vec<ASTNodes>, id: usize) -> Self {
        Self { nodes, _id: id }
    }
}
