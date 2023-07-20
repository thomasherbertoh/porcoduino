use crate::{
    evaluator::Evaluator,
    tokens::{Operator, TokenType, Value},
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
    ASTProcNode(ASTProcNode),
    ASTParamNode(ASTParamNode),
}

#[derive(Clone, Debug, Default)]
pub struct ASTNode {
    pub left: Box<Option<ASTNodes>>,
    pub right: Box<Option<ASTNodes>>,
    pub depth: Option<u64>,
}

impl ASTNode {
    pub fn new(
        left: Box<Option<ASTNodes>>,
        right: Box<Option<ASTNodes>>,
        depth: Option<u64>,
    ) -> Self {
        Self { left, right, depth }
    }
}

#[derive(Clone, Debug)]
pub struct ASTOpNode {
    pub node: ASTNode,
    pub op: Operator,
    pub depth: Option<u64>,
}

impl ASTOpNode {
    pub fn new(
        left: Box<Option<ASTNodes>>,
        right: Box<Option<ASTNodes>>,
        op: Operator,
        depth: Option<u64>,
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
    pub depth: Option<u64>,
    pub is_ret: bool,
}

impl ASTValNode {
    pub fn new(val: Value, depth: Option<u64>, is_ret: bool) -> Self {
        Self { val, depth, is_ret }
    }
}

#[derive(Clone, Debug)]
pub struct ASTIdentifierNode {
    pub name: String,
    pub depth: Option<u64>,
    pub is_declaration: bool,
    pub is_ret: bool,
}

impl ASTIdentifierNode {
    pub fn new(name: String, depth: Option<u64>, is_declaration: bool, is_ret: bool) -> Self {
        Self {
            name,
            depth,
            is_declaration,
            is_ret,
        }
    }

    pub fn set_depth(&mut self, depth: Option<u64>) {
        self.depth = depth;
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

#[derive(Clone, Debug)]
pub struct ASTProcNode {
    pub name: String,
    pub body: Box<Option<ASTNodes>>,
    pub depth: Option<u64>,
    pub is_declaration: bool,
    pub params: Vec<ASTParamNode>, // in the declaration
    pub args: Vec<ASTNodes>,       // passed values
    pub ret_type: Option<TokenType>,
}

impl ASTProcNode {
    pub fn new(
        name: String,
        body: Box<Option<ASTNodes>>,
        depth: Option<u64>,
        is_declaration: bool,
        params: Vec<ASTParamNode>,
        args: Vec<ASTNodes>,
        ret_type: Option<TokenType>,
    ) -> Self {
        Self {
            name,
            body,
            depth,
            is_declaration,
            params,
            args,
            ret_type,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ASTParamNode {
    pub name: String,
    pub param_type: TokenType,
}

impl ASTParamNode {
    pub fn new(name: String, param_type: TokenType) -> Self {
        Self { name, param_type }
    }
}
