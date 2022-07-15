use crate::{astnode::ASTNode, tokens::Token};

pub struct Parser {
    pub token_list: Vec<Token>,
    pub root: ASTNode,
}

impl Parser {
    pub fn new(t_list: &Vec<Token>) -> Self {
        Self {
            token_list: t_list.to_vec(),
            root: ASTNode::default(),
        }
    }
}
