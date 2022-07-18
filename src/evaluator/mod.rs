use std::collections::HashMap;

use crate::{
    astnode::{ASTNodes, EvalNode},
    tokens::Value,
};

pub struct Evaluator {
    map: HashMap<String, Value>,
    nodes: Vec<ASTNodes>,
}

impl Evaluator {
    pub fn new(nodes: &Vec<ASTNodes>) -> Self {
        Self {
            nodes: nodes.to_vec(),
            map: HashMap::new(),
        }
    }

    pub fn evaluate(&mut self) -> HashMap<String, Value> {
        for node in &self.nodes {
            let _ = match node {
                ASTNodes::ASTOpNode(op_node) => op_node.eval_node(&mut self.map),
                _ => panic!("expected operator node, got {:?}", node),
            };
        }
        self.map.clone()
    }
}
