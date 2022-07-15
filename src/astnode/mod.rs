use crate::tokens::{Operator, Value};

trait EvalNode {
    fn eval_node(&self) -> Value;
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
    left: Box<Option<ASTNodes>>,
    right: Box<Option<ASTNodes>>,
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
    node: ASTNode,
    op: Operator,
}

impl ASTOpNode {
    pub fn new(left: Box<Option<ASTNodes>>, right: Box<Option<ASTNodes>>, op: Operator) -> Self {
        Self {
            node: ASTNode::new(left, right),
            op,
        }
    }
}

impl EvalNode for ASTOpNode {
    fn eval_node(&self) -> Value {
        let left_node = match self.node.clone().left.unwrap() {
            ASTNodes::ASTNode(node) => panic!("eval_node() called on {:?}", node),
            ASTNodes::ASTOpNode(node) => Some(node.eval_node()),
            ASTNodes::ASTValNode(node) => Some(node.eval_node()),
            ASTNodes::ASTIdentifierNode(_) => match self.op {
                Operator::Assignment => None,
                _ => todo!(), // fetch value from hashmap
            },
        };

        let _right_node = match self.node.clone().right.unwrap() {
            ASTNodes::ASTNode(node) => panic!("eval_node() called on {:?}", node),
            ASTNodes::ASTOpNode(node) => node.eval_node(),
            ASTNodes::ASTValNode(node) => node.eval_node(),
            ASTNodes::ASTIdentifierNode(_) => {
                // figure out how to fetch value from hashmap?
                // should the hashmap even exist at this stage?
                // should that be the job of the parser?
                todo!();
            }
        };

        /*
        let left;
        let right;

        match self.op {
            Operator::Addition => Variable::Integer(left + right),
            Operator::Subtraction => Variable::Integer(left - right),
            Operator::Multiplication => Variable::Integer(left * right),
            Operator::Division => Variable::Integer(left / right),
            Operator::Assignment => todo!(),
        }
        */
        left_node.unwrap()
    }
}

#[derive(Clone, Debug)]
pub struct ASTValNode {
    val: Value,
}

impl ASTValNode {
    pub fn new(val: Value) -> Self {
        Self { val }
    }
}

impl EvalNode for ASTValNode {
    fn eval_node(&self) -> Value {
        todo!()
    }
}

#[derive(Clone, Debug)]
pub struct ASTIdentifierNode {
    name: String,
    // should this even store its value?
    // could just be left in the hashmap...
    val: Option<Value>,
}

impl ASTIdentifierNode {
    pub fn new(name: String, val: Option<Value>) -> Self {
        Self { name, val }
    }
}
