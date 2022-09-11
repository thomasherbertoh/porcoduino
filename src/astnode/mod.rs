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

impl EvalNode for ASTOpNode {
    fn eval_node(&self, map: &mut HashMap<String, Value>) -> Value {
        let left_node;
        if self.node.clone().left.is_none() {
            left_node = None;
        } else {
            left_node = match self.node.clone().left.unwrap() {
                ASTNodes::ASTNode(node) => panic!("eval_node() called on {:?}", node),
                ASTNodes::ASTOpNode(node) => Some(node.eval_node(map)),
                ASTNodes::ASTValNode(node) => Some(node.eval_node(map)),
                ASTNodes::ASTIdentifierNode(ident) => match self.op {
                    Operator::Assignment => None,
                    _ => Some(map.get(&ident.name).unwrap().clone()),
                },
            };
        }

        let right_node = match self.node.clone().right.unwrap() {
            ASTNodes::ASTNode(node) => panic!("eval_node() called on {:?}", node),
            ASTNodes::ASTOpNode(node) => node.eval_node(map),
            ASTNodes::ASTValNode(node) => node.eval_node(map),
            ASTNodes::ASTIdentifierNode(ident) => map.get(&ident.name).unwrap().clone(),
        };

        // assignment
        if let Operator::Assignment = self.op {
            if let None = left_node {
                map.insert(
                    match self.node.clone().left.unwrap() {
                        ASTNodes::ASTIdentifierNode(ident) => ident.name,
                        _ => panic!("expected an identifier, got {:?}", self.node.left),
                    },
                    right_node.clone(),
                );
                return right_node;
            } else {
                panic!("assignment to non-identifier {:?}", left_node);
            }
        }

        match self.op {
            Operator::Addition => match left_node {
                Some(Value::String(sl)) => match right_node {
                    Value::String(sr) => Value::String(sl + &sr),
                    Value::Char(cr) => Value::String(sl + &cr.to_string()), // feels dirty
                    Value::Integer(ir) => Value::String(sl + &ir.to_string()), // feels dirty
                },
                Some(Value::Char(cl)) => match right_node {
                    Value::String(sr) => Value::String(cl.to_string() + &sr),
                    Value::Char(cr) => Value::String(cl.to_string() + &cr.to_string()),
                    Value::Integer(ir) => Value::String(cl.to_string() + &ir.to_string()),
                },
                Some(Value::Integer(il)) => match right_node {
                    Value::String(sr) => Value::String(il.to_string() + &sr),
                    Value::Char(cr) => Value::String(il.to_string() + &cr.to_string()),
                    Value::Integer(ir) => Value::Integer(il.checked_add(ir).expect(&format!(
                        "integers can only be up to 64 bits: `{} + {}`",
                        il, ir
                    ))),
                },
                None => match right_node {
                    Value::String(sr) => Value::String(sr),
                    Value::Char(cr) => Value::Char(cr),
                    Value::Integer(ir) => Value::Integer(ir),
                },
            },
            Operator::Subtraction => match left_node.clone() {
                Some(Value::Integer(il)) => match right_node {
                    Value::Integer(ir) => Value::Integer(il.checked_sub(ir).expect(&format!(
                        "error while performing subtraction: `{} - {}`",
                        il, ir
                    ))),
                    _ => panic!("invalid operation {:?} on value {:?}", self.op, right_node),
                },
                None => match right_node {
                    Value::Integer(ir) => Value::Integer(0_i64.checked_sub(ir).expect(&format!(
                        "error while performing subtraction: {} - {}",
                        0, ir
                    ))),
                    _ => panic!("invalid operation {:?} on value {:?}", self.op, right_node),
                },
                _ => panic!(
                    "invalid operation {:?} on value {:?}",
                    self.op,
                    left_node.unwrap()
                ),
            },
            Operator::Multiplication => match left_node.unwrap() {
                Value::String(sl) => match right_node {
                    Value::Integer(ir) => Value::String(sl.repeat(ir as usize)),
                    _ => panic!("invalid operation {:?} on value {:?}", self.op, right_node),
                },
                Value::Char(cl) => match right_node {
                    Value::Integer(ir) => Value::String(cl.to_string().repeat(ir as usize)),
                    _ => panic!("invalid operation {:?} on value {:?}", self.op, right_node),
                },
                Value::Integer(il) => match right_node {
                    Value::String(sr) => Value::String(sr.repeat(il as usize)),
                    Value::Char(cr) => Value::String(cr.to_string().repeat(il as usize)),
                    Value::Integer(ir) => Value::Integer(il.checked_mul(ir).expect(&format!(
                        "integers can only be up to 64 bits: `{} * {}`",
                        il, ir
                    ))),
                },
            },
            Operator::Division => match left_node.clone().unwrap() {
                Value::Integer(il) => match right_node {
                    Value::Integer(ir) => Value::Integer(
                        il.checked_div(ir)
                            .expect(&format!("integer division error: `{} / {}`", il, ir)),
                    ),
                    _ => panic!("invalid operation {:?} on value {:?}", self.op, right_node),
                },
                _ => panic!(
                    "invalid operation {:?} on value {:?}",
                    self.op,
                    left_node.unwrap()
                ),
            },
            Operator::Assignment => unreachable!(),
        }
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
    fn eval_node(&self, _map: &mut HashMap<String, Value>) -> Value {
        self.val.clone()
    }
}

#[derive(Clone, Debug)]
pub struct ASTIdentifierNode {
    name: String,
}

impl ASTIdentifierNode {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}
