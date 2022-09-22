use std::collections::HashMap;

use crate::{
    astnode::{ASTNodes, ASTOpNode, ASTValNode, EvalNode},
    tokens::{Operator, Value},
};

pub struct Evaluator {
    map: HashMap<String, Value>,
    nodes: Vec<ASTNodes>,
}

impl Evaluator {
    pub fn new(nodes: &[ASTNodes]) -> Self {
        Self {
            nodes: nodes.to_vec(),
            map: HashMap::new(),
        }
    }

    pub fn evaluate(&mut self) -> HashMap<String, Value> {
        for node in &self.nodes {
            let _ = match node {
                ASTNodes::ASTOpNode(op_node) => op_node.eval_node(&mut self.map),
                _ => panic!("[EVAL] Expected `Operator`. Found `{:?}`", node),
            };
        }
        self.map.clone()
    }
}

impl EvalNode for ASTOpNode {
    fn eval_node(&self, map: &mut HashMap<String, Value>) -> Value {
        let left_node = if self.node.clone().left.is_none() {
            None
        } else {
            match self.node.clone().left.unwrap() {
                ASTNodes::ASTOpNode(node) => Some(node.eval_node(map)),
                ASTNodes::ASTValNode(node) => Some(node.eval_node(map)),
                ASTNodes::ASTIdentifierNode(ident) => match self.op {
                    Operator::Assignment => None,
                    _ => Some(map.get(&ident.name).unwrap().clone()),
                },
                _ => panic!(
                    "[EVAL] eval_node() called on unevaluatable `{:?}`",
                    self.node.clone().left.unwrap()
                ),
            }
        };

        let right_node = match self.node.clone().right.unwrap() {
            ASTNodes::ASTOpNode(node) => node.eval_node(map),
            ASTNodes::ASTValNode(node) => node.eval_node(map),
            ASTNodes::ASTIdentifierNode(ident) => map.get(&ident.name).unwrap().clone(),
            _ => panic!(
                "[EVAL] eval_node() called on unevaluatable `{:?}`",
                self.node.clone().right.unwrap()
            ),
        };

        // assignment
        if let Operator::Assignment = self.op {
            if left_node.is_none() {
                map.insert(
                    match self.node.clone().left.unwrap() {
                        ASTNodes::ASTIdentifierNode(ident) => {
                            if !map.contains_key(&ident.name) {
                                ident.name
                            } else if right_node.get_type()
                                == *map.get(&ident.name).unwrap().get_type()
                            {
                                ident.name
                            } else {
                                panic!("[EVAL] Invalid operation: assigning value of type `{}`({:?}) to variable of type `{}`({})",
                                    right_node.get_type(),
                                    right_node,
                                    map.get(&ident.name).unwrap().get_type(),
                                    ident.name
                                );
                            }
                        }
                        _ => panic!("[EVAL] Expected `Identifier`. Found `{:?}`", self.node.left),
                    },
                    right_node.clone(),
                );
                return right_node;
            } else {
                panic!("[EVAL] Expected `Identifier`. Found `{:?}`", left_node);
            }
        }

        match self.op {
            Operator::Addition => match left_node {
                Some(Value::String(sl)) => match right_node {
                    Value::String(sr) => Value::String(sl + &sr),
                    Value::Char(cr) => Value::String(sl + &cr.to_string()), // feels dirty
                    Value::Integer(ir) => Value::String(sl + &ir.to_string()), // feels dirty
                    Value::Boolean(br) => {
                        panic!("[EVAL] Invalid operation: {} {:?} {}", sl, self.op, br)
                    }
                },
                Some(Value::Char(cl)) => match right_node {
                    Value::String(sr) => Value::String(cl.to_string() + &sr),
                    Value::Char(cr) => Value::String(cl.to_string() + &cr.to_string()),
                    Value::Integer(ir) => Value::String(cl.to_string() + &ir.to_string()),
                    Value::Boolean(br) => {
                        panic!("[EVAL] Invalid operation: {} {:?} {}", cl, self.op, br)
                    }
                },
                Some(Value::Integer(il)) => match right_node {
                    Value::String(sr) => Value::String(il.to_string() + &sr),
                    Value::Char(cr) => Value::String(il.to_string() + &cr.to_string()),
                    Value::Integer(ir) => Value::Integer(il.checked_add(ir).unwrap_or_else(|| {
                        panic!(
                            "[EVAL] Integers can only be up to 64 bits: `{} + {}`",
                            il, ir
                        )
                    })),
                    Value::Boolean(br) => Value::Integer(
                        il.checked_add(if br { 1 } else { 0 }).unwrap_or_else(|| {
                            panic!(
                                "[EVAL] Integers can only be up to 64 bits: `{} + {}`",
                                il, br
                            )
                        }),
                    ),
                },
                Some(Value::Boolean(bl)) => match right_node {
                    Value::Boolean(br) => Value::Boolean(bl || br),
                    v => panic!("[EVAL] Invalid operation: {} {:?} {:?}", bl, self.op, v),
                },
                None => right_node,
            },
            Operator::Subtraction => match left_node.clone() {
                Some(Value::Integer(il)) => match right_node {
                    Value::Integer(ir) => Value::Integer(il.checked_sub(ir).unwrap_or_else(|| {
                        panic!(
                            "[EVAL] Error while performing subtraction: `{} - {}`",
                            il, ir
                        )
                    })),
                    Value::Boolean(br) => Value::Integer(
                        il.checked_sub(if br { 1 } else { 0 }).unwrap_or_else(|| {
                            panic!(
                                "[EVAL] Error while performing subtraction: `{} - {}`",
                                il, br
                            )
                        }),
                    ),
                    _ => panic!(
                        "[EVAL] Invalid operation `{:?}` on value `{:?}`",
                        self.op, right_node
                    ),
                },
                None => match right_node {
                    Value::Integer(ir) => {
                        Value::Integer(0_i64.checked_sub(ir).unwrap_or_else(|| {
                            panic!(
                                "[EVAL] Error while performing subtraction: `{} - {}`",
                                0, ir
                            )
                        }))
                    }
                    _ => panic!(
                        "[EVAL] Invalid operation `{:?}` on value `{:?}`",
                        self.op, right_node
                    ),
                },
                _ => panic!(
                    "[EVAL] Invalid operation `{:?}` on value `{:?}`",
                    self.op,
                    left_node.unwrap()
                ),
            },
            Operator::Multiplication => match left_node.unwrap() {
                Value::String(sl) => match right_node {
                    Value::Integer(ir) => Value::String(sl.repeat(ir as usize)),
                    Value::Boolean(br) => Value::String(sl.repeat(if br { 1 } else { 0 })),
                    _ => panic!(
                        "[EVAL] Invalid operation `{:?}` on value `{:?}`",
                        self.op, right_node
                    ),
                },
                Value::Char(cl) => match right_node {
                    Value::Integer(ir) => Value::String(cl.to_string().repeat(ir as usize)),
                    Value::Boolean(br) => {
                        if br {
                            Value::Char(cl)
                        } else {
                            Value::Char('\0')
                        }
                    }
                    _ => panic!(
                        "[EVAL] Invalid operation `{:?}` on value `{:?}`",
                        self.op, right_node
                    ),
                },
                Value::Integer(il) => match right_node {
                    Value::String(sr) => Value::String(sr.repeat(il as usize)),
                    Value::Char(cr) => Value::String(cr.to_string().repeat(il as usize)),
                    Value::Integer(ir) => Value::Integer(il.checked_mul(ir).unwrap_or_else(|| {
                        panic!(
                            "[EVAL] Integers can only be up to 64 bits: `{} + {}`",
                            il, ir
                        )
                    })),
                    Value::Boolean(br) => Value::Integer(
                        il.checked_mul(if br { 1 } else { 0 }).unwrap_or_else(|| {
                            panic!(
                                "[EVAL] Integers can only be up to 64 bits: `{} + {}`",
                                il, br
                            )
                        }),
                    ),
                },
                Value::Boolean(bl) => match right_node {
                    Value::String(sr) => Value::String(sr.repeat(if bl { 1 } else { 0 })),
                    Value::Char(cr) => {
                        if bl {
                            Value::Char(cr)
                        } else {
                            Value::Char('\0')
                        }
                    }
                    Value::Integer(ir) => Value::Integer(
                        ir.checked_mul(if bl { 1 } else { 0 }).unwrap_or_else(|| {
                            panic!(
                                "[EVAL] Integers can only be up to 64 bits: `{} * {}`",
                                bl, ir
                            )
                        }),
                    ),
                    Value::Boolean(br) => Value::Boolean(bl && br),
                },
            },
            Operator::Division => match left_node.clone().unwrap() {
                Value::Integer(il) => match right_node {
                    Value::Integer(ir) => Value::Integer(il.checked_div(ir).unwrap_or_else(|| {
                        panic!("[EVAL] Integer division error: `{} / {}`", il, ir)
                    })),
                    Value::Boolean(br) => {
                        Value::Integer(il.checked_div(if br { 1 } else { 0 }).unwrap_or_else(
                            || panic!("[EVAL] Integer division error: `{} / {}`", il, br),
                        ))
                    }
                    _ => panic!(
                        "[EVAL] Invalid operation `{:?}` on value `{:?}`",
                        self.op, right_node
                    ),
                },
                _ => panic!(
                    "[EVAL] Invalid operation `{:?}` on value `{:?}`",
                    self.op,
                    left_node.unwrap()
                ),
            },
            Operator::Assignment => unreachable!(),
        }
    }
}

impl EvalNode for ASTValNode {
    fn eval_node(&self, _map: &mut HashMap<String, Value>) -> Value {
        self.val.clone()
    }
}
