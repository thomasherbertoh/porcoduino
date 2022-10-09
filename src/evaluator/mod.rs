use std::collections::HashMap;

use crate::{
    astnode::{ASTNodes, ASTOpNode, ASTValNode, EvalNode},
    tokens::{Operator, Value},
};

pub struct Evaluator<'a> {
    evaluator_vars: Vec<&'a mut HashMap<String, Value>>,
    nodes: Vec<ASTNodes>,
}

impl<'a> Evaluator<'a> {
    pub fn new(nodes: &[ASTNodes]) -> Self {
        Self {
            evaluator_vars: Vec::new(),
            nodes: nodes.to_vec(),
        }
    }

    pub fn evaluate(&mut self) {
        for node in &self.nodes {
            let _ = match node {
                ASTNodes::ASTOpNode(op_node) => op_node.eval_node(&mut self.evaluator_vars),
                _ => panic!("[EVAL] Expected `Operator`. Found `{:?}`", node),
            };
        }
    }

    pub fn get_vars(self) -> Vec<&'a mut HashMap<String, Value>> {
        self.evaluator_vars
    }
}

impl EvalNode for ASTOpNode {
    fn eval_node(&self, vars: &mut Vec<&mut HashMap<String, Value>>) -> Value {
        let left_node = if self.node.clone().left.is_none() {
            None
        } else {
            match self.node.clone().left.unwrap() {
                ASTNodes::ASTOpNode(node) => Some(node.eval_node(vars)),
                ASTNodes::ASTValNode(node) => Some(node.eval_node(vars)),
                ASTNodes::ASTIdentifierNode(ident) => match self.op {
                    Operator::Assignment => None,
                    _ => {
                        // search through relevant block depths
                        let depth = ident.depth;
                        let val;
                        loop {
                            let tmp = (*vars).get(depth as usize).unwrap().get(&ident.name);
                            match tmp {
                                Some(v) => {
                                    val = Some(v.clone());
                                    // break out when variable found
                                    break;
                                }
                                None => {
                                    // error if run out of scopes to search for variable in
                                    depth.checked_sub(1).unwrap_or_else(||
                                        panic!(
                                            "[EVAL] Unable to fetch value of identifier `{}`. Is it in scope?",
                                            ident.name
                                        )
                                    );
                                }
                            }
                        }
                        val
                    }
                },
                _ => panic!(
                    "[EVAL] eval_node() called on unevaluatable `{:?}`",
                    self.node.clone().left.unwrap()
                ),
            }
        };

        let right_node = match self.node.clone().right.unwrap() {
            ASTNodes::ASTOpNode(node) => node.eval_node(vars),
            ASTNodes::ASTValNode(node) => node.eval_node(vars),
            ASTNodes::ASTIdentifierNode(ident) => {
                // search through relevant block depths
                let depth = ident.depth;
                let val;
                loop {
                    let tmp = (*vars).get(depth as usize).unwrap().get(&ident.name);
                    match tmp {
                        Some(v) => {
                            val = Some(v.clone());
                            // break out when variable found
                            break;
                        }
                        None => {
                            // error if run out of scopes to search for variable in
                            depth.checked_sub(1).unwrap_or_else(||
                                panic!(
                                    "[EVAL] Unable to fetch value of identifier `{}`. Is it in scope?",
                                    ident.name
                                )
                            );
                        }
                    }
                }
                val.unwrap()
            }
            _ => panic!(
                "[EVAL] eval_node() called on unevaluatable `{:?}`",
                self.node.clone().right.unwrap()
            ),
        };

        // assignment
        if let Operator::Assignment = self.op {
            if left_node.is_none() {
                let depth = match self.node.right.clone().unwrap() {
                    ASTNodes::ASTNode(node) => node.depth,
                    ASTNodes::ASTOpNode(node) => node.depth,
                    ASTNodes::ASTIdentifierNode(node) => node.depth,
                    ASTNodes::ASTValNode(_) => {
                        panic!("[EVAL] Invalid operation: Assignment to `Value`")
                    }
                };

                let curr_map_mut = vars.get_mut(depth as usize).unwrap();
                let curr_map = curr_map_mut.clone();

                curr_map_mut.insert(
                    match self.node.clone().left.unwrap() {
                        ASTNodes::ASTIdentifierNode(ident) => {
                            if !curr_map.contains_key(&ident.name) || right_node.get_type()
                                == *curr_map.get(&ident.name).unwrap().get_type()
                            {
                                ident.name
                            } else {
                                panic!("[EVAL] Invalid operation: assigning value of type `{}`({:?}) to variable of type `{}`({})",
                                    right_node.get_type(),
                                    right_node,
                                    vars.get(ident.depth as usize).unwrap().get(&ident.name).unwrap().get_type(),
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
            Operator::BitwiseAnd => match left_node.clone().unwrap() {
                Value::String(_) | Value::Char(_) => {
                    panic!(
                        "[EVAL] Invalid operation `{:?}` on value `{:?}`",
                        self.op, left_node
                    )
                }
                Value::Integer(il) => match right_node {
                    Value::String(_) | Value::Char(_) => panic!(
                        "[EVAL] Invalid operation `{:?}` on value `{:?}`",
                        self.op, right_node
                    ),
                    Value::Integer(ir) => Value::Integer(il & ir),
                    Value::Boolean(br) => Value::Integer(il & if br { 1 } else { 0 }),
                },
                Value::Boolean(bl) => match right_node {
                    Value::String(_) | Value::Char(_) => {
                        panic!(
                            "[EVAL] Invalid operation `{:?}` on value `{:?}`",
                            self.op, right_node
                        )
                    }
                    Value::Integer(ir) => Value::Boolean((if bl { 1 } else { 0 } & ir) % 2 != 0),
                    Value::Boolean(br) => Value::Boolean(bl & br),
                },
            },
            Operator::BitwiseOr => match left_node.clone().unwrap() {
                Value::String(_) | Value::Char(_) => {
                    panic!(
                        "[EVAL] Invalid operation `{:?}` on value `{:?}`",
                        self.op, left_node
                    )
                }
                Value::Integer(il) => match right_node {
                    Value::String(_) | Value::Char(_) => panic!(
                        "[EVAL] Invalid operation `{:?}` on value `{:?}`",
                        self.op, right_node
                    ),
                    Value::Integer(ir) => Value::Integer(il | ir),
                    Value::Boolean(br) => Value::Integer(il | if br { 1 } else { 0 }),
                },
                Value::Boolean(bl) => match right_node {
                    Value::String(_) | Value::Char(_) => {
                        panic!(
                            "[EVAL] Invalid operation `{:?}` on value `{:?}`",
                            self.op, right_node
                        )
                    }
                    Value::Integer(ir) => Value::Boolean((if bl { 1 } else { 0 } | ir) % 2 != 0),
                    Value::Boolean(br) => Value::Boolean(bl | br),
                },
            },
            Operator::Equality => {
                if left_node.clone().unwrap().get_type() != right_node.get_type() {
                    Value::Boolean(false)
                } else {
                    Value::Boolean(left_node.unwrap() == right_node)
                }
            }
            Operator::Inequality => {
                if left_node.clone().unwrap().get_type() != right_node.get_type() {
                    Value::Boolean(true)
                } else {
                    Value::Boolean(left_node.unwrap() != right_node)
                }
            }
            Operator::Negation => match left_node.clone() {
                None => match right_node {
                    Value::Boolean(br) => Value::Boolean(!br),
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
    fn eval_node(&self, _vars: &mut Vec<&mut HashMap<String, Value>>) -> Value {
        self.val.clone()
    }
}
