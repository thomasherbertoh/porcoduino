use std::collections::HashMap;

use crate::{
    astnode::{
        ASTBlockNode, ASTIdentifierNode, ASTNodes, ASTOpNode, ASTProcNode, ASTValNode, EvalNode,
    },
    tokens::{Operator, Value},
    TokenType::ValType,
};

#[derive(Clone, Debug)]
pub struct Evaluator {
    evaluator_vars: Vec<HashMap<String, Value>>,
    evaluator_procs: Vec<HashMap<String, ASTProcNode>>,
    nodes: Vec<ASTNodes>,
    last_ret: Option<Value>,
    ret_depth: Option<u64>, // depth the value `last_ret` was returned from
}

impl Evaluator {
    pub fn new(nodes: &[ASTNodes]) -> Self {
        Self {
            evaluator_vars: Vec::new(),
            evaluator_procs: Vec::new(),
            nodes: nodes.to_vec(),
            last_ret: None,
            ret_depth: None,
        }
    }

    pub fn evaluate(&mut self) {
        for node in &self.nodes.clone() {
            match node {
                ASTNodes::ASTOpNode(op_node) => op_node.eval_node(self),
                ASTNodes::ASTBlockNode(block_node) => block_node.eval_node(self),
                ASTNodes::ASTProcNode(proc_node) => proc_node.eval_node(self),
                _ => panic!(
                    "[EVAL] Expected `Operator`, `StartBlock`, `EndBlock`, or `Proc`. Found `{:?}`",
                    node
                ),
            };
        }
    }

    pub fn get_vars(self) -> Vec<HashMap<String, Value>> {
        self.evaluator_vars
    }

    pub fn get_val(&self, ident_node: &ASTIdentifierNode) -> Value {
        let mut search_depth = self.evaluator_vars.len() - 1;
        loop {
            let map = self.evaluator_vars.get(search_depth).unwrap();
            if !map.contains_key(&ident_node.name) {
                if search_depth == 0 {
                    break; // -= 1 would cause error due to usize
                }
                search_depth -= 1;
            } else {
                return map.get(&ident_node.name).unwrap().clone();
            }
        }
        panic!(
            "[EVAL] Unable to find `Value` for `Identifier` {:?}",
            ident_node.name
        );
    }

    pub fn add_new_var_scope(&mut self) {
        self.evaluator_vars.push(HashMap::new());
    }

    pub fn remove_old_var_scope(&mut self) {
        self.evaluator_vars.remove(self.evaluator_vars.len() - 1);
    }

    pub fn add_new_proc_scope(&mut self) {
        self.evaluator_procs.push(HashMap::new());
    }

    pub fn remove_old_proc_scope(&mut self) {
        self.evaluator_procs.remove(self.evaluator_procs.len() - 1);
    }
}

impl EvalNode for ASTOpNode {
    fn eval_node(&self, evaluator: &mut Evaluator) -> Value {
        let mut to_ret = false;
        let left_node = if self.node.clone().left.is_none() {
            None
        } else {
            match self.node.clone().left.unwrap() {
                ASTNodes::ASTOpNode(node) => Some(node.eval_node(evaluator)),
                ASTNodes::ASTValNode(node) => {
                    to_ret |= node.is_ret;
                    Some(node.eval_node(evaluator))
                }
                ASTNodes::ASTIdentifierNode(ident) => {
                    to_ret |= ident.is_ret;
                    match self.op {
                        Operator::Assignment => None,
                        _ => {
                            // search through relevant block depths
                            let depth = ident.depth;
                            let val;
                            loop {
                                match evaluator
                                    .evaluator_vars
                                    .get(depth.unwrap() as usize)
                                    .unwrap()
                                    .get(&ident.name)
                                {
                                    Some(v) => {
                                        val = Some(v.clone());
                                        // break out when variable found
                                        break;
                                    }
                                    None => {
                                        // error if run out of scopes to search for variable in
                                        depth.unwrap().checked_sub(1).unwrap_or_else(||
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
                    }
                }
                ASTNodes::ASTBlockNode(block) => Some(block.eval_node(evaluator)),
                _ => panic!(
                    "[EVAL] eval_node() called on unevaluatable `{:?}`",
                    self.node.clone().left.unwrap()
                ),
            }
        };

        let right_node = match self.node.clone().right.unwrap() {
            ASTNodes::ASTOpNode(node) => node.eval_node(evaluator),
            ASTNodes::ASTValNode(node) => {
                to_ret |= node.is_ret;
                node.eval_node(evaluator)
            }
            ASTNodes::ASTIdentifierNode(ident) => {
                to_ret |= ident.is_ret;
                // search through relevant block depths
                let mut depth = ident.depth;
                let val;
                loop {
                    match evaluator
                        .evaluator_vars
                        .get(depth.unwrap() as usize)
                        .unwrap()
                        .get(&ident.name)
                    {
                        Some(v) => {
                            val = Some(v.clone());
                            // break out when variable found
                            break;
                        }
                        None => {
                            // error if run out of scopes to search for variable in
                            if depth.unwrap() == 0 {
                                panic!(
                                    "[EVAL] Unable to fetch value of identifier `{}`. Is it in scope?",
                                    ident.name
                                );
                            } else {
                                depth = Some(depth.unwrap() - 1);
                            }
                        }
                    }
                }
                val.unwrap()
            }
            ASTNodes::ASTBlockNode(block) => block.eval_node(evaluator),
            ASTNodes::ASTProcNode(proc) => proc.eval_node(evaluator),
            _ => panic!(
                "[EVAL] eval_node() called on unevaluatable `{:?}`",
                self.node.clone().right.unwrap()
            ),
        };

        // assignment
        if let Operator::Assignment = self.op {
            if left_node.is_none() {
                let depth = self.depth;

                // create scopes that should now exist
                while depth.unwrap() >= evaluator.evaluator_vars.len() as u64 {
                    evaluator.add_new_var_scope();
                }

                // delete scopes that should no longer exist
                while depth.unwrap() < (evaluator.evaluator_vars.len() as u64) - 1 {
                    evaluator.remove_old_var_scope();
                }

                let mut search_depth = depth;
                // break on declaration or successful redefinition
                loop {
                    let curr_map_mut = evaluator
                        .evaluator_vars
                        .get_mut(search_depth.unwrap() as usize)
                        .unwrap_or_else(|| {
                            panic!("[EVAL] variables disappeared for some reason lol")
                        });
                    let curr_map = curr_map_mut.clone();

                    let ident = match self.node.clone().left.unwrap() {
                        ASTNodes::ASTIdentifierNode(ident) => ident,
                        _ => panic!("[EVAL] Expected `Identifier`. Found `{:?}`", self.node.left),
                    };

                    if !curr_map.contains_key(&ident.name) && ident.is_declaration {
                        curr_map_mut.insert(ident.name.clone(), right_node.clone());
                        break; // successful declaration
                    } else if curr_map.contains_key(&ident.name) {
                        if ident.is_declaration {
                            panic!(
                                "[EVAL] Re-declaration of variable `{}`. It currently has value `{:?}` and you're trying to assign it `{:?}`. Self = {:#?}",
                                ident.name,
                                curr_map.get(&ident.name).unwrap(),
                                right_node,
                                self,
                            );
                        } else if right_node.get_type()
                            != curr_map.get(&ident.name).unwrap().get_type()
                        {
                            // wrong type
                            panic!("[EVAL] Invalid operation: assigning value of type `{}`({:?}) to variable of type `{}`({})",
                                    right_node.get_type(),
                                    right_node,
                                    evaluator.evaluator_vars.get((ident.depth.unwrap()) as usize).unwrap().get(&ident.name).unwrap().get_type(),
                                    ident.name
                        );
                        } else {
                            // redefine variable
                            *curr_map_mut.get_mut(&ident.name).unwrap() = right_node.clone();
                            break; // successful redefinition
                        }
                    } else {
                        search_depth = Some(search_depth.unwrap().checked_sub(1).unwrap_or_else(|| panic!(
                            "[EVAL] Unable to fetch value of identifier `{}`. Is it in scope? Current depth: {:?}, vars: {:?}",
                            ident.name, depth, evaluator.evaluator_vars
                        )));
                    }
                }
                if to_ret {
                    evaluator.last_ret = Some(right_node.clone());
                    evaluator.ret_depth = depth;
                }
                return right_node;
            } else if let Some(Value::Integer(_)) = left_node {
                return left_node.unwrap(); // not sure why clippy thinks this is in the block above
            } else {
                panic!(
                    "[EVAL] Expected `Identifier`. Found `{:?}`, self = {:#?}",
                    left_node, self
                );
            }
        }

        let res = match self.op {
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
                    Value::Boolean(br) => {
                        Value::Integer(il.checked_add(br as i64).unwrap_or_else(|| {
                            panic!(
                                "[EVAL] Integers can only be up to 64 bits: `{} + {}`",
                                il, br
                            )
                        }))
                    }
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
                    Value::Boolean(br) => {
                        Value::Integer(il.checked_sub(br as i64).unwrap_or_else(|| {
                            panic!(
                                "[EVAL] Error while performing subtraction: `{} - {}`",
                                il, br
                            )
                        }))
                    }
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
                    Value::Boolean(br) => Value::String(sl.repeat(br as usize)),
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
                    Value::Boolean(br) => {
                        Value::Integer(il.checked_mul(br as i64).unwrap_or_else(|| {
                            panic!(
                                "[EVAL] Integers can only be up to 64 bits: `{} + {}`",
                                il, br
                            )
                        }))
                    }
                },
                Value::Boolean(bl) => match right_node {
                    Value::String(sr) => Value::String(sr.repeat(bl as usize)),
                    Value::Char(cr) => {
                        if bl {
                            Value::Char(cr)
                        } else {
                            Value::Char('\0')
                        }
                    }
                    Value::Integer(ir) => {
                        Value::Integer(ir.checked_mul(bl as i64).unwrap_or_else(|| {
                            panic!(
                                "[EVAL] Integers can only be up to 64 bits: `{} * {}`",
                                bl, ir
                            )
                        }))
                    }
                    Value::Boolean(br) => Value::Boolean(bl && br),
                },
            },
            Operator::Division => match left_node.clone().unwrap() {
                Value::Integer(il) => match right_node {
                    Value::Integer(ir) => Value::Integer(il.checked_div(ir).unwrap_or_else(|| {
                        panic!("[EVAL] Integer division error: `{} / {}`", il, ir)
                    })),
                    Value::Boolean(br) => {
                        Value::Integer(il.checked_div(br as i64).unwrap_or_else(|| {
                            panic!("[EVAL] Integer division error: `{} / {}`", il, br)
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
                    Value::Boolean(br) => Value::Integer(il & br as i64),
                },
                Value::Boolean(bl) => match right_node {
                    Value::String(_) | Value::Char(_) => {
                        panic!(
                            "[EVAL] Invalid operation `{:?}` on value `{:?}`",
                            self.op, right_node
                        )
                    }
                    Value::Integer(ir) => Value::Boolean((bl as i64 & ir) % 2 != 0),
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
                    Value::Boolean(br) => Value::Integer(il | br as i64),
                },
                Value::Boolean(bl) => match right_node {
                    Value::String(_) | Value::Char(_) => {
                        panic!(
                            "[EVAL] Invalid operation `{:?}` on value `{:?}`",
                            self.op, right_node
                        )
                    }
                    Value::Integer(ir) => Value::Boolean((bl as i64 | ir) % 2 != 0),
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
        };
        if to_ret {
            evaluator.last_ret = Some(res.clone());
            evaluator.ret_depth = self.depth; // takes depth of the `ASTOpNode`
        }
        res
    }
}

impl EvalNode for ASTValNode {
    fn eval_node(&self, _evaluator: &mut Evaluator) -> Value {
        self.val.clone()
    }
}

impl EvalNode for ASTBlockNode {
    fn eval_node(&self, evaluator: &mut Evaluator) -> Value {
        let mut out = None;
        for node in &self.nodes {
            out = match node {
                ASTNodes::ASTOpNode(op) => Some(op.eval_node(evaluator)),
                ASTNodes::ASTValNode(val) => Some(val.eval_node(evaluator)),
                ASTNodes::ASTBlockNode(block) => Some(block.eval_node(evaluator)),
                _ => panic!("[EVAL] eval_node() called on unevaluatable `{:?}`", node),
            };
        }
        out.unwrap()
    }
}

impl EvalNode for ASTProcNode {
    fn eval_node(&self, evaluator: &mut Evaluator) -> Value {
        if self.is_declaration {
            let depth = self.depth;

            // create scopes that should now exist
            while depth.unwrap() >= evaluator.evaluator_procs.len() as u64 {
                evaluator.add_new_proc_scope();
            }

            // remove scopes that should no longer exist
            while depth.unwrap() < (evaluator.evaluator_procs.len() as u64) - 1 {
                evaluator.remove_old_proc_scope();
            }

            // get proc map
            let map = evaluator
                .evaluator_procs
                .get_mut(depth.unwrap() as usize)
                .unwrap_or_else(|| panic!("[EVAL] procedures disappeared for some reason lol"));

            // insert proc
            map.insert(self.name.clone(), self.clone());

            Value::Integer(0)
        } else {
            // create var scope for proc
            // proc scope = call scope + 1
            let depth = self.depth.unwrap() + 1;
            while depth >= evaluator.evaluator_vars.len() as u64 {
                evaluator.add_new_var_scope();
            }

            let mut evaluator_clone = evaluator.clone();

            // get proc scope map
            let proc_var_map = evaluator
                .evaluator_vars
                .get_mut(depth as usize)
                .unwrap_or_else(|| panic!("[EVAL] error getting proc scope at depth {:?}", depth));

            // get map entry of proc
            let proc;
            let mut search_depth = self.depth.unwrap();
            loop {
                let curr_map = evaluator
                    .evaluator_procs
                    .get(search_depth as usize)
                    .unwrap();

                if !curr_map.contains_key(&self.name) {
                    if search_depth == 0 {
                        panic!("[EVAL] unable to find proc {:?}", self.name);
                    }

                    search_depth -= 1;
                } else {
                    proc = curr_map.get(&self.name).unwrap();
                    break;
                }
            }

            // copy args into params of proc (using var scope of proc)
            for (i, param) in proc.params.iter().enumerate() {
                proc_var_map.insert(param.name.clone(), {
                    let val = match self.args.get(i).unwrap() {
                        // this can only search before proc scope
                        ASTNodes::ASTIdentifierNode(ident_node) => {
                            evaluator_clone.get_val(ident_node)
                        }
                        ASTNodes::ASTOpNode(op_node) => op_node.eval_node(&mut evaluator_clone),
                        ASTNodes::ASTValNode(val_node) => val_node.val.clone(),
                        ASTNodes::ASTBlockNode(block_node) => {
                            block_node.eval_node(&mut evaluator_clone)
                        }
                        _ => panic!(
                            "[EVAL] eval_node() called on unevaluatable `{:?}`",
                            self.args.get(i).unwrap()
                        ),
                    };

                    // check type of arg matches type of param
                    if val.get_type()
                        != match param.param_type.clone() {
                            ValType(s) => s,
                            _ => String::new(),
                        }
                    {
                        panic!(
                            "[EVAL] Type of argument {:?} does not match type of parameter {:?}",
                            self.args.get(i).unwrap(),
                            param,
                        );
                    }

                    val
                });
            }

            let proc_ret_type = proc.ret_type.clone().unwrap();
            let proc_name = proc.name.clone();

            // evaluate block
            let block_val = match proc.body.clone().unwrap() {
                ASTNodes::ASTBlockNode(block_node) => block_node.eval_node(evaluator),
                _ => panic!(
                    "[EVAL] Expected an `ASTBlockNode`, found an `{:?}`",
                    proc.body.clone().unwrap()
                ),
            };

            // check return type of proc
            let tmp = &String::new();
            if &block_val.get_type()
                != match &proc_ret_type {
                    ValType(s) => s,
                    _ => tmp,
                }
            {
                panic!("[EVAL] Returned value `{:?}` doesn't match the expected return type `{:?}` of the procedure `{:?}`",
                    block_val,
                    proc_ret_type,
                    proc_name);
            }

            // decide whether or not to return value
            //delete proc scope

            block_val
        }
    }
}
