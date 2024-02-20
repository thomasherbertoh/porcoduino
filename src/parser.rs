use crate::{
    astnode::{
        ASTBlockNode, ASTIdentifierNode, ASTNodes, ASTOpNode, ASTParamNode, ASTProcNode, ASTValNode,
    },
    tokens::{Operator, Token, TokenType, Value},
};

pub struct Parser {
    pub token_list: Vec<Token>,
}

impl Parser {
    pub fn new(t_list: &[Token]) -> Self {
        Self {
            token_list: t_list.to_vec(),
        }
    }

    fn astnode_from_token(
        token: &Token,
        prev: Option<&Token>,
        next: Option<&Token>,
    ) -> Option<ASTNodes> {
        match &token.t_type {
            TokenType::Identifier(name) => {
                Some(ASTNodes::ASTIdentifierNode(ASTIdentifierNode::new(
                    name.to_string(),
                    Some(token.code_depth),
                    prev.is_some()
                        && matches!(prev.unwrap().t_type, TokenType::Make | TokenType::Proc),
                    next.is_some() && next.unwrap().t_type == TokenType::EndBlock,
                )))
            }
            TokenType::Value(v) => Some(ASTNodes::ASTValNode(ASTValNode::new(
                v.clone(),
                Some(token.code_depth),
                next.is_some() && next.unwrap().t_type == TokenType::EndBlock,
            ))),
            TokenType::Operator(op) => Some(ASTNodes::ASTOpNode(ASTOpNode::new(
                Box::new(None),
                Box::new(None),
                op.clone(),
                Some(token.code_depth),
            ))),
            _ => None,
        }
    }

    fn find_rbracket_index(&self, start: usize, end: usize) -> usize {
        let mut bracket_count = 0;
        for i in start..=end {
            bracket_count += match self.token_list.get(i).unwrap().t_type {
                TokenType::LBracket => 1,
                TokenType::RBracket => -1,
                _ => 0,
            };
            if bracket_count == 0 {
                // found corresponding RBracket at index `i`
                return i;
            }
        }
        panic!(
            "[PARSE] Unable to find RBracket in slice {}..={}",
            start, end
        );
    }

    fn build_arg_list(&self, start: usize, end: usize) -> Vec<ASTNodes> {
        let mut out = Vec::new();
        let mut rbracket_index = self.find_rbracket_index(start, end);
        let rbracket = self.token_list.get(rbracket_index).unwrap();
        if !matches!(rbracket.t_type, TokenType::RBracket) {
            rbracket_index = end;
        }
        for i in start + 1..rbracket_index {
            let curr = self.token_list.get(i).unwrap();
            if curr.t_type == TokenType::ItemSeparator {
                continue;
            }

            match curr.t_type {
                TokenType::Identifier(_) | TokenType::Value(_) => out.push(
                    Parser::astnode_from_token(
                        curr,
                        self.token_list.get(i - 1),
                        self.token_list.get(i + 1),
                    )
                    .unwrap(),
                ),
                _ => break,
            };
        }

        out
    }

    fn build_param_list(&self, start: usize, end: usize) -> Vec<ASTParamNode> {
        let mut out = Vec::new();
        let mut rbracket_index = self.find_rbracket_index(start, end);
        let rbracket = self.token_list.get(rbracket_index).unwrap();
        if !matches!(rbracket.t_type, TokenType::RBracket) {
            rbracket_index = end;
            // panic!("[PARSE] Expected ')' but found {:?} instead.", rbracket);
        }

        let mut ind = start + 1; // skip lbracket
        while ind < rbracket_index {
            let ident_token = self.token_list.get(ind).unwrap();
            let name = match &ident_token.t_type {
                TokenType::Identifier(ident_name) => ident_name,
                _ => panic!(
                    "[PARSE] Expected `Identifier`, got {:?}",
                    ident_token.t_type
                ),
            };

            ind += 2; // skip `HasType` token
            let param_type = &self.token_list.get(ind).unwrap().t_type;
            if !matches!(param_type, TokenType::ValType(_)) {
                panic!("[PARSE] Expected `ValType`, got {:?}", param_type);
            }
            out.push(ASTParamNode::new(name.to_string(), param_type.clone()));
            ind += 2; // try to point to name of next param, skipping `ItemSeparator` token
        }
        out
    }

    fn build_proc_decl_node(&self, start: usize, end: usize) -> ASTProcNode {
        let mut ind = start + 1; // start from proc name

        let name = match &self.token_list[ind].t_type {
            TokenType::Identifier(name) => name,
            _ => panic!(
                "[PARSE] expected an identifier, found {:?}",
                self.token_list[ind].t_type
            ),
        };

        let depth = self.token_list[ind].code_depth;

        ind += 1; // jump to lbracket

        let params = self.build_param_list(ind, end);

        ind += 2 + (3 * params.len()); // attempt to jump to end of params
        while !matches!(
            self.token_list[ind].t_type,
            TokenType::Returns | TokenType::StartBlock
        ) {
            ind += 1; // do the rest
        }

        let mut ret_type = None;

        if self.token_list[ind].t_type == TokenType::Returns {
            // returns is the arrow; jump ahead to return type
            ind += 1;
            ret_type = Some(self.token_list[ind].t_type.clone());
            ind += 1; // jump to startblock token
        }

        let body = if self.token_list[ind].t_type == TokenType::StartBlock {
            Box::new(Some(self.parse(ind, end)[0].clone())) //FIXME: this is gross
        } else {
            panic!("[PARSE] Proc declaration has no body?");
        };

        ASTProcNode {
            name: name.to_string(),
            body,
            depth: Some(depth),
            is_declaration: true,
            params,
            args: Vec::new(),
            ret_type,
        }
    }

    fn build_brackets(&self, start: usize, end: usize) -> ASTNodes {
        let ind = self.find_rbracket_index(start, end);
        let rbracket = self.token_list.get(ind).unwrap().clone();
        if !matches!(rbracket.t_type, TokenType::RBracket) {
            panic!("[PARSE] Expected ')' but found {:?} instead.", rbracket);
        }
        self.build_tree(start + 1, ind - 1)
    }

    fn find_end_of_block_index(&self, start: usize, end: usize) -> usize {
        let mut ind = start;

        if self.token_list.get(start).unwrap().t_type == TokenType::StartBlock {
            // find index of corresponding `EndBlock`
            let mut bracket_count = 0;
            while ind <= end {
                bracket_count += match self.token_list.get(ind).unwrap().t_type {
                    TokenType::StartBlock => 1,
                    TokenType::EndBlock => -1,
                    _ => 0,
                };
                if bracket_count == 0 {
                    return ind;
                }
                ind += 1;
            }
        } else {
            let start_depth = self.token_list.get(start).unwrap().code_depth;
            ind = start + 1;
            while ind <= end && self.token_list.get(ind).unwrap().code_depth >= start_depth {
                ind += 1;
            }
        }

        // don't go out of range
        if ind > end {
            end
        } else {
            ind
        }
    }

    fn build_tree(&self, mut start: usize, end: usize) -> ASTNodes {
        if start == end {
            return match &self.token_list.get(start).unwrap().t_type {
                TokenType::Identifier(_) | TokenType::Value(_) => Parser::astnode_from_token(
                    &self.token_list[start],
                    self.token_list.get(start - 1),
                    self.token_list.get(start + 1),
                )
                .unwrap_or_else(|| panic!("[PARSE] Error on token {:?}", self.token_list[start])),
                TokenType::End | TokenType::EndBlock => ASTNodes::ASTValNode(ASTValNode::new(
                    Value::Integer(0),
                    Some(self.token_list[start].code_depth),
                    false,
                )),
                _ => panic!(
                    "[PARSE] Expected `Identifier`,`Value`, `End`, or `EndBlock`. Found `{:?}` at index {}",
                    self.token_list[start], start
                ),
            };
        }

        // `StartBlock` and `EndBlock` tokens have already served their purpose at this stage; ignore them
        while matches!(
            self.token_list.get(start).unwrap().t_type,
            TokenType::StartBlock
        ) {
            start += 1;
        }

        let lhs;
        let mut op_offset = 1;

        if self.token_list.get(start).unwrap().t_type == TokenType::Make {
            return self.build_tree(start + 1, end);
        } else if let TokenType::Operator(_) = &self.token_list.get(start).unwrap().t_type {
            // first token is an operator (hopefully unary)
            lhs = None;
            op_offset = 0;
        } else if matches!(
            self.token_list.get(start).unwrap().t_type,
            TokenType::LBracket
        ) {
            // evaluate as lhs
            lhs = Some(self.build_brackets(start, end));

            // update op_offset
            op_offset = self.find_rbracket_index(start, end) + 1 - start;
        } else if start + 1 < self.token_list.len()
            && !matches!(
                self.token_list.get(start + 1).unwrap().t_type,
                TokenType::Operator(Operator::Multiplication)
                    | TokenType::Operator(Operator::Division)
            )
        {
            // no division/multiplication => evaluate normally
            lhs = Parser::astnode_from_token(
                &self.token_list[start],
                self.token_list.get(start - 1),
                self.token_list.get(start + 1),
            );
        } else {
            // have (possibly multiple occurrences of) division or multiplication
            let mut offset = 1;
            let mut last;

            while start + offset < self.token_list.len() && start + offset < end {
                last = &self.token_list[start + offset - 1];
                match &self.token_list[start + offset].t_type {
                    TokenType::Identifier(_) | TokenType::Value(_) => {
                        // looking for next operator
                        offset += 1;
                    }
                    TokenType::Operator(o) => match o {
                        Operator::Addition
                        | Operator::Subtraction
                        | Operator::BitwiseAnd
                        | Operator::BitwiseOr
                        | Operator::Equality
                        | Operator::Inequality
                        | Operator::Negation => {
                            if matches!(
                                last.t_type,
                                TokenType::Operator(Operator::Multiplication)
                                    | TokenType::Operator(Operator::Division)
                            ) {
                                // e.g., reading `...*-5...` in sequence of muls
                                offset += 2;
                            } else {
                                // broken chain of mul/div
                                op_offset = offset; // add/sub will be done last
                                break;
                            }
                        }
                        Operator::Multiplication | Operator::Division => {
                            op_offset = offset;
                            offset += 1;
                        }
                        Operator::Assignment => {
                            panic!(
                                "[PARSE] Expected `Addition`, `Subtraction`, `Multiplication`, or `Division. Found `{:?}`",
                                self.token_list[start + offset])
                        }
                    },
                    TokenType::End => break,
                    TokenType::LBracket => break,
                    _ => panic!(
                        "[PARSE] Expected `Operator`, `Identifier`, or `Value`. Found `{:?}`",
                        self.token_list[start + offset]
                    ),
                }
            }

            lhs = Some(self.build_tree(start, start + op_offset - 1));
        }

        if start + op_offset > end {
            return lhs.unwrap_or_else(|| {
                panic!(
                    "[PARSE] Unable to evaluate lhs `{:?}`",
                    &self.token_list[start..=end]
                )
            });
        }

        let op = match &self.token_list.get(start + op_offset).unwrap().t_type {
            TokenType::Operator(o) => Some(o),
            TokenType::End => return lhs.unwrap(),
            TokenType::EndBlock => {
                return match lhs.unwrap() {
                    ASTNodes::ASTNode(_) => todo!(), // place `is_ret = true` in bottom-rightmost child node
                    ASTNodes::ASTOpNode(_) => todo!(), // error? two consecutive operators
                    ASTNodes::ASTValNode(mut node) => {
                        node.is_ret = true;
                        ASTNodes::ASTValNode(node)
                    }
                    ASTNodes::ASTIdentifierNode(mut node) => {
                        node.is_ret = true;
                        ASTNodes::ASTIdentifierNode(node)
                    }
                    _ => todo!(),
                };
            }
            TokenType::LBracket => None, // start of param list, no operator
            TokenType::StartBlock => {
                // skip over this block; it's already part of an `ASTProcNode`
                let end_block = self.find_end_of_block_index(start + op_offset, end);
                op_offset = end_block - start-1;
                None
            }, // start of block of function we've already parsed
            TokenType::HasType => {
                None
            },  // param in function declaration
            _ => panic!(
                "[PARSE] Expected `Operator`, end of statement, or end of block. Found `{:?}` at index `{}`. lhs = {:?}",
                self.token_list[start + op_offset], start + op_offset, lhs,
            ),
        };

        if let Some(op) = op {
            ASTNodes::ASTOpNode(ASTOpNode::new(
                Box::new(lhs),
                Box::new(Some(self.build_tree(start + op_offset + 1, end))),
                op.clone(),
                Some(self.token_list.get(start).unwrap().code_depth),
            ))
        } else {
            if self.token_list.get(start + op_offset).unwrap().t_type == TokenType::EndBlock {
                // this block is already part of an `ASTProcNode`, so we do this to skip over it
                return ASTNodes::ASTBlockNode(ASTBlockNode::new(Vec::new(), start + op_offset));
            }

            // build param list
            let start_param_list = start + op_offset;
            let mut param_list = None;
            let mut arg_list = None;
            if let Some(ASTNodes::ASTIdentifierNode(ident)) = lhs.clone() {
                if !ident.is_declaration
                    && self.token_list.get(start_param_list).unwrap().t_type == TokenType::LBracket
                {
                    // proc call
                    arg_list = Some(self.build_arg_list(start_param_list, end));
                } else {
                    param_list = Some(self.build_param_list(start_param_list, end));
                }
            }

            // read return type of procedure
            let mut ret_type = None;
            if self.token_list.get(end + 1).unwrap().t_type == TokenType::Returns {
                ret_type = Some(self.token_list.get(end + 2).unwrap().t_type.clone());
            }

            let mut start_of_body = start + op_offset;
            while self.token_list.get(start_of_body).unwrap().t_type != TokenType::StartBlock
                && start_of_body <= end
            {
                start_of_body += 1;
            }
            let end_of_body = self.find_end_of_block_index(start_of_body, end);

            let body = ASTNodes::ASTBlockNode(ASTBlockNode::new(
                self.parse(start_of_body, end_of_body),
                start,
            ));

            ASTNodes::ASTProcNode(ASTProcNode::new(
                if let Some(lhs_node) = lhs.clone() {
                    match lhs_node {
                        ASTNodes::ASTIdentifierNode(ident) => ident.name,
                        _ => panic!(
                            "[PARSE] Expected `Identifier`, got {:?} at index {}",
                            lhs_node, start
                        ),
                    }
                } else {
                    panic!("[PARSE] Invalid lhs at index {start}");
                },
                Box::new(Some(body)),
                Some(self.token_list.get(start).unwrap().code_depth),
                match lhs.clone() {
                    Some(ASTNodes::ASTIdentifierNode(ident)) => ident.is_declaration,
                    _ => panic!("[PARSE] expected `ASTIdentifierNode`, found {:?}", lhs),
                },
                param_list.unwrap_or_default(),
                arg_list.unwrap_or_default(),
                ret_type,
            ))
        }
    }

    pub fn parse(&self, start: usize, end: usize) -> Vec<ASTNodes> {
        let mut ind = start;

        let mut nodes = Vec::new();

        while ind < self.token_list.len() && ind <= end {
            // build lhs

            if self.token_list[ind].t_type == TokenType::Proc {
                // build proc declaration

                let mut i = ind;
                while self.token_list[i].t_type != TokenType::StartBlock {
                    i += 1;
                }
                let end_of_proc = self.find_end_of_block_index(i, end);
                nodes.push(ASTNodes::ASTProcNode(
                    self.build_proc_decl_node(ind, end_of_proc),
                ));
                ind = end_of_proc + 1; // skip `EndBlock`
                continue; // procs don't have rhs
            }

            if matches!(self.token_list[ind].t_type, TokenType::Make) {
                ind += 1;
            }

            let mut start_lhs = ind;
            let mut end_lhs = ind;
            let lhs;

            if ind < self.token_list.len()
                && self.token_list[start_lhs].t_type == TokenType::StartBlock
            {
                let open_ind = start_lhs;
                start_lhs += 1;
                ind = start_lhs;
                // find end of block
                while ind <= end
                    && self.token_list[ind].code_depth == self.token_list[start_lhs].code_depth
                {
                    end_lhs = ind;
                    ind += 1;
                }
                let build_lhs = self.parse(start_lhs, end_lhs);
                lhs = ASTNodes::ASTBlockNode(ASTBlockNode::new(build_lhs, open_ind));
            } else {
                // find end of lhs
                while ind <= end && ind < self.token_list.len() {
                    if matches!(
                        self.token_list[ind].t_type,
                        TokenType::End
                            | TokenType::EndBlock
                            | TokenType::Operator(Operator::Assignment)
                            | TokenType::Returns
                    ) {
                        break;
                    }
                    end_lhs = ind;
                    if ind == end {
                        break;
                    }
                    ind += 1;
                }
                lhs = self.build_tree(start_lhs, end_lhs);
            }

            // check if done
            if ind < self.token_list.len()
                && (matches!(
                    self.token_list[ind].t_type,
                    TokenType::End | TokenType::EndBlock
                ))
            {
                nodes.push(lhs);
                ind += 1;
                continue; // this shouldn't have a rhs
            }

            // block nodes shouldn't be lhs => cannot have rhs => skip production of rhs
            if let ASTNodes::ASTBlockNode(_) = lhs {
                nodes.push(lhs);
                continue;
            }

            // build rhs
            ind += 1;
            let mut start_rhs = ind;
            let mut end_rhs = ind;
            let mut rhs = None;

            if ind < self.token_list.len() {
                if self.token_list[start_rhs].t_type == TokenType::StartBlock {
                    let open_ind = start_rhs;
                    start_rhs += 1;
                    ind = start_rhs;
                    // find end of block
                    while self.token_list[ind].code_depth == self.token_list[start_rhs].code_depth {
                        end_rhs = ind;
                        ind += 1;
                    }
                    let build_rhs = self.parse(start_rhs, end_rhs);
                    rhs = Some(ASTNodes::ASTBlockNode(ASTBlockNode::new(
                        build_rhs, open_ind,
                    )));
                } else {
                    let end_block_ind = self.find_end_of_block_index(start_rhs, end); // rhs could be block
                    while ind < end_block_ind && self.token_list[ind].t_type != TokenType::End {
                        end_rhs = ind;
                        ind += 1;
                    }
                    rhs = Some(self.build_tree(start_rhs, end_rhs));
                }

                ind += 1; // point to next line
            }

            if rhs.is_none() {
                nodes.push(lhs);
            } else {
                nodes.push(ASTNodes::ASTOpNode(ASTOpNode::new(
                    Box::new(Some(lhs)),
                    Box::new(rhs),
                    Operator::Assignment,
                    Some(self.token_list[start_lhs].code_depth),
                )));
            }
        }

        nodes
    }
}
