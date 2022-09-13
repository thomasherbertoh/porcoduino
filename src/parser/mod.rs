use std::collections::HashMap;

use crate::{
    astnode::{ASTIdentifierNode, ASTNodes, ASTOpNode, ASTValNode},
    tokens::{Operator, Token, TokenType, Value},
};

pub struct Parser {
    pub token_list: Vec<Token>,
    pub map: HashMap<String, Value>,
}

impl Parser {
    pub fn new(t_list: &[Token]) -> Self {
        Self {
            token_list: t_list.to_vec(),
            map: HashMap::new(),
        }
    }

    fn astnode_from_token(token: &Token) -> Option<ASTNodes> {
        match &token.t_type {
            TokenType::Identifier(name) => Some(ASTNodes::ASTIdentifierNode(
                ASTIdentifierNode::new(name.to_string()),
            )),
            TokenType::Value(v) => Some(ASTNodes::ASTValNode(ASTValNode::new(v.clone()))),
            TokenType::Operator(op) => Some(ASTNodes::ASTOpNode(ASTOpNode::new(
                Box::new(None),
                Box::new(None),
                op.clone(),
            ))),
            _ => None,
        }
    }

    fn build_tree(&self, start: usize, end: usize) -> ASTNodes {
        if start == end {
            return match &self.token_list.get(start).unwrap().t_type {
                TokenType::Identifier(_) | TokenType::Value(_) => {
                    Parser::astnode_from_token(&self.token_list[start])
                        .unwrap_or_else(|| panic!("error on token {:?}", self.token_list[start]))
                }
                _ => panic!(
                    "expected `Identifier` or `Value` but found {:?} at index {}",
                    self.token_list[start].wordy, start
                ),
            };
        }

        let lhs;
        let mut op_offset = 1;

        if let TokenType::Operator(_) = &self.token_list.get(start).unwrap().t_type {
            // first token is an operator (hopefully unary)
            lhs = None;
            op_offset = 0;
        } else if start + 1 < self.token_list.len()
            && !matches!(
                self.token_list.get(start + 1).unwrap().t_type,
                TokenType::Operator(Operator::Multiplication)
                    | TokenType::Operator(Operator::Division)
            )
        {
            // no division/multiplication => evaluate normally
            lhs = Parser::astnode_from_token(&self.token_list[start]);
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
                        Operator::Addition | Operator::Subtraction => {
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
                            offset += 2;
                        }
                        Operator::Assignment => {
                            panic!("Unexpected token {:?}", self.token_list[start + offset])
                        }
                    },
                    TokenType::End => break,
                    _ => panic!("Unexpected token {:?}", self.token_list[start + offset]),
                }
            }

            lhs = Some(self.build_tree(start, start + op_offset - 1));
        }

        let op = match &self.token_list.get(start + op_offset).unwrap().t_type {
            TokenType::Operator(t) => match t {
                Operator::Assignment => {
                    panic!(
                        "didn't expect assignment operator at index {}",
                        start + op_offset
                    )
                }
                _ => t,
            },
            _ => panic!(
                "expected token at index {} to be an operator, but found {:?}",
                start + op_offset,
                self.token_list[start + op_offset].wordy
            ),
        };

        ASTNodes::ASTOpNode(ASTOpNode::new(
            Box::new(lhs),
            Box::new(Some(self.build_tree(start + op_offset + 1, end))),
            op.clone(),
        ))
    }

    pub fn parse(&self) -> Vec<ASTNodes> {
        let mut ind = 0;

        let mut nodes = Vec::new();

        while ind < self.token_list.len() {
            // build lhs
            if self.token_list[ind].t_type == TokenType::Make {
                ind += 1;
            }

            let start_lhs = ind;
            let mut end_lhs = ind;

            // find end of lhs
            while self.token_list[ind].t_type != TokenType::End
                && self.token_list[ind].t_type != TokenType::Operator(Operator::Assignment)
            {
                end_lhs = ind;
                ind += 1;
            }

            let lhs = self.build_tree(start_lhs, end_lhs);

            // check if done
            if self.token_list[ind].t_type == TokenType::End {
                nodes.push(lhs);
                ind += 1;
                continue;
            }

            // build rhs
            ind += 1;
            let start_rhs = ind;
            let mut end_rhs = ind;
            while self.token_list[ind].t_type != TokenType::End {
                end_rhs = ind;
                ind += 1;
            }
            let rhs = self.build_tree(start_rhs, end_rhs);

            ind += 1; // point to next line

            nodes.push(ASTNodes::ASTOpNode(ASTOpNode::new(
                Box::new(Some(lhs)),
                Box::new(Some(rhs)),
                Operator::Assignment,
            )));
        }

        nodes
    }
}
