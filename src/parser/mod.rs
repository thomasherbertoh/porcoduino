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
                ASTIdentifierNode::new(name.to_string(), token.code_depth),
            )),
            TokenType::Value(v) => Some(ASTNodes::ASTValNode(ASTValNode::new(v.clone()))),
            TokenType::Operator(op) => Some(ASTNodes::ASTOpNode(ASTOpNode::new(
                Box::new(None),
                Box::new(None),
                op.clone(),
                token.code_depth,
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

    fn build_brackets(&self, start: usize, end: usize) -> ASTNodes {
        let ind = self.find_rbracket_index(start, end);
        let rbracket = self.token_list.get(ind).unwrap().clone();
        if !matches!(rbracket.t_type, TokenType::RBracket) {
            panic!("[PARSE] Expected ')' but found {:?} instead.", rbracket);
        }
        self.build_tree(start + 1, ind - 1)
    }

    fn build_tree(&self, start: usize, end: usize) -> ASTNodes {
        if start == end {
            return match &self.token_list.get(start).unwrap().t_type {
                TokenType::Identifier(_) | TokenType::Value(_) => {
                    Parser::astnode_from_token(&self.token_list[start])
                        .unwrap_or_else(|| panic!("error on token {:?}", self.token_list[start]))
                }
                _ => panic!(
                    "[PARSE] Expected `Identifier` or `Value`. Found `{:?}`",
                    self.token_list[start].wordy
                ),
            };
        }

        let lhs;
        let mut op_offset = 1;

        if let TokenType::Operator(_) = &self.token_list.get(start).unwrap().t_type {
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

        let op = match &self.token_list.get(start + op_offset).unwrap().t_type {
            TokenType::Operator(t) => match t {
                Operator::Assignment => {
                    panic!(
                        "[PARSE] Unexpected assignment operator at index {}",
                        start + op_offset
                    )
                }
                _ => t,
            },
            TokenType::End => return lhs.unwrap(),
            _ => panic!(
                "[PARSE] Expected `Operator`. Found `{:?}`",
                self.token_list[start + op_offset]
            ),
        };

        ASTNodes::ASTOpNode(ASTOpNode::new(
            Box::new(lhs),
            Box::new(Some(self.build_tree(start + op_offset + 1, end))),
            op.clone(),
            self.token_list.get(start).unwrap().code_depth,
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
                self.token_list[start_lhs].code_depth,
            )));
        }

        nodes
    }
}
