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
            return match &self.token_list[start].t_type {
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

        // division and multiplication at lower point of tree as higher precedence
        // checking operator is division/multiplication and next operator isn't the end
        // FIXME: could have multiple occurrences of same operator in a row
        // see example `l` in `maths.pd`
        if let TokenType::Operator(_) = &self.token_list[start].t_type {
            // first token is an operator (hopefully unary)
            lhs = None;
            op_offset = 0;
        } else if start + 1 < self.token_list.len()
            && (self.token_list[start + 1].t_type == TokenType::Operator(Operator::Division)
                || self.token_list[start + 1].t_type
                    == TokenType::Operator(Operator::Multiplication))
            && start + 3 < self.token_list.len()
            && start + 3 <= end
            && self.token_list[start + 3].t_type != TokenType::End
        {
            // need to push division/multiplication down to leaf of tree
            lhs = Some(self.build_tree(start, start + 2));
            op_offset = 3;
        } else {
            // no division/multiplication => evaluate normally
            lhs = Parser::astnode_from_token(&self.token_list[start]);
        }

        let op = match &self.token_list[start + op_offset].t_type {
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

            // check done or assignment

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
            )))
        }

        nodes
    }
}
