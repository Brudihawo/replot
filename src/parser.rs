use crate::ast::{BinaryExpression, EvalASTNode, Literal, Name, SyntaxError, UnaryExpression};
use crate::tokenize::{OperatorType, Token, TokenType};
use std::cmp::Ordering;

#[derive(Debug)]
struct TokenInfo<'a> {
    token: &'a Token,
    depth: usize,
    index: usize,
}

#[derive(Debug, PartialEq, Eq)]
struct OperatorInfo<'a> {
    op_type: &'a OperatorType,
    token: &'a Token,
    depth: usize,
    index: usize,
}

impl<'a> Ord for OperatorInfo<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.depth < other.depth {
            Ordering::Less
        } else if self.depth > other.depth {
            Ordering::Greater
        } else {
            // equal depth
            if self.op_type > other.op_type {
                Ordering::Greater
            } else if self.op_type < other.op_type {
                Ordering::Less
            } else {
                if self.token.location > other.token.location {
                    Ordering::Greater
                } else if self.token.location < other.token.location {
                    Ordering::Less
                } else {
                    Ordering::Equal
                }
            }
        }
    }
}

impl<'a> PartialOrd for OperatorInfo<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

struct Parser<'a> {
    tokens: Vec<Token>,
    token_info: Vec<TokenInfo<'a>>,
    operator_info: Vec<OperatorInfo<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            operator_info: Vec::new(),
            token_info: Vec::new(),
        }
    }

    fn surrounded_by_parens(range: &[TokenInfo]) -> bool {
        if range.len() < 2 {
            return false;
        }

        let matched_parens = range[1..range.len() - 1].iter().fold(0, |acc, token_info| {
            if acc < 0 {
                // if at any point there is a right paren before a left one, this should
                // be negative and stay negative
                -1
            } else {
                match token_info.token.token_type {
                    TokenType::Lparen => acc + 1,
                    TokenType::Rparen => acc - 1,
                    _ => acc,
                }
            }
        });
        println!("{}", matched_parens);

        (matched_parens == 0)
            && (range.first().unwrap().token.token_type == TokenType::Lparen)
            && (range.last().unwrap().token.token_type == TokenType::Rparen)
    }

    fn parse_expressions(&self, range: &[TokenInfo]) -> Result<Box<dyn EvalASTNode>, SyntaxError> {
        assert!(!range.is_empty());
        println!("================================================================");

        // Remove Parens around current scope
        if Self::surrounded_by_parens(range) {
            // This has to be larger than 2 now
            println!("Removing Surrounding Parens");
            if range.len() == 2 {
                return Err(SyntaxError::EmptyParenScope(
                    range[0].token.location,
                    range[1].token.location,
                ));
            }
            return self.parse_expressions(&range[1..range.len() - 1]);
        }

        match range.len() {
            1 => {
                // Scope contains one token
                // only valid for Literals and Names
                let token = &range[0].token;
                match token.token_type {
                    TokenType::Name(ref name) => {
                        return Ok(Name::new_box(name.clone(), token.location));
                    }
                    TokenType::Literal(value) => {
                        return Ok(Literal::new_box(value, token.location));
                    }
                    _ => return Err(SyntaxError::CannotEvaluate(token.location)),
                }
            }
            2 => {
                // scope contains 2 elements
                // this needs to be a unary expression
                return Ok(Box::new(
                    UnaryExpression::try_from(
                        range.first().unwrap().token,
                        range.last().unwrap().token,
                    )
                    .map_err(|_| {
                        SyntaxError::NonEvaluatableScope(
                            range.first().unwrap().token.location,
                            range.last().unwrap().token.location,
                        )
                    })?,
                ));
            }
            3 => {
                // scope contains 3 elements
                // this needs to be a binary expression or brackets around a single name or value
                if let TokenType::Operator(op) = range[1].token.token_type {
                    let lhs: Box<dyn EvalASTNode> = match range[0].token.token_type {
                        TokenType::Name(ref name) => {
                            Name::new_box(name.clone(), range[0].token.location)
                        }
                        TokenType::Literal(value) => {
                            Literal::new_box(value, range[0].token.location)
                        }
                        _ => {
                            return Err(SyntaxError::InvalidBinaryOperand(range[0].token.location))
                        }
                    };

                    let rhs: Box<dyn EvalASTNode> = match range[2].token.token_type {
                        TokenType::Name(ref name) => {
                            Name::new_box(name.clone(), range[2].token.location)
                        }
                        TokenType::Literal(value) => {
                            Literal::new_box(value, range[2].token.location)
                        }
                        _ => {
                            return Err(SyntaxError::InvalidBinaryOperand(range[2].token.location))
                        }
                    };

                    Ok(Box::new(
                        BinaryExpression::new(op, lhs, rhs).expect("Equals are handled elsewhere"),
                    ))
                } else {
                    Err(SyntaxError::InvalidBinaryOperator(range[1].token.location))
                }
            }
            _ => {
                // get operators
                let mut begin = 0;
                while range.first().unwrap().token.location
                    >= self.operator_info[begin].token.location
                {
                    begin += 1;
                }
                let mut end = begin;
                while (end < self.operator_info.len())
                    && (range.last().unwrap().token.location
                        >= self.operator_info[end].token.location)
                {
                    end += 1;
                }

                // get operator with lowest precedence.
                // This will be the top node of the expression tree
                let operator = self.operator_info[begin..end].iter().min().unwrap();

                println!("Operator: {:?}", operator.op_type);

                let l_end = operator.index - range[0].index;
                let mut l_first = l_end - 1;
                let depth = operator.depth;

                while (l_first > 0) && (range[l_first].depth >= depth) {
                    l_first -= 1;
                }

                println!(
                    "Left: {}",
                    range[l_first..l_end]
                        .iter()
                        .fold("".to_string(), |acc, x| format!("{}\n{:?}", acc, x))
                );
                let lhs = self.parse_expressions(&range[l_first..l_end])?;
                println!("Left parsed: {}", lhs.simple_print());

                let r_first = operator.index - range[0].index + 1;
                let mut r_end = r_first + 1;

                while (r_end < range.len()) && (range[r_end].depth >= depth) {
                    r_end += 1;
                }

                println!(
                    "Right: {}",
                    range[r_first..r_end]
                        .iter()
                        .fold("".to_string(), |acc, x| format!("{}\n{:?}", acc, x))
                );
                println!("=======================================================");
                let rhs = self.parse_expressions(&range[r_first..r_end])?;
                println!("Right Parsed: {}", rhs.simple_print());

                BinaryExpression::new_box(*operator.op_type, lhs, rhs)
                    .map_err(|_| SyntaxError::MisplacedEquals(operator.token.location))
                    .map(|x| x as Box<dyn EvalASTNode>)
            }
        }
    }

    pub fn parse(&'a mut self) -> Result<Box<dyn EvalASTNode>, SyntaxError> {
        let mut depth: usize = 0;

        self.token_info.reserve(self.tokens.len());
        for (index, token) in self.tokens.iter().enumerate() {
            match token.token_type {
                TokenType::Lparen => {
                    depth += 1;
                }
                TokenType::Rparen => depth -= 1,
                TokenType::Operator(ref op_type) => {
                    self.operator_info.push(OperatorInfo {
                        op_type,
                        token,
                        depth,
                        index,
                    });
                }
                _ => {}
            }
            self.token_info.push(TokenInfo {
                index,
                token,
                depth: if token.token_type == TokenType::Rparen {
                    depth + 1
                } else {
                    depth
                },
            });
        }

        // check for assignments
        match self
            .operator_info
            .iter()
            .filter(|x| *x.op_type == OperatorType::Equals)
            .count()
        {
            0 => self.parse_expressions(&self.token_info), // we good
            1 => {
                todo!()
            }   // we have an assignment expression
            _ => {
                return Err(SyntaxError::MultipleEquals(
                    self.operator_info
                        .iter()
                        .filter(|x| *x.op_type == OperatorType::Equals)
                        .map(|x| x.token.location)
                        .collect(),
                ));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenize::Lexer;

    #[test]
    fn add() {
        let tokens = Lexer::new("a + b")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(output.simple_print(), "(a + b)");
    }

    #[test]
    fn redundant_plus() {
        let tokens = Lexer::new("(+ b)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(output.simple_print(), "(+ b)");
    }

    #[test]
    fn negate() {
        let tokens = Lexer::new("(- b)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(output.simple_print(), "(- b)");
    }

    #[test]
    fn only_val() {
        let tokens = Lexer::new("(1.0)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(output.simple_print(), "1");
    }

    #[test]
    fn only_name() {
        let tokens = Lexer::new("(test)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(output.simple_print(), "test");
    }

    #[test]
    fn add_and_multiply() {
        let tokens = Lexer::new("a + b * c")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(output.simple_print(), "(a + (b * c))");
    }

    #[test]
    fn add_and_multiply_parens() {
        let tokens = Lexer::new("(a + b) * c")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(output.simple_print(), "((a + b) * c)");
    }

    #[test]
    fn double_add() {
        let tokens = Lexer::new("a + b + c")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(output.simple_print(), "(a + (b + c))");
    }

    #[test]
    fn add_2_fracs() {
        let tokens = Lexer::new("a / 2.0 - c / 3.0")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(output.simple_print(), "((a / 2) - (c / 3))");
    }

    #[test]
    fn power() {
        let tokens = Lexer::new("a^b")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(output.simple_print(), "(a ^ b)");
    }

    #[test]
    fn power_add() {
        let tokens = Lexer::new("a^(b + c)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(output.simple_print(), "(a ^ (b + c))");
    }

    #[test]
    fn exponent_2_fracs() {
        let tokens = Lexer::new("(a / 2.0) ^ (c / 3.0)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(output.simple_print(), "((a / 2) ^ (c / 3))");
    }

    #[test]
    fn all_ops() {
        let tokens = Lexer::new("a + b - c * d / e ^ f")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(output.simple_print(), "(a + (b - (c * (d / (e ^ f)))))");
    }

    #[test]
    fn quadratic_formula_plus() {
        let tokens = Lexer::new("-p/2 + ((p/2)^2 - q)^(1 / 2)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(
            output.simple_print(),
            "(((- p) / 2) + ((((p / 2) ^ 2) - q) ^ (1 / 2)))"
        );
    }

    #[test]
    fn assignment_func() {
        let tokens = Lexer::new("f(p, q) = -p/2 + ((p/2)^2 - q)^(1 / 2)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(
            output.simple_print(),
            "f(p, q) = (((- p) / 2) + ((((p / 2) ^ 2) - q) ^ (1 / 2)))"
        );
    }

    fn assignment_simple() {
        let tokens = Lexer::new("x = 1")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(
            output.simple_print(),
            "x = 1"
        );
    }
    // TODO(Hawo): Need More Test Cases. These nested brackets seem to be trickier than i thought.
    // TODO(Hawo): I may need to work on selection of neighboring scopes, but for now it's fine
}
