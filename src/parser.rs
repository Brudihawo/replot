use crate::ast::{
    Assignment, AssignmentError, AssignmentLHS, AssignmentRHS, BinaryExpression, EvalASTNode,
    FunctionNode, Literal, Name, Seq, SyntaxError, UnaryExpression,
};
use crate::tokenize::{KeyWord, OperatorType, Token, TokenType};
use std::cmp::Ordering;
use std::fmt::Display;

macro_rules! ti_with {
    ($p:pat) => {
        TokenInfo {
            token: Token { token_type: $p, .. },
            ..
        }
    };
}

macro_rules! ti_match_list {
    ($($p:pat),+ $(,)?) => {
        [
            $(TokenInfo {
                token: Token {
                    token_type: $p,
                    ..
                },
               ..
            }),+
        ]
    };
}

#[derive(Debug)]
pub enum Argument {
    Name(Name),
    Literal(Literal),
    Function(FunctionNode),
    Expression(Box<dyn EvalASTNode>),
}

impl std::fmt::Display for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Argument::Name(name) => write!(f, "{}", name),
            Argument::Literal(lit) => write!(f, "{}", lit),
            Argument::Function(func) => write!(f, "{}", func),
            Argument::Expression(xpr) => write!(f, "{}", xpr),
        }
    }
}

enum ParsedFunction {
    WithNames(FunctionNode),
    MustEval(Box<dyn EvalASTNode>),
}

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
                if self.token.loc > other.token.loc {
                    Ordering::Greater
                } else if self.token.loc < other.token.loc {
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

pub enum Command {
    Sequence(Seq),
    // TODO(Hawo): Plot
}

pub enum ParseResult {
    Eval(Box<dyn EvalASTNode>),
    Command(Command),
    Definition(Assignment),
}

impl std::fmt::Display for ParseResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eval(ast) => ast.fmt(f),
            Self::Definition(ass) => ass.fmt(f),
            Self::Command(cmd) => match cmd {
                Command::Sequence(seq) => seq.fmt(f),
            },
        }
    }
}

pub struct Parser<'a> {
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

        (matched_parens == 0)
            && (range.first().unwrap().token.token_type == TokenType::Lparen)
            && (range.last().unwrap().token.token_type == TokenType::Rparen)
    }

    fn strip_parens(range: &'a [TokenInfo<'a>]) -> Result<&'a [TokenInfo<'a>], SyntaxError> {
        if Self::surrounded_by_parens(range) {
            // This has to be larger than 2 now
            if range.len() == 2 {
                return Err(SyntaxError::EmptyParenScope(
                    range[0].token.loc,
                    range[1].token.loc,
                ));
            }
            return Ok(&range[1..range.len() - 1]);
        } else {
            return Ok(&range);
        }
    }

    fn parse_expression(
        &self,
        range: &'a [TokenInfo<'a>],
    ) -> Result<Box<dyn EvalASTNode>, SyntaxError> {
        assert!(!range.is_empty());

        let range = Self::strip_parens(range)?;

        match range.len() {
            1 => {
                // Scope contains one token
                // only valid for Literals and Names
                let token = &range[0].token;
                match token.token_type {
                    TokenType::Name(ref name) => {
                        return Ok(Name::new_box(name.clone(), token.loc));
                    }
                    TokenType::Literal(value) => {
                        return Ok(Literal::new_box(value, token.loc));
                    }
                    _ => return Err(SyntaxError::CannotEvaluate(token.loc)),
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
                            range.first().unwrap().token.loc,
                            range.last().unwrap().token.loc,
                        )
                    })?,
                ));
            }
            3 => {
                // scope contains 3 elements
                // this needs to be a binary expression or brackets around a single name or value
                // if this is a function with no arguments throw an error for now
                if matches!(
                    range[0..3],
                    ti_match_list![TokenType::Name(_), TokenType::Lparen, TokenType::Rparen]
                ) {
                    return Err(SyntaxError::UnexpectedToken(range[2].token.clone()));
                }

                if let TokenType::Operator(op) = range[1].token.token_type {
                    let lhs: Box<dyn EvalASTNode> = match range[0].token.token_type {
                        TokenType::Name(ref name) => {
                            Name::new_box(name.clone(), range[0].token.loc)
                        }
                        TokenType::Literal(value) => Literal::new_box(value, range[0].token.loc),
                        _ => return Err(SyntaxError::InvalidBinaryOperand(range[0].token.loc)),
                    };

                    let rhs: Box<dyn EvalASTNode> = match range[2].token.token_type {
                        TokenType::Name(ref name) => {
                            Name::new_box(name.clone(), range[2].token.loc)
                        }
                        TokenType::Literal(value) => Literal::new_box(value, range[2].token.loc),
                        _ => return Err(SyntaxError::InvalidBinaryOperand(range[2].token.loc)),
                    };

                    Ok(Box::new(
                        BinaryExpression::new(op, lhs, rhs).expect("Equals are handled elsewhere"),
                    ))
                } else {
                    Err(SyntaxError::InvalidBinaryOperator(range[1].token.loc))
                }
            }
            _ => {
                // If it is a function parse the function
                if let Some(kw_func) = self.try_parse_keyword(&range)? {
                    match kw_func {
                        Command::Sequence(seq) => return Ok(Box::new(seq)),
                    }
                    // TODO(Hawo): Continue debugging here!!
                    // TODO(Hawo): How do i handle function head / function call differently?
                }
                if let Some(func) = self.try_parse_function(&range)? {
                    match func {
                        ParsedFunction::WithNames(func) => {
                            return Ok(Box::new(func));
                        }
                        ParsedFunction::MustEval(_) => todo!(),
                    }
                    // TODO(Hawo): Continue debugging here!!
                    // TODO(Hawo): How do i handle function head / function call differently?
                }

                // we are not parsing something that is only a function or keyword expression
                // get operators
                let mut begin = 0;
                while range.first().unwrap().token.loc >= self.operator_info[begin].token.loc {
                    begin += 1;
                }
                let mut end = begin;
                while (end < self.operator_info.len())
                    && (range.last().unwrap().token.loc >= self.operator_info[end].token.loc)
                {
                    end += 1;
                }

                // get operator with lowest precedence.
                // This will be the top node of the expression tree
                let operator = self.operator_info[begin..end].iter().min().unwrap();
                let depth = operator.depth;

                let l_end = operator.index - range[0].index;
                let mut l_first = l_end - 1;
                while (l_first > 0) && (range[l_first].depth >= depth) {
                    l_first -= 1;
                }
                let lhs = self.parse_expression(&range[l_first..l_end])?;

                let r_first = operator.index - range[0].index + 1;
                let mut r_end = r_first + 1;
                while (r_end < range.len()) && (range[r_end].depth >= depth) {
                    r_end += 1;
                }
                let rhs = self.parse_expression(&range[r_first..r_end])?;

                BinaryExpression::new_box(*operator.op_type, lhs, rhs)
                    .map_err(|_| SyntaxError::MisplacedEquals(operator.token.loc))
                    .map(|x| x as Box<dyn EvalASTNode>)
            }
        }
    }

    fn try_parse_keyword(
        &self,
        range: &'a [TokenInfo<'a>],
    ) -> Result<Option<Command>, SyntaxError> {
        // TODO(Hawo): Change handling of keyword parsing
        // Try to parse a function
        if !matches!(
            range[0..2],
            ti_match_list![TokenType::Keyword(_), TokenType::Lparen],
        ) {
            return Ok(None);
        }

        let kw = if let TokenType::Keyword(kw) = &range[0].token.token_type {
            kw
        } else {
            unreachable!()
        };

        match kw {
            KeyWord::Seq => {
                let mut start = 1;
                let var_list = self.try_parse_params(Self::strip_parens(&range[start..])?)?;
                if range[start].token.token_type == TokenType::Lparen {
                    start += 1;
                }

                if let Some((index, _)) = var_list
                    .iter()
                    .enumerate()
                    .find(|(_, x)| !matches![x, Argument::Literal(_)])
                {
                    println!("Arguments: {:?}", var_list);
                    return Err(SyntaxError::InvalidArgument(range[start + index].token.clone()));
                }
                let var_list: Vec<&Literal> = var_list
                    .iter()
                    .map(|x| match x {
                        Argument::Literal(lit) => lit,
                        _ => unreachable!(),
                    })
                    .collect();
                match var_list.len() {
                    0..=2 => {
                        // we know var_list has at least length 1, otherwise
                        // self.try_parse_var_list throws an error
                        Err(SyntaxError::NotEnoughArguments(
                            var_list.last().unwrap().loc,
                        ))
                    }
                    3 => Ok(Some(Command::Sequence(Seq::new(
                        var_list[0].value,
                        var_list[1].value,
                        var_list[2].value as usize,
                    )))),
                    _ => Err(SyntaxError::TooManyArguments(var_list[3].loc)),
                }
            }
            KeyWord::Plot => todo!(),
        }

        // 1 => {
        //     if !range[0].index == 0 {
        //         Err(SyntaxError::InvalidKeywordPosition(range[0].token.loc))
        //     } else {
        //         if let TokenType::Keyword(kw) = range[0].token.token_type.clone() {
        //             match kw {
        //                 KeyWord::Plot => unimplemented!("plotting is not implemented yet"),
        //                 KeyWord::Seq => {
        //                     let var_list =
        //                         self.try_parse_params(Self::strip_parens(&range[1..])?)?;
        //                     if let Some(it) =
        //                         var_list.iter().find(|x| !matches![x, Argument::Literal(_)])
        //                     {
        //                         return Err(todo!());
        //                     }
        //                     let var_list: Vec<&Literal> = var_list
        //                         .iter()
        //                         .map(|x| match x {
        //                             Argument::Literal(lit) => lit,
        //                             _ => unreachable!(),
        //                         })
        //                         .collect();
        //                     match var_list.len() {
        //                         0..=2 => {
        //                             // we know var_list has at least length 1, otherwise
        //                             // self.try_parse_var_list throws an error
        //                             Err(SyntaxError::NotEnoughArguments(
        //                                 var_list.last().unwrap().loc,
        //                             ))
        //                         }
        //                         3 => Ok(Some(Command::Sequence(Seq::new(
        //                             var_list[0].value,
        //                             var_list[1].value,
        //                             var_list[2].value as usize,
        //                         )))),
        //                         _ => Err(SyntaxError::TooManyArguments(var_list[3].loc)),
        //                     }
        //                 }
        //             }
        //         } else {
        //             unreachable!("We filtered by this before")
        //         }
        //     }
        // }
    }

    fn parse_assignment_rhs(&self, tokens: &[TokenInfo]) -> Result<AssignmentRHS, SyntaxError> {
        if tokens.len() == 1 {
            match &tokens[0].token.token_type {
                TokenType::Name(name) => Ok(AssignmentRHS::Name(Name {
                    name: name.clone(),
                    loc: tokens[0].token.loc,
                })),
                TokenType::Literal(val) => Ok(AssignmentRHS::Single(*val)),
                _ => Err(SyntaxError::InvalidAssignmentRHS(tokens[0].token.loc)),
            }
        } else {
            if let Some(cmd) = self.try_parse_keyword(tokens)? {
                match cmd {
                    Command::Sequence(seq) => Ok(AssignmentRHS::Multiple(seq)),
                }
            } else {
                if let Some(rhs) = self.try_parse_function(tokens)? {
                    match rhs {
                        // if rhs is function, we need to evaluate the function.
                        ParsedFunction::WithNames(func) => Ok(AssignmentRHS::Function(func)),
                        ParsedFunction::MustEval(ast) => Ok(AssignmentRHS::Expression(ast)),
                    }
                } else {
                    Ok(AssignmentRHS::Expression(self.parse_expression(tokens)?))
                }
            }
        }
    }

    fn try_parse_param(&self, range: &[TokenInfo]) -> Result<Argument, SyntaxError> {
        if range.len() == 1 {
            return match range[0] {
                ti_with![TokenType::Name(ref name)] => {
                    Ok(Argument::Name(Name::new(name.clone(), range[0].token.loc)))
                }
                ti_with![TokenType::Literal(ref val)] => {
                    Ok(Argument::Literal(Literal::new(*val, range[0].token.loc)))
                }
                _ => Err(SyntaxError::UnexpectedToken(range[0].token.clone())),
            };
        }
        // TODO(Hawo): For now, this only supports Literals and Names.
        // TODO(Hawo): This needs to support expressions and Functions in the future
        todo!()
    }

    /// Parse a variable list from a list of tokens without surrounding parens
    /// Input: a, b, c
    /// Output: Vec<FunctionParam>(a, b, c)
    fn try_parse_params(&self, range: &[TokenInfo]) -> Result<Vec<Argument>, SyntaxError> {
        let mut var_list = Vec::<Argument>::new();
        let mut comma_next = true;

        let mut start = 0;
        let mut end = 0;
        while let Some(ti) = range.get(end) {
            if !(ti.token.token_type == TokenType::Comma) {
                end += 1;
                continue;
            }

            // We have found a comma
            var_list.push(self.try_parse_param(&range[start..end])?);
            start = end + 1;
            end = start;
        }
        if start == end {
            return Err(SyntaxError::UnexpectedToken(range[end - 1].token.clone()));
        }
        var_list.push(self.try_parse_param(&range[start..end])?);

        Ok(var_list)
    }

    fn try_parse_function(
        &self,
        tokens: &'a [TokenInfo],
    ) -> Result<Option<ParsedFunction>, SyntaxError> {
        // Minimal function signature has length 4:  Ident ( var1 )
        // var may be a name, function or literal
        if tokens.len() < 4 {
            Ok(None)
        } else {
            // Try to parse a function
            if matches!(
                tokens[0..2],
                ti_match_list![TokenType::Name(_), TokenType::Lparen],
            ) {
                let func_name = if let TokenType::Name(name) = &tokens[0].token.token_type {
                    name.clone()
                } else {
                    unreachable!()
                };

                if crate::tokenize::KEYWORDS.contains_key(&func_name) {
                    return Err(SyntaxError::InvalidFunctionName(tokens[0].token.clone()));
                }

                let var_list = self.try_parse_params(Self::strip_parens(&tokens[1..])?)?;
                return Ok(Some(ParsedFunction::WithNames(FunctionNode {
                    loc: tokens[0].token.loc,
                    name: func_name,
                    args: var_list,
                })));
            }
            Ok(None)
        }
    }

    fn parse_assignment_lhs(&self, tokens: &[TokenInfo]) -> Result<AssignmentLHS, SyntaxError> {
        match tokens.len() {
            0 => Err(SyntaxError::EmptyAssignmentLHS),
            1 => {
                if let TokenType::Name(name) = &tokens[0].token.token_type {
                    Ok(AssignmentLHS::Name(Name {
                        name: name.clone(),
                        loc: tokens[0].token.loc,
                    }))
                } else {
                    Err(SyntaxError::InvalidAssignmentLHS(tokens[0].token.loc))
                }
            }
            _ => {
                if let Some(func) = self.try_parse_function(tokens)? {
                    match func {
                        ParsedFunction::WithNames(head) => Ok(AssignmentLHS::Function(head)),
                        // Cannot assign to a function evaluation -> this is an rvalue
                        ParsedFunction::MustEval(_) => {
                            Err(SyntaxError::InvalidAssignmentLHS(tokens[0].token.loc))
                        }
                    }
                } else {
                    Err(SyntaxError::ExpectedFunctionDef(tokens[0].token.loc))
                }
            }
        }
    }

    pub fn parse(&'a mut self) -> Result<ParseResult, SyntaxError> {
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
            0 => {
                if let Some(cmd) = self.try_parse_keyword(&self.token_info)? {
                    Ok(ParseResult::Command(cmd))
                } else {
                    Ok(ParseResult::Eval(self.parse_expression(&self.token_info)?))
                }
            }
            1 => {
                let equals = self
                    .operator_info
                    .iter()
                    .filter(|x| *x.op_type == OperatorType::Equals)
                    .next()
                    .unwrap();

                Ok(ParseResult::Definition(Assignment::from(
                    self.parse_assignment_lhs(&self.token_info[..equals.index])?,
                    self.parse_assignment_rhs(&self.token_info[equals.index + 1..])?,
                    equals.token.loc,
                )?))
            } // we have an assignment expression

            _ => {
                return Err(SyntaxError::MultipleEquals(
                    self.operator_info
                        .iter()
                        .filter(|x| *x.op_type == OperatorType::Equals)
                        .map(|x| x.token.loc)
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
        assert_eq!(format!("{}", output), "(a + b)");
    }

    #[test]
    fn redundant_plus() {
        let tokens = Lexer::new("(+ b)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "(+ b)");
    }

    #[test]
    fn negate() {
        let tokens = Lexer::new("(- b)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "(- b)");
    }

    #[test]
    fn only_val() {
        let tokens = Lexer::new("(1.0)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "1");
    }

    #[test]
    fn only_name() {
        let tokens = Lexer::new("(test)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "test");
    }

    #[test]
    fn add_and_multiply() {
        let tokens = Lexer::new("a + b * c")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "(a + (b * c))");
    }

    #[test]
    fn add_and_multiply_parens() {
        let tokens = Lexer::new("(a + b) * c")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "((a + b) * c)");
    }

    #[test]
    fn double_add() {
        let tokens = Lexer::new("a + b + c")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "(a + (b + c))");
    }

    #[test]
    fn add_2_fracs() {
        let tokens = Lexer::new("a / 2.0 - c / 3.0")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "((a / 2) - (c / 3))");
    }

    #[test]
    fn power() {
        let tokens = Lexer::new("a^b")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "(a ^ b)");
    }

    #[test]
    fn power_add() {
        let tokens = Lexer::new("a^(b + c)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "(a ^ (b + c))");
    }

    #[test]
    fn exponent_2_fracs() {
        let tokens = Lexer::new("(a / 2.0) ^ (c / 3.0)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "((a / 2) ^ (c / 3))");
    }

    #[test]
    fn all_ops() {
        let tokens = Lexer::new("a + b - c * d / e ^ f")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "(a + (b - (c * (d / (e ^ f)))))");
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
            format!("{}", output),
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
            format!("{}", output),
            "f(p, q) = (((- p) / 2) + ((((p / 2) ^ 2) - q) ^ (1 / 2)))"
        );
    }

    #[test]
    fn assignment_simple() {
        let tokens = Lexer::new("x = 1")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "x = 1");
    }

    #[test]
    fn assignment_seq() {
        let tokens = Lexer::new("x = seq(0, 1, 10)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "x = seq(0, 1, 10)");
    }

    #[test]
    fn seq() {
        let tokens = Lexer::new("seq(0, 1, 10)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "seq(0, 1, 10)");
    }

    #[test]
    fn func_3_vars() {
        let tokens = Lexer::new("func(0, 1, 10)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "func(0, 1, 10)");
    }

    #[test]
    fn func_1_var() {
        let tokens = Lexer::new("func(0)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "func(0)");
    }

    #[test]
    fn seq_in_expr() {
        let tokens = Lexer::new("1 + seq(0, 1, 10)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        println!("{:?}", tokens);
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "(1 + seq(0, 1, 10))");
    }

    #[test]
    fn seq_in_expr_assignment() {
        let tokens = Lexer::new("x = 1 + seq(0, 1, 10)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        println!("{:?}", tokens);
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "x = (1 + seq(0, 1, 10))");
    }

    #[test]
    fn seq_in_rhs_assignment() {
        let tokens = Lexer::new("f(x) = seq(1, x, 10)")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
        println!("{:?}", tokens);
        let output = Parser::new(tokens)
            .parse()
            .expect("This should be parseable syntax");
        assert_eq!(format!("{}", output), "func(0)");
    }

    // TODO(Hawo): Need More Test Cases for Expressions. These nested brackets are tricky
    // TODO(Hawo): I may need to work on selection of neighboring scopes, but for now it's fine
    // TODO(Hawo): Write more Test Cases for Functions
}
