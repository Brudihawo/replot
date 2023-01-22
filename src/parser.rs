use crate::ast::{
    Assignment, AssignmentError, AssignmentLHS, AssignmentRHS, BinaryExpression, Evaluatable,
    FunctionNode, Literal, Name, Seq, SyntaxError, UnaryExpression,
};

use crate::tokenize::{KeyWord, OperatorType, Token, TokenType};
use std::cmp::Ordering;

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

#[derive(Debug, Clone)]
pub enum Argument {
    Name(Name),
    Lit(Literal),
    Eval(Evaluatable),
}

impl std::fmt::Display for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Argument::Lit(lit) => write!(f, "{}", lit),
            Argument::Eval(ev) => write!(f, "{}", ev),
            Argument::Name(n) => write!(f, "{}", n),
        }
    }
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
    Eval(Evaluatable),
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

    fn parse_evaluatable(&self, range: &'a [TokenInfo<'a>]) -> Result<Evaluatable, SyntaxError> {
        assert!(!range.is_empty());

        let range = Self::strip_parens(range)?;

        match range.len() {
            1 => {
                // Scope contains one token
                // only valid for Literals and Names
                let token = &range[0].token;
                match token.token_type {
                    TokenType::Name(ref name) => {
                        return Ok(Name::new(name.clone(), token.loc).into());
                    }
                    TokenType::Literal(value) => {
                        return Ok(Literal::new(value, token.loc).into());
                    }
                    _ => return Err(SyntaxError::CannotEvaluate(token.loc)),
                }
            }
            2 => {
                // scope contains 2 elements
                // this needs to be a unary expression
                return Ok(UnaryExpression::try_from(
                    range.first().unwrap().token,
                    range.last().unwrap().token,
                )
                .map_err(|_| {
                    SyntaxError::NonEvaluatableScope(
                        range.first().unwrap().token.loc,
                        range.last().unwrap().token.loc,
                    )
                })?
                .into());
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
                    let lhs: Evaluatable = match range[0].token.token_type {
                        TokenType::Name(ref name) => {
                            Name::new(name.clone(), range[0].token.loc).into()
                        }
                        TokenType::Literal(value) => Literal::new(value, range[0].token.loc).into(),
                        _ => return Err(SyntaxError::InvalidBinaryOperand(range[0].token.loc)),
                    };

                    let rhs: Evaluatable = match range[2].token.token_type {
                        TokenType::Name(ref name) => {
                            Name::new(name.clone(), range[2].token.loc).into()
                        }
                        TokenType::Literal(value) => Literal::new(value, range[2].token.loc).into(),
                        _ => return Err(SyntaxError::InvalidBinaryOperand(range[2].token.loc)),
                    };

                    Ok(BinaryExpression::new(op, lhs, rhs)
                        .expect("Equals are handled elsewhere")
                        .into())
                } else {
                    Err(SyntaxError::InvalidBinaryOperator(range[1].token.loc))
                }
            }
            _ => {
                // If it is a function parse the function
                if let Some(kw_func) = self.try_parse_keyword(&range)? {
                    match kw_func {
                        Command::Sequence(seq) => return Ok(seq.into()),
                    }
                    // TODO(Hawo): How do i handle function head / function call differently?
                }
                if let Some(func) = self.try_parse_function(&range)? {
                    return Ok(func.into());
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
                let lhs = self.parse_evaluatable(&range[l_first..l_end])?;

                let r_first = operator.index - range[0].index + 1;
                let mut r_end = r_first + 1;
                while (r_end < range.len()) && (range[r_end].depth >= depth) {
                    r_end += 1;
                }
                let rhs = self.parse_evaluatable(&range[r_first..r_end])?;

                Ok(BinaryExpression::new(*operator.op_type, lhs, rhs)
                    .map_err(|_| SyntaxError::MisplacedEquals(operator.token.loc))?
                    .into())
            }
        }
    }

    fn try_parse_keyword(
        &self,
        range: &'a [TokenInfo<'a>],
    ) -> Result<Option<Command>, SyntaxError> {
        // TODO(Hawo): Change handling of keyword parsing
        // Try to parse a function
        if range.len() < 3 {
            return Ok(None);
        }

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
                let mut var_list = match self.try_parse_params(Self::strip_parens(&range[start..])?)
                {
                    Ok(vars) => vars,
                    Err(_) => return Ok(None),
                };

                if range[start].token.token_type == TokenType::Lparen {
                    start += 1;
                }

                match var_list.len() {
                    0..=2 => {
                        // we know var_list has at least length 1, otherwise
                        // self.try_parse_var_list throws an error
                        Err(SyntaxError::NotEnoughArguments(range[start].token.loc))
                    }
                    3 => {
                        let n = var_list.pop().unwrap();
                        let end = var_list.pop().unwrap();
                        let start = var_list.pop().unwrap();
                        if let Argument::Lit(num_iter) = n {
                            Ok(Some(Command::Sequence(Seq::new(
                                match start {
                                    Argument::Name(name) => name.into(),
                                    Argument::Eval(ev) => ev,
                                    Argument::Lit(lit) => lit.into(),
                                },
                                match end {
                                    Argument::Name(name) => name.into(),
                                    Argument::Eval(ev) => ev,
                                    Argument::Lit(lit) => lit.into(),
                                },
                                num_iter.value as usize,
                            ))))
                        } else {
                            Err(SyntaxError::InvalidArgument(n))
                        }
                    }
                    _ => Err(SyntaxError::TooManyArguments(range[0].token.loc)),
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
                TokenType::Name(name) => Ok(AssignmentRHS::Eval(
                    Name::new(name.clone(), tokens[0].token.loc).into(),
                )),
                TokenType::Literal(val) => Ok(AssignmentRHS::Single(*val)),
                _ => Err(SyntaxError::InvalidAssignmentRHS(tokens[0].token.loc)),
            }
        } else {
            if let Some(cmd) = self.try_parse_keyword(tokens)? {
                match cmd {
                    Command::Sequence(seq) => Ok(AssignmentRHS::Eval(seq.into())),
                }
            } else {
                if let Some(rhs) = self.try_parse_function(tokens)? {
                    Ok(AssignmentRHS::Eval(rhs.into()))
                } else {
                    Ok(AssignmentRHS::Eval(self.parse_evaluatable(tokens)?))
                }
            }
        }
    }

    fn try_parse_arg(&self, range: &[TokenInfo]) -> Result<Argument, SyntaxError> {
        if range.len() == 1 {
            return match range[0] {
                ti_with![TokenType::Name(ref name)] => {
                    Ok(Argument::Name(Name::new(name.clone(), range[0].token.loc)))
                }
                ti_with![TokenType::Literal(ref val)] => {
                    Ok(Argument::Lit(Literal::new(*val, range[0].token.loc).into()))
                }
                _ => Err(SyntaxError::UnexpectedToken(range[0].token.clone())),
            };
        }

        // try parsing an expression
        Ok(Argument::Eval(self.parse_evaluatable(range)?))
        // TODO(Hawo): For now, this only supports Literals and Names.
        // TODO(Hawo): This needs to support expressions and Functions in the future
    }

    /// Parse a variable list from a list of tokens without surrounding parens
    /// Input: a, b, c
    /// Output: Vec<FunctionParam>(a, b, c)
    fn try_parse_params(&self, range: &[TokenInfo]) -> Result<Vec<Argument>, SyntaxError> {
        let mut var_list = Vec::<Argument>::new();

        let mut start = 0;
        let mut end = 0;
        while let Some(ti) = range.get(end) {
            if !(ti.token.token_type == TokenType::Comma) {
                end += 1;
                continue;
            }

            // We have found a comma
            var_list.push(self.try_parse_arg(&range[start..end])?);
            start = end + 1;
            end = start;
        }
        if start == end {
            return Err(SyntaxError::UnexpectedToken(range[end - 1].token.clone()));
        }
        var_list.push(self.try_parse_arg(&range[start..end])?);

        Ok(var_list)
    }

    fn try_parse_function(
        &self,
        tokens: &'a [TokenInfo],
    ) -> Result<Option<FunctionNode>, SyntaxError> {
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
                return Ok(Some(FunctionNode::new(
                    func_name,
                    tokens[0].token.loc,
                    var_list,
                )));
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
                    Ok(AssignmentLHS::Function(func))
                    // Cannot assign to a function evaluation -> this is an rvalue
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
                    Ok(ParseResult::Eval(self.parse_evaluatable(&self.token_info)?))
                }
            }
            1 => {
                let equals = self
                    .operator_info
                    .iter()
                    .filter(|x| *x.op_type == OperatorType::Equals)
                    .next()
                    .unwrap();

                Ok(ParseResult::Definition(
                    Assignment::new(
                        self.parse_assignment_lhs(&self.token_info[..equals.index])?,
                        self.parse_assignment_rhs(&self.token_info[equals.index + 1..])?,
                        equals.token.loc,
                    )
                    .map_err(|x| match x {
                        AssignmentError::InvalidFunctionParameter(_) => todo!(),
                        AssignmentError::ExpectedFunc(_) => todo!(),
                        AssignmentError::ExpectedName(_) => todo!(),
                        AssignmentError::InvalidName(_) => todo!(),
                        AssignmentError::CannotDepend(_) => todo!(),
                    })?,
                ))
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
