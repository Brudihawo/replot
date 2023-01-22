use phf::phf_map;
use std::iter::{Enumerate, Peekable};
use std::str::Chars;

#[derive(Debug, PartialEq)]
pub enum LexerError {
    InvalidLiteralCharacter(char, Location),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum KeyWord {
    Plot,
    Seq,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum OperatorType {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Power,
    Equals,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Operator(OperatorType),
    Lparen,
    Rparen,
    Comma,
    Literal(f64),
    Keyword(KeyWord),
    Name(String),
}

impl Eq for TokenType {}

pub type Location = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub loc: Location,
}

impl Token {
    fn new(token_type: TokenType, location: Location) -> Self {
        Token {
            token_type,
            loc: location,
        }
    }
}

pub struct Lexer<'a> {
    text: &'a str,
}

const SPECIALCHARS: phf::Map<char, TokenType> = phf_map![
    '(' => TokenType::Lparen,
    ')' => TokenType::Rparen,
    ',' => TokenType::Comma,
    '=' => TokenType::Operator(OperatorType::Equals),
    '+' => TokenType::Operator(OperatorType::Plus),
    '-' => TokenType::Operator(OperatorType::Minus),
    '*' => TokenType::Operator(OperatorType::Asterisk),
    '/' => TokenType::Operator(OperatorType::Slash),
    '^' => TokenType::Operator(OperatorType::Power),
];

pub const KEYWORDS: phf::Map<&'static str, KeyWord> = phf_map![
    "plot" => KeyWord::Plot,
    "seq" => KeyWord::Seq
];

fn try_special_to_token(c: char) -> Option<TokenType> {
    if let Some(simpletok) = SPECIALCHARS.get(&c) {
        Some(simpletok.clone())
    } else {
        None
    }
}

fn is_special_char(c: char) -> bool {
    if SPECIALCHARS.get(&c).is_some() {
        true
    } else {
        false
    }
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self { text }
    }

    fn parse_number(to_process: &mut Peekable<Enumerate<Chars>>) -> Result<f64, LexerError> {
        let mut num: f64 = 0.0;
        let mut digit_after_dot: u32 = 0;
        while let Some((pos, digit_char)) =
            to_process.next_if(|(_, x)| !x.is_whitespace() && !is_special_char(*x))
        {
            if digit_char.is_ascii_digit() {
                if digit_after_dot == 0 {
                    num *= 10.0;
                    num += digit_char
                        .to_digit(10)
                        .expect("we checked that this is a digit")
                        as f64;
                } else {
                    num += digit_char
                        .to_digit(10)
                        .expect("we checked that this is a digit")
                        as f64
                        * (10.0f64).powi(-(digit_after_dot as i32));
                    digit_after_dot += 1;
                }
            } else if digit_char == '.' {
                digit_after_dot += 1;
            } else {
                return Err(LexerError::InvalidLiteralCharacter(digit_char, pos));
            }
        }
        Ok(num)
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens: Vec<Token> = Vec::new();
        let mut to_process = self.text.chars().enumerate().peekable();
        loop {
            // skip while characters are whitespace
            while let Some(_) = to_process.next_if(|(_, x)| x.is_whitespace()) {}

            if let Some(&(pos, c)) = to_process.peek() {
                if let Some(tt) = try_special_to_token(c) {
                    // Special character
                    tokens.push(Token::new(tt, pos));
                    to_process.next();
                } else {
                    if c.is_ascii_digit() {
                        tokens.push(Token::new(
                            TokenType::Literal(Self::parse_number(&mut to_process)?),
                            pos,
                        ));
                    } else {
                        let mut word = String::new();
                        while let Some(&(_, x)) = to_process.peek() {
                            if !x.is_whitespace() && !is_special_char(x) {
                                word.push(x);
                                to_process.next();
                            } else {
                                break;
                            }
                        }
                        if let Some(keyword) = KEYWORDS.get(&word) {
                            tokens.push(Token::new(TokenType::Keyword(keyword.clone()), pos));
                        } else {
                            tokens.push(Token::new(TokenType::Name(word), pos));
                        }
                    }
                }
            } else {
                break;
            }
        }
        return Ok(tokens);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_unwrap_to_types(to_parse: &str) -> Vec<TokenType> {
        Lexer::new(to_parse)
            .tokenize()
            .expect("this is a test and it should not fail in parsing")
            .iter()
            .map(|x| x.token_type.clone())
            .collect()
    }

    // #[macro_use]
    // use static_assertions as sa;

    #[test]
    fn all_types() {
        let expected = vec![
            TokenType::Operator(OperatorType::Equals),
            TokenType::Lparen,
            TokenType::Rparen,
            TokenType::Operator(OperatorType::Plus),
            TokenType::Operator(OperatorType::Minus),
            TokenType::Operator(OperatorType::Asterisk),
            TokenType::Operator(OperatorType::Slash),
            TokenType::Operator(OperatorType::Power),
            TokenType::Name("name".to_string()),
            TokenType::Literal(1.0),
            TokenType::Keyword(KeyWord::Plot),
            TokenType::Keyword(KeyWord::Seq),
            TokenType::Comma,
        ];

        assert_eq!(
            parse_unwrap_to_types("= () + - * /  ^ name 1.0 plot seq ,"),
            expected
        );
    }

    #[test]
    fn minimal_spaces() {
        let expected = vec![
            TokenType::Operator(OperatorType::Equals),
            TokenType::Lparen,
            TokenType::Rparen,
            TokenType::Operator(OperatorType::Plus),
            TokenType::Operator(OperatorType::Minus),
            TokenType::Operator(OperatorType::Asterisk),
            TokenType::Operator(OperatorType::Slash),
            TokenType::Operator(OperatorType::Power),
            TokenType::Name("name1".to_string()),
            TokenType::Literal(1.0),
            TokenType::Comma,
        ];

        assert_eq!(parse_unwrap_to_types("=()+-*/^name1 1.0,"), expected);
    }

    #[test]
    fn function_call() {
        let expected = vec![
            TokenType::Name("f".to_string()),
            TokenType::Lparen,
            TokenType::Name("x".to_string()),
            TokenType::Rparen,
            TokenType::Operator(OperatorType::Equals),
            TokenType::Name("x".to_string()),
            TokenType::Operator(OperatorType::Plus),
            TokenType::Literal(1.0),
        ];

        assert_eq!(parse_unwrap_to_types("f(x) = x + 1"), expected);
    }

    #[test]
    fn empty() {
        assert_eq!(parse_unwrap_to_types(""), vec![]);
    }

    #[test]
    fn invalid_literal() {
        assert_eq!(
            Lexer::new("1.0e")
                .tokenize()
                .expect_err("This test should give an error"),
            LexerError::InvalidLiteralCharacter('e', 3)
        );
    }

    #[test]
    fn add() {
        assert_eq!(
            parse_unwrap_to_types("a + b"),
            vec![
                TokenType::Name("a".to_string()),
                TokenType::Operator(OperatorType::Plus),
                TokenType::Name("b".to_string()),
            ]
        );
    }

    #[test]
    fn literal_brackets() {
        assert_eq!(
            parse_unwrap_to_types("(1.0)"),
            vec![
                TokenType::Lparen,
                TokenType::Literal(1.0),
                TokenType::Rparen,
            ]
        );
    }

    #[test]
    fn literal_specialchars() {
        let expected = vec![TokenType::Literal(1.0), TokenType::Lparen];
        assert_eq!(parse_unwrap_to_types("1.0("), expected);

        let expected = vec![TokenType::Literal(1.0), TokenType::Rparen];
        assert_eq!(parse_unwrap_to_types("1.0)"), expected);

        let expected = vec![
            TokenType::Literal(1.0),
            TokenType::Operator(OperatorType::Power),
        ];
        assert_eq!(parse_unwrap_to_types("1.0^"), expected);

        let expected = vec![
            TokenType::Literal(1.0),
            TokenType::Operator(OperatorType::Plus),
        ];
        assert_eq!(parse_unwrap_to_types("1.0+"), expected);

        let expected = vec![
            TokenType::Literal(1.0),
            TokenType::Operator(OperatorType::Minus),
        ];
        assert_eq!(parse_unwrap_to_types("1.0-"), expected);

        let expected = vec![
            TokenType::Literal(1.0),
            TokenType::Operator(OperatorType::Asterisk),
        ];
        assert_eq!(parse_unwrap_to_types("1.0*"), expected);

        let expected = vec![
            TokenType::Literal(1.0),
            TokenType::Operator(OperatorType::Slash),
        ];
        assert_eq!(parse_unwrap_to_types("1.0/"), expected);

        let expected = vec![TokenType::Literal(1.0), TokenType::Comma];
        assert_eq!(parse_unwrap_to_types("1.0,"), expected);
    }

    #[test]
    fn seq_in_rhs_function_call() {
        let _ = Lexer::new("f(seq(0, 1, 10))")
            .tokenize()
            .expect("this is a test and it should not fail in parsing");
    }
}
