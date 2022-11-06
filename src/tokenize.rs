use phf::phf_map;
use std::iter::{Peekable};
use std::str::Chars;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    InvalidLiteralCharacter(char),
}

#[derive(Debug, PartialEq, Clone)]
pub enum KeyWord {
    Plot,
    Table,
    Seq,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Equals,
    Lparen,
    Rparen,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Power,
    Literal(f64),
    Keyword(KeyWord),
    Name(String),
}

struct Lexer<'a> {
    text: &'a str,
}

const SPECIALCHARS: phf::Map<char, TokenType> = phf_map![
    '=' => TokenType::Equals,
    '(' => TokenType::Lparen,
    ')' => TokenType::Rparen,
    '+' => TokenType::Plus,
    '-' => TokenType::Minus,
    '*' => TokenType::Asterisk,
    '/' => TokenType::Slash,
    '^' => TokenType::Power,
];

const KEYWORDS: phf::Map<&'static str, KeyWord> = phf_map![
    "plot" => KeyWord::Plot,
    "table" => KeyWord::Table,
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
    fn new(text: &'a str) -> Self {
        Self { text: text }
    }

    fn parse_number(to_process: &mut Peekable<Chars>) -> Result<f64, ParseError> {
        let mut num: f64 = 0.0;
        let mut digit_after_dot: u32 = 0;
        while let Some(digit_char) = to_process.next_if(|&x| !x.is_whitespace()) {
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
                return Err(ParseError::InvalidLiteralCharacter(digit_char));
            }
        }
        Ok(num)
    }

    fn tokenize(&mut self) -> Result<Vec<TokenType>, ParseError> {
        let mut tokens: Vec<TokenType> = Vec::new();
        let mut to_process = self.text.chars().peekable();
        loop {
            // skip while characters are whitespace
            while let Some(_) = to_process.next_if(|&x| x.is_whitespace()) {}

            if let Some(chr) = to_process.peek() {
                println!("New token starting with {}", chr);
            }

            if let Some(c) = to_process.peek() {
                if let Some(tok) = try_special_to_token(*c) {
                    // Special character
                    tokens.push(tok);
                    to_process.next();
                } else {
                    if c.is_ascii_digit() {
                        tokens.push(TokenType::Literal(Self::parse_number(&mut to_process)?));
                    } else {
                        let mut word = String::new();
                        while let Some(x) = to_process.peek() {
                            if !x.is_whitespace() && !is_special_char(*x) {
                                word.push(*x);
                                to_process.next();
                            } else {
                                break;
                            }
                        }
                        if let Some(keyword) = KEYWORDS.get(&word) {
                            tokens.push(TokenType::Keyword(keyword.clone()));
                        } else {
                            tokens.push(TokenType::Name(word));
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
mod test {
    use super::*;

    // #[macro_use]
    // use static_assertions as sa;

    #[test]
    fn all_types() {
        let expected = vec![
            TokenType::Equals,
            TokenType::Lparen,
            TokenType::Rparen,
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Asterisk,
            TokenType::Slash,
            TokenType::Power,
            TokenType::Name("name".to_string()),
            TokenType::Literal(1.0),
            TokenType::Keyword(KeyWord::Plot),
            TokenType::Keyword(KeyWord::Table),
            TokenType::Keyword(KeyWord::Seq),
        ];

        assert_eq!(
            Lexer::new("= () + - * /  ^ name 1.0 plot table seq")
                .tokenize()
                .expect("this is a test and it should not fail in parsing"),
            expected
        );
    }

    #[test]
    fn minimal_spaces() {
        let expected = vec![
            TokenType::Equals,
            TokenType::Lparen,
            TokenType::Rparen,
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Asterisk,
            TokenType::Slash,
            TokenType::Name("name1".to_string()),
            TokenType::Literal(1.0),
        ];

        assert_eq!(
            Lexer::new("=()+-*/name1 1.0")
                .tokenize()
                .expect("this is a test and it should not fail in parsing"),
            expected
        );
    }

    #[test]
    fn function_call() {
        let expected = vec![
            TokenType::Name("f".to_string()),
            TokenType::Lparen,
            TokenType::Name("x".to_string()),
            TokenType::Rparen,
            TokenType::Equals,
            TokenType::Name("x".to_string()),
            TokenType::Plus,
            TokenType::Literal(1.0),
        ];

        assert_eq!(
            Lexer::new("f(x) = x + 1")
                .tokenize()
                .expect("this is a test and it should not fail in parsing"),
            expected
        );
    }

    #[test]
    fn empty() {
        assert_eq!(
            Lexer::new("")
                .tokenize()
                .expect("this is a test and it should not fail in parsing"),
            vec![]
        );
    }

    #[test]
    fn invalid_literal() {
        assert_eq!(
            Lexer::new("1.0e")
                .tokenize()
                .expect_err("This test should give an error"),
            ParseError::InvalidLiteralCharacter('e')
        );
    }
}
