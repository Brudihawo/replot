use std::io::{self, Write};

use crate::ast::{Assignment, EvalASTNode, KnownValues, SyntaxError};
use crate::parser::{ParseResult, Parser};
use crate::tokenize::{Lexer, LexerError};

pub struct Repl {
    knowns: KnownValues,
    cur_line: String,
}

impl Repl {
    pub fn new() -> Self {
        Self {
            knowns: KnownValues::new(),
            cur_line: String::new(),
        }
    }

    fn get_line(&mut self) -> Result<(), io::Error> {
        self.cur_line.clear();
        io::stdin().read_line(&mut self.cur_line)?;
        self.cur_line.pop();
        Ok(())
    }

    fn handle_eval(&self, ev: crate::ast::Evaluatable) {
        match ev.eval(&self.knowns) {
            Ok(eval) => print!("{}", eval),
            Err(err) => println!("Error during Evaluation: {:?}", err),
        }
    }

    fn handle_assignment(&mut self, ass: Assignment) {
        let statement = format!("{}", ass);
        match ass.execute(&mut self.knowns) {
            Ok(res) => match res {
                crate::ast::AssignmentResult::Set(_) => {
                    println!("Assigning {}", statement);
                }
                crate::ast::AssignmentResult::Update(_) => {
                    println!("Updating {}", statement);
                }
            },
            Err(err) => {
                println!("{:?}", err);
            }
        }
    }

    fn handle_lexer_error(&self, err: LexerError) {
        println!(
            "Error during Tokenization of Line '{}': {:?}",
            self.cur_line, err
        );
    }

    fn handle_parser_error(&self, err: SyntaxError) {
        println!(
            "Error during Parsing of Line '{}':\n  {:?}",
            &self.cur_line[..self.cur_line.len() - 1],
            err
        );
    }

    fn handle_cmd(&self, cmd: crate::parser::Command) {
        match cmd {
            crate::parser::Command::Sequence(seq) => {
                println!("i, val");
                // TODO: This is obsolete
                for (i, val) in seq.eval(&self.knowns).iter().enumerate() {
                    println!("{}, {}", i, val);
                }
            }
        }
    }

    pub fn parse_and_process(&mut self) {
        if let Err(err) = self.get_line() {
            println!("Could not get line: {}", err);
            return;
        }
        if self.cur_line.is_empty() {
            return;
        }

        let tokens = match Lexer::new(&self.cur_line).tokenize() {
            Ok(tok) => tok,
            Err(err) => {
                self.handle_lexer_error(err);
                return;
            }
        };

        let parse_result = Parser::new(tokens).parse();
        match parse_result {
            Ok(res) => match res {
                ParseResult::Eval(ev) => self.handle_eval(ev),
                ParseResult::Command(cmd) => self.handle_cmd(cmd),
                ParseResult::Definition(ass) => self.handle_assignment(ass),
            },
            Err(err) => {
                self.handle_parser_error(err);
            }
        }
    }

    pub fn print_prompt() {
        print!("=> ");
        io::stdout().flush().unwrap();
    }
}
