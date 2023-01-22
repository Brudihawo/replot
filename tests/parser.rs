use replot::parser::*;
use replot::tokenize::Lexer;

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
    assert_eq!(format!("{}", output), "func: (0, 1, 10) -> func(0, 1, 10)");
}

#[test]
fn func_1_var() {
    let tokens = Lexer::new("func(0)")
        .tokenize()
        .expect("this is a test and it should not fail in parsing");
    let output = Parser::new(tokens)
        .parse()
        .expect("This should be parseable syntax");
    assert_eq!(format!("{}", output), "func: (0) -> func(0)");
}

#[test]
fn seq_in_expr() {
    let tokens = Lexer::new("1 + seq(0, 1, 10)")
        .tokenize()
        .expect("this is a test and it should not fail in parsing");
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
    let output = Parser::new(tokens)
        .parse()
        .expect("This should be parseable syntax");
    assert_eq!(format!("{}", output), "x = (1 + seq(0, 1, 10))");
}

#[test]
fn add_2_seqs() {
    let tokens = Lexer::new("seq(1, 10, 10) + seq(0, 1, 10)")
        .tokenize()
        .expect("this is a test and it should not fail in parsing");

    for tok in tokens.iter() {
        println!("{:?}", tok);
    }

    let output = Parser::new(tokens)
        .parse()
        .expect("This should be parseable syntax");
    assert_eq!(format!("{}", output), "(seq(1, 10, 10) + seq(0, 1, 10))");
}

#[test]
fn seq_in_rhs_assignment() {
    let tokens = Lexer::new("f(x) = seq(1, x, 10)")
        .tokenize()
        .expect("this is a test and it should not fail in parsing");

    for tok in tokens.iter() {
        println!("{:?}", tok);
    }

    let output = Parser::new(tokens)
        .parse()
        .expect("This should be parseable syntax");
    assert_eq!(format!("{}", output), "f(x) = seq(1, x, 10)");
}

#[test]
fn seq_in_rhs_function_call() {
    let tokens = Lexer::new("f(seq(0, 1, 10))")
        .tokenize()
        .expect("this is a test and it should not fail in parsing");

    for tok in tokens.iter() {
        println!("{:?}", tok);
    }

    let output = Parser::new(tokens)
        .parse()
        .expect("This should be parseable syntax");
    assert_eq!(format!("{}", output), "f(x) = seq(1, x, 10)");
}


// TODO(Hawo): Need More Test Cases for Expressions. These nested brackets are tricky
// TODO(Hawo): I may need to work on selection of neighboring scopes, but for now it's fine
// TODO(Hawo): Write more Test Cases for Functions
