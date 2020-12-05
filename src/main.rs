#![feature(try_trait)]

use std::fs::File;
use std::io::prelude::*;

mod ast;

mod tokenizer;
use tokenizer::Tokenizer;

fn main() {
    tokenize("./main.boxy");

    println!("");

    build_ast("./main.boxy").unwrap();
}

fn tokenize(root_file: &str) {
    let mut file = File::open(root_file).unwrap();
    let mut string = String::new();
    file.read_to_string(&mut string).unwrap();

    let tokenizer = Tokenizer::new(string.chars());
    let tokens: Vec<_> = tokenizer.map(Result::unwrap).collect();

    for token in &tokens {
        token.print();
    }
}

fn build_ast(root_file: &str) -> Result<(), tokenizer::ParseError> {
    let mut file = File::open(root_file).unwrap();
    let mut string = String::new();
    file.read_to_string(&mut string).unwrap();

    let tokenizer = Tokenizer::new(string.chars())
        .collect::<Result<Vec<_>, tokenizer::ParseError>>()?
        .into_iter()
        .peekable();

    let _ = ast::build_ast(tokenizer).unwrap();

    Ok(())
}
