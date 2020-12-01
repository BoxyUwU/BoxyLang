use std::fs::File;
use std::io::prelude::*;

mod tokenizer;
use tokenizer::Tokenizer;

fn main() {
    tokenize("./main.boxy").unwrap();
}

fn tokenize(root_file: &str) -> Result<(), std::io::Error> {
    let mut file = File::open(root_file)?;
    let mut string = String::new();
    file.read_to_string(&mut string).unwrap();

    let tokenizer = Tokenizer::new(string.chars());
    let tokens: Vec<_> = tokenizer.map(Result::unwrap).collect();

    for token in &tokens {
        token.print();
    }

    Ok(())
}
