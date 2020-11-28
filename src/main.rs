use std::io::prelude::*;
use std::{fs::File, str::Chars};

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

#[derive(Debug)]
enum SpecialCharacter {
    Not,
    Dot,
    Equals,

    Plus,
    Minus,
    Multiply,
    Divide,

    LBrace,
    RBrace,
    LCurly,
    RCurly,
    LSquare,
    RSquare,

    Semicolon,
}

impl SpecialCharacter {
    fn from_char(operator: char) -> Option<Self> {
        Some(match operator {
            '!' => Self::Not,
            '.' => Self::Dot,
            '=' => Self::Equals,

            '+' => Self::Plus,
            '-' => Self::Minus,
            '*' => Self::Multiply,
            '/' => Self::Divide,

            '(' => Self::LBrace,
            ')' => Self::RBrace,
            '{' => Self::LCurly,
            '}' => Self::RCurly,
            '[' => Self::LSquare,
            ']' => Self::RSquare,

            ';' => Self::Semicolon,

            _ => return None,
        })
    }
}

#[derive(Debug)]
enum Token {
    Number(String),
    Str(String),
    Ident(String),
    Special(SpecialCharacter),
}

impl Token {
    fn print(&self) {
        match self {
            Self::Number(num) => println!("number: {}", num),
            Self::Str(string) => println!("string: \"{}\"", string),
            Self::Special(SpecialCharacter::Semicolon) => println!("semicolon \n"),
            Self::Special(special) => println!("special: {:?}", special),
            Self::Ident(string) => println!("ident: {}", string),
        }
    }
}

#[derive(Debug)]
enum ParseError {
    UnmatchedQuote,
}

struct Tokenizer<'a> {
    source: std::iter::Peekable<Chars<'a>>,
}

impl<'a> Tokenizer<'a> {
    fn new(source: Chars<'a>) -> Self {
        Self {
            source: source.peekable(),
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token, ParseError>;

    fn next(&mut self) -> Option<Result<Token, ParseError>> {
        let mut token: String = "".into();

        // Get the next non-whitespace char
        let c = loop {
            let c = self.source.next()?;
            if !c.is_whitespace() {
                break c;
            }
        };

        // Numbers
        if c.is_numeric() {
            token = c.into();

            while let Some(&peek_c) = self.source.peek() {
                if peek_c.is_numeric() {
                    token.push(self.source.next().unwrap());
                } else {
                    break;
                }
            }

            return Some(Ok(Token::Number(token)));
        }
        // Special Chars
        else if let Some(special_char) = SpecialCharacter::from_char(c) {
            return Some(Ok(Token::Special(special_char)));
        }
        // Strings
        else if c == '"' {
            while let Some(c) = self.source.next() {
                if c != '"' {
                    token.push(c);
                    continue;
                }
                return Some(Ok(Token::Str(token)));
            }

            return Some(Err(ParseError::UnmatchedQuote));
        }
        // Idents
        else if c.is_alphabetic() {
            token = c.into();

            while let Some(&peeked) = self.source.peek() {
                if peeked.is_alphanumeric() && SpecialCharacter::from_char(peeked).is_none() {
                    token.push(self.source.next().unwrap());
                } else {
                    break;
                }
            }

            return Some(Ok(Token::Ident(token)));
        } else {
            unreachable!("Could not parse char: {}", c);
        }
    }
}
