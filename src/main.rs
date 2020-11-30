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
enum Keyword {
    Fn,
    Struct,
    If,
    Else,
    For,
    In,
    While,
    Loop,
    Continue,
    Break,
    Return,
    Let,
}

impl Keyword {
    fn from_str(keyword: &str) -> Option<Self> {
        Some(match keyword {
            "fn" => Self::Fn,
            "struct" => Self::Struct,
            "if" => Self::If,
            "else" => Self::Else,
            "for" => Self::For,
            "in" => Self::In,
            "while" => Self::While,
            "loop" => Self::Loop,
            "continue" => Self::Continue,
            "break" => Self::Break,
            "return" => Self::Return,
            "let" => Self::Let,
            _ => return None,
        })
    }
}

#[derive(Debug)]
enum Operator {
    Not,
    NotEquals,

    Assign,
    Equals,

    Add,
    Sub,
    Mul,
    Div,

    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
}

#[derive(Debug)]
enum SpecialCharacter {
    Dot,

    LBrace,
    RBrace,
    LCurly,
    RCurly,
    LSquare,
    RSquare,

    Semicolon,
    Colon,
}

impl SpecialCharacter {
    fn from_char(operator: char) -> Option<Self> {
        Some(match operator {
            '.' => Self::Dot,

            '(' => Self::LBrace,
            ')' => Self::RBrace,
            '{' => Self::LCurly,
            '}' => Self::RCurly,
            '[' => Self::LSquare,
            ']' => Self::RSquare,

            ';' => Self::Semicolon,
            ':' => Self::Colon,

            _ => return None,
        })
    }
}

fn is_special_or_operator_char(character: char) -> bool {
    let is_special = || {
        matches!(
            character,
            '.' | '{' | '}' | '[' | ']' | '<' | '>' | ';' | ':'
        )
    };
    let is_operator = || matches!(character, '!' | '=' | '+' | '-' | '*' | '/');

    is_operator() || is_special()
}

#[derive(Debug)]
enum Token {
    Operator(Operator),
    Integer(String),
    Float(String),
    Str(String),
    Ident(String),
    Keyword(Keyword),
    Special(SpecialCharacter),
}

impl Token {
    fn print(&self) {
        match self {
            Self::Operator(op) => println!("operator: {:?}", op),
            Self::Integer(int) => println!("int: {}", int),
            Self::Float(num) => println!("float: {}", num),
            Self::Str(string) => println!("string: \"{}\"", string),
            Self::Special(SpecialCharacter::Semicolon) => println!("semicolon \n"),
            Self::Special(special) => println!("special: {:?}", special),
            Self::Keyword(keyword) => println!("{:?}", keyword),
            Self::Ident(string) => println!("ident: {}", string),
        }
    }

    /// Tries to parse the string as a keyword and if it fails returns a Token::Ident() with the provided string
    fn from_ident_or_keyword(string: &String) -> Self {
        if let Some(keyword) = Keyword::from_str(&string) {
            return Self::Keyword(keyword);
        } else {
            return Self::Ident(string.clone());
        }
    }
}

#[derive(Debug)]
enum ParseError {
    UnmatchedQuote,
    MalformedFloat,
    MalformedInt,
    UnexpectedDot,
    UnexpectedEOF,
    NumParseErr(std::num::ParseIntError),
    InvalidASCII,
    InvalidUnicode,
}

struct Tokenizer<'a> {
    source: std::iter::Peekable<Chars<'a>>,

    token_buffer: Vec<Token>,
}

impl<'a> Tokenizer<'a> {
    fn new(source: Chars<'a>) -> Self {
        Self {
            source: source.peekable(),
            token_buffer: Vec::new(),
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token, ParseError>;

    fn next(&mut self) -> Option<Result<Token, ParseError>> {
        if self.token_buffer.len() > 0 {
            return Some(Ok(self.token_buffer.remove(0)));
        }

        // Get the next non-whitespace char
        let c = loop {
            let c = self.source.next()?;
            if !c.is_whitespace() {
                break c;
            }
        };

        // Number
        if c.is_numeric() {
            let mut token = String::new();
            token.push(c);

            loop {
                match self.source.peek() {
                    Some(c) if c.is_numeric() => token.push(self.source.next().unwrap()),
                    _ => break,
                }
            }

            match self.source.peek() {
                Some('.') => {
                    self.source.next().unwrap();
                    token.push('.');
                }
                Some(c) if c.is_whitespace() || is_special_or_operator_char(*c) => {
                    return Some(Ok(Token::Integer(token)));
                }
                Some(_) => return Some(Err(ParseError::MalformedInt)),
                None => return Some(Ok(Token::Integer(token))),
            }

            // Number is a float
            loop {
                match self.source.peek() {
                    Some('.') => return Some(Err(ParseError::MalformedFloat)),
                    Some(c) if c.is_numeric() => token.push(self.source.next().unwrap()),
                    Some(c) if c.is_whitespace() || is_special_or_operator_char(*c) => {
                        return Some(Ok(Token::Float(token)));
                    }
                    Some(_) => return Some(Err(ParseError::MalformedFloat)),
                    None => return Some(Ok(Token::Float(token))),
                }
            }
        }
        // Float with no leading number
        else if c == '.' {
            let mut token = String::new();
            token.push(c);

            loop {
                match self.source.peek() {
                    Some('.') => return Some(Err(ParseError::MalformedFloat)),
                    Some(c) if c.is_numeric() => token.push(self.source.next().unwrap()),
                    Some(c) if c.is_whitespace() || is_special_or_operator_char(*c) => {
                        return Some(Ok(Token::Float(token)));
                    }
                    Some(_) => return Some(Err(ParseError::MalformedFloat)),
                    None => return Some(Ok(Token::Float(token))),
                }
            }
        } else if c == '=' {
            if matches!(self.source.peek(), Some('=')) {
                self.source.next().unwrap();
                return Some(Ok(Token::Operator(Operator::Equals)));
            }
            return Some(Ok(Token::Operator(Operator::Assign)));
        } else if c == '!' {
            if matches!(self.source.peek(), Some('=')) {
                self.source.next().unwrap();
                return Some(Ok(Token::Operator(Operator::NotEquals)));
            }
            return Some(Ok(Token::Operator(Operator::Not)));
        } else if c == '+' {
            if matches!(self.source.peek(), Some('=')) {
                self.source.next().unwrap();
                return Some(Ok(Token::Operator(Operator::AddAssign)));
            }
            return Some(Ok(Token::Operator(Operator::Add)));
        } else if c == '-' {
            if matches!(self.source.peek(), Some('=')) {
                self.source.next().unwrap();
                return Some(Ok(Token::Operator(Operator::SubAssign)));
            }
            return Some(Ok(Token::Operator(Operator::Sub)));
        } else if c == '*' {
            if matches!(self.source.peek(), Some('=')) {
                self.source.next().unwrap();
                return Some(Ok(Token::Operator(Operator::MulAssign)));
            }
            return Some(Ok(Token::Operator(Operator::Mul)));
        } else if c == '/' {
            if matches!(self.source.peek(), Some('=')) {
                self.source.next().unwrap();
                return Some(Ok(Token::Operator(Operator::DivAssign)));
            }
            return Some(Ok(Token::Operator(Operator::Div)));
        }
        // Special Chars
        else if let Some(special_char) = SpecialCharacter::from_char(c) {
            if matches!(special_char, SpecialCharacter::Dot) {
                return Some(Err(ParseError::UnexpectedDot));
            }
            return Some(Ok(Token::Special(special_char)));
        }
        // Strings
        else if c == '"' {
            let mut token: String = "".into();

            while let Some(c) = self.source.next() {
                match c {
                    '\\' => match self.source.next() {
                        Some('\n') => {
                            while let Some(c) = self.source.peek() {
                                if c.is_whitespace() {
                                    self.source.next();
                                } else {
                                    break;
                                }
                            }
                        }

                        Some('n') => token.push('\n'),
                        Some('t') => token.push('\t'),
                        Some('r') => token.push('\r'),
                        Some('0') => token.push('\0'),

                        Some('x') => {
                            // ASCII
                            let mut hex: String = "".into();

                            match self.source.next() {
                                Some('{') => (),
                                Some(_) => return Some(Err(ParseError::InvalidASCII)),
                                None => return Some(Err(ParseError::UnexpectedEOF)),
                            }

                            for _ in 0..2 {
                                let next = self.source.next();
                                if let None = next {
                                    return Some(Err(ParseError::UnexpectedEOF));
                                }
                                hex.push(next.unwrap());
                            }

                            match self.source.next() {
                                Some('}') => (),
                                Some(_) => return Some(Err(ParseError::InvalidASCII)),
                                None => return Some(Err(ParseError::UnexpectedEOF)),
                            }

                            let ascii_char: char = match u8::from_str_radix(&hex, 16) {
                                Ok(num) if num <= 0x7F => num as char,
                                Ok(_) => return Some(Err(ParseError::InvalidASCII)),
                                Err(e) => return Some(Err(ParseError::NumParseErr(e))),
                            };

                            token.push(ascii_char);
                        }
                        Some('u') => {
                            // UNICODE
                            let mut hex: String = "".into();

                            match self.source.next() {
                                Some('{') => (),
                                Some(_) => return Some(Err(ParseError::InvalidUnicode)),
                                None => return Some(Err(ParseError::UnexpectedEOF)),
                            }

                            for _ in 0..7 {
                                match self.source.next() {
                                    Some('}') => break,
                                    Some(c) => hex.push(c),
                                    None => return Some(Err(ParseError::UnexpectedEOF)),
                                }
                            }

                            let unicode_char = match usize::from_str_radix(&hex, 16) {
                                Ok(num) => match std::char::from_u32(num as u32) {
                                    Some(c) => c,
                                    None => return Some(Err(ParseError::InvalidUnicode)),
                                },
                                Err(e) => return Some(Err(ParseError::NumParseErr(e))),
                            };

                            token.push(unicode_char);
                        }

                        Some(c) => token.push(c),
                        None => return Some(Err(ParseError::UnmatchedQuote)),
                    },
                    '"' => return Some(Ok(Token::Str(token))),
                    _ => token.push(c),
                }
            }
            return Some(Err(ParseError::UnmatchedQuote));
        }
        // Idents
        else if c.is_alphabetic() {
            let mut token = String::new();
            token.push(c);

            loop {
                // Read first ident
                loop {
                    match self.source.peek() {
                        Some(c) if c.is_alphanumeric() => token.push(self.source.next().unwrap()),
                        Some(_) => {
                            self.token_buffer.push(Token::from_ident_or_keyword(&token));
                            token.clear();
                            break;
                        }
                        None => {
                            self.token_buffer.push(Token::from_ident_or_keyword(&token));
                            return Some(Ok(self.token_buffer.remove(0)));
                        }
                    }
                }

                // Skip whitespace
                loop {
                    match self.source.peek() {
                        Some(c) if c.is_whitespace() => {
                            self.source.next().unwrap();
                        }
                        Some(_) => break,
                        None => return Some(Ok(self.token_buffer.remove(0))),
                    }
                }

                // Return if no DOT operator
                match self.source.peek() {
                    Some('.') => {
                        self.source.next().unwrap();
                        self.token_buffer
                            .push(Token::Special(SpecialCharacter::Dot));

                        match self.source.peek() {
                            Some(c) if c.is_alphanumeric() => continue,
                            Some(_) => return Some(Err(ParseError::MalformedFloat)),
                            None => return Some(Err(ParseError::UnexpectedEOF)),
                        }
                    }
                    _ => return Some(Ok(self.token_buffer.remove(0))),
                }
            }
        }

        unreachable!("Could not parse char: {}", c);
    }
}
