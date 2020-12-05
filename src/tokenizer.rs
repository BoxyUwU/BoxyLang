use std::str::Chars;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Keyword {
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
/// These tokens are not disambiguated between the unary and binary operators with the same token
/// E.g. the '-' in 10 - 10 and -10 will both tokenize as OpToken::Minus
pub enum OpToken {
    Excl,
    ExclEqual,

    Equal,
    EqualEqual,

    Plus,
    Minus,
    Star,
    FwdSlash,

    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,

    LessThan,
    GreaterThan,

    LessThanEqualTo,
    GreaterThanEqualTo,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum SpecialToken {
    Dot,

    LParen,
    RParen,
    LCurly,
    RCurly,
    LSquare,
    RSquare,

    Semicolon,
    Colon,

    Returns,

    Comma,
}

impl SpecialToken {
    fn from_char(operator: char) -> Option<Self> {
        Some(match operator {
            '.' => Self::Dot,

            '(' => Self::LParen,
            ')' => Self::RParen,
            '{' => Self::LCurly,
            '}' => Self::RCurly,
            '[' => Self::LSquare,
            ']' => Self::RSquare,

            ';' => Self::Semicolon,
            ':' => Self::Colon,

            ',' => Self::Comma,

            _ => return None,
        })
    }
}

pub fn is_special_or_operator_char(character: char) -> bool {
    matches!(
        character,
        '.' | '{'
            | '}'
            | '['
            | ']'
            | '('
            | ')'
            | ';'
            | ':'
            | '!'
            | '='
            | '+'
            | '-'
            | '*'
            | '/'
            | '<'
            | '>'
            | ','
    )
}

#[derive(Debug)]
pub enum Literal {
    Integer(String),
    Float(String),
    Str(String),
    Bool(bool),
}

impl Literal {
    fn print(&self) {
        match self {
            Self::Bool(b) => println!("bool: {}", b),
            Self::Integer(int) => println!("int: {}", int),
            Self::Float(num) => println!("float: {}", num),
            Self::Str(string) => println!("string: \"{}\"", string),
        }
    }
}

#[derive(Debug)]
pub enum Token {
    Literal(Literal),
    Operator(OpToken),
    Ident(String),
    Keyword(Keyword),
    Special(SpecialToken),
}

impl Token {
    pub fn print(&self) {
        match self {
            Self::Literal(lit) => lit.print(),
            Self::Operator(op) => println!("operator: {:?}", op),
            Self::Special(SpecialToken::Semicolon) => println!("semicolon \n"),
            Self::Special(special) => println!("special: {:?}", special),
            Self::Keyword(keyword) => println!("{:?}", keyword),
            Self::Ident(string) => println!("ident: {}", string),
        }
    }

    /// Tries to parse the string as a keyword and if it fails returns a Token::Ident() with the provided string
    /// if the string is true or false then it will return Token::Bool
    pub fn from_ident_or_keyword(string: &str) -> Self {
        match string {
            "true" => return Self::Literal(Literal::Bool(true)),
            "false" => return Self::Literal(Literal::Bool(false)),
            _ => (),
        };

        if let Some(keyword) = Keyword::from_str(&string) {
            return Self::Keyword(keyword);
        } else {
            return Self::Ident(string.to_owned());
        }
    }
}

impl Token {
    pub fn assert_keyword(&self, assert_keyword: Keyword) -> Result<(), ()> {
        if let Self::Keyword(keyword) = self {
            if *keyword == assert_keyword {
                return Ok(());
            };
        }

        Err(())
    }

    pub fn assert_ident(self) -> Result<String, ()> {
        if let Self::Ident(string) = self {
            Ok(string)
        } else {
            Err(())
        }
    }

    pub fn assert_special(&self, assert_special: SpecialToken) -> Result<(), ()> {
        if let Self::Special(special) = self {
            if *special == assert_special {
                return Ok(());
            }
        }

        Err(())
    }
}

#[derive(Clone, Debug)]
pub enum ParseError {
    UnmatchedQuote,
    MalformedFloat,
    MalformedInt,
    UnexpectedDot,
    UnexpectedEOF,
    NumParseErr(std::num::ParseIntError),
    InvalidASCII,
    InvalidUnicode,
}

pub struct Tokenizer<'a> {
    source: std::iter::Peekable<Chars<'a>>,

    token_buffer: Vec<Token>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: Chars<'a>) -> Self {
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
                    return Some(Ok(Token::Literal(Literal::Integer(token))));
                }
                Some(_) => return Some(Err(ParseError::MalformedInt)),
                None => return Some(Ok(Token::Literal(Literal::Integer(token)))),
            }

            // Number is a float
            loop {
                match self.source.peek() {
                    Some('.') => return Some(Err(ParseError::MalformedFloat)),
                    Some(c) if c.is_numeric() => token.push(self.source.next().unwrap()),
                    Some(c) if c.is_whitespace() || is_special_or_operator_char(*c) => {
                        return Some(Ok(Token::Literal(Literal::Float(token))));
                    }
                    Some(_) => return Some(Err(ParseError::MalformedFloat)),
                    None => return Some(Ok(Token::Literal(Literal::Float(token)))),
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
                        return Some(Ok(Token::Literal(Literal::Float(token))));
                    }
                    Some(_) => return Some(Err(ParseError::MalformedFloat)),
                    None => return Some(Ok(Token::Literal(Literal::Float(token)))),
                }
            }
        } else if c == '=' {
            if matches!(self.source.peek(), Some('=')) {
                self.source.next().unwrap();
                return Some(Ok(Token::Operator(OpToken::EqualEqual)));
            }
            return Some(Ok(Token::Operator(OpToken::Equal)));
        } else if c == '!' {
            if matches!(self.source.peek(), Some('=')) {
                self.source.next().unwrap();
                return Some(Ok(Token::Operator(OpToken::ExclEqual)));
            }
            return Some(Ok(Token::Operator(OpToken::Excl)));
        } else if c == '+' {
            if matches!(self.source.peek(), Some('=')) {
                self.source.next().unwrap();
                return Some(Ok(Token::Operator(OpToken::AddAssign)));
            }
            return Some(Ok(Token::Operator(OpToken::Plus)));
        } else if c == '-' {
            if matches!(self.source.peek(), Some('=')) {
                self.source.next().unwrap();
                return Some(Ok(Token::Operator(OpToken::SubAssign)));
            } else if matches!(self.source.peek(), Some('>')) {
                self.source.next().unwrap();
                return Some(Ok(Token::Special(SpecialToken::Returns)));
            }
            return Some(Ok(Token::Operator(OpToken::Minus)));
        } else if c == '*' {
            if matches!(self.source.peek(), Some('=')) {
                self.source.next().unwrap();
                return Some(Ok(Token::Operator(OpToken::MulAssign)));
            }
            return Some(Ok(Token::Operator(OpToken::Star)));
        } else if c == '/' {
            if matches!(self.source.peek(), Some('=')) {
                self.source.next().unwrap();
                return Some(Ok(Token::Operator(OpToken::DivAssign)));
            }
            return Some(Ok(Token::Operator(OpToken::FwdSlash)));
        } else if c == '<' {
            if matches!(self.source.peek(), Some('=')) {
                self.source.next().unwrap();
                return Some(Ok(Token::Operator(OpToken::LessThanEqualTo)));
            }
            return Some(Ok(Token::Operator(OpToken::LessThan)));
        } else if c == '>' {
            if matches!(self.source.peek(), Some('=')) {
                self.source.next().unwrap();
                return Some(Ok(Token::Operator(OpToken::GreaterThanEqualTo)));
            }
            return Some(Ok(Token::Operator(OpToken::GreaterThan)));
        }
        // Special Chars
        else if let Some(special_char) = SpecialToken::from_char(c) {
            if matches!(special_char, SpecialToken::Dot) {
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
                    '"' => return Some(Ok(Token::Literal(Literal::Str(token)))),
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
                        self.token_buffer.push(Token::Special(SpecialToken::Dot));

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
