use std::{
    collections::VecDeque,
    fmt::{self},
    io::{self, BufReader, Read},
};

use console::style;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LexError {
    #[error("failed to read")]
    ReadError(#[from] io::Error),
}

macro_rules! test {
    ($e:expr; $name:path) => {
        match $e {
            Ok(v) => v,
            Err(err) => return Some(Err($name(err))),
        }
    };
    ($e:expr) => {
        match $e {
            Ok(v) => v,
            Err(err) => return Some(Err(err)),
        }
    };
}

#[derive(Debug)]
pub enum Token {
    Comment { contents: String },
    Identifier(String),
    NewLine,
    Space,
    // TODO: keyword token or just like this?
    Func,
    Return,
    Let,
    If,
    Else,
    While,
    For,
    String(String),
    // NOTE: we keep this as a string, because quite frankly, we don't really care about the value
    // right now
    Number(String),
    Unknown(char),
    RParen,
    LParen,
    RBracket,
    LBracket,
    Semicolon,
    Add,
    Sub,
    AddAssign,
    SubAssign,
    Equal,
    Comma,
    GT,
    LT,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Comment { contents } => write!(f, "{}", style(format!("//{}", contents)).dim()),
            Token::Identifier(ident) => write!(f, "{}", style(ident).blue()),
            Token::NewLine => writeln!(f),
            Token::Space => write!(f, " "),
            Token::Func => write!(f, "{}", style("func").red()),
            Token::Return => write!(f, "{}", style("return").red()),
            Token::Let => write!(f, "{}", style("let").red()),
            Token::If => write!(f, "{}", style("if").red()),
            Token::Else => write!(f, "{}", style("else").red()),
            Token::While => write!(f, "{}", style("while").red()),
            Token::For => write!(f, "{}", style("for").red()),
            Token::String(s) => write!(f, "{}", style(format!("\"{s}\"")).green()),
            Token::Number(n) => write!(f, "{}", style(n).magenta()),
            Token::Unknown(c) => write!(f, "{}", style(c).dim()),
            Token::RParen => write!(f, "{}", style("(").cyan()),
            Token::LParen => write!(f, "{}", style(")").cyan()),
            Token::RBracket => write!(f, "{}", style("{").cyan()),
            Token::LBracket => write!(f, "{}", style("}").cyan()),
            Token::Semicolon => write!(f, "{}", style(";").cyan()),
            Token::Add => write!(f, "{}", style("+").cyan()),
            Token::Sub => write!(f, "{}", style("-").cyan()),
            Token::AddAssign => write!(f, "{}", style("+=").cyan()),
            Token::SubAssign => write!(f, "{}", style("-=").cyan()),
            Token::Equal => write!(f, "{}", style("=").cyan()),
            Token::Comma => write!(f, "{}", style(",").cyan()),
            Token::GT => write!(f, "{}", style(">").cyan()),
            Token::LT => write!(f, "{}", style("<").cyan()),
        }
    }
}

struct FileReader<T: Read> {
    reader: BufReader<T>,
}

// Idea taken from https://github.com/timothee-haudebourg/utf8-decode
impl<T: Read> FileReader<T> {
    fn read_next_byte(&mut self) -> Option<Result<u8, io::Error>> {
        let mut buf = [0u8; 1];
        let n = test!(self.reader.read(&mut buf));

        if n != 1 {
            return None;
        }

        Some(Ok(buf[0]))
    }

    fn next_byte(&mut self) -> Option<Result<u32, io::Error>> {
        let c = test!(self.read_next_byte()?);

        if c & 0xC0 == 0x80 {
            Some(Ok((c & 0x3F) as u32))
        } else {
            Some(Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "invalid UTF-8 sequence.",
            )))
        }
    }

    fn decode_next(&mut self, a: u32) -> Option<Result<u32, io::Error>> {
        if a & 0x80 == 0x00 {
            Some(Ok(a))
        } else if a & 0xE0 == 0xC0 {
            let b = test!(self.next_byte()?);

            Some(Ok((a & 0x1F) << 6 | b))
        } else if a & 0xF0 == 0xE0 {
            let b = test!(self.next_byte()?);
            let c = test!(self.next_byte()?);

            Some(Ok((a & 0x0F) << 12 | b << 6 | c))
        } else if a & 0xF8 == 0xF0 {
            let b = test!(self.next_byte()?);
            let c = test!(self.next_byte()?);
            let d = test!(self.next_byte()?);

            Some(Ok((a & 0x07) << 18 | b << 12 | c << 6 | d))
        } else {
            Some(Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "invalid UTF-8 sequence.",
            )))
        }
    }
}

impl<T: Read> Iterator for FileReader<T> {
    type Item = Result<char, io::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let a = test!(self.read_next_byte()?) as u32;

        let c = test!(self.decode_next(a)?);

        Some(match char::try_from(c) {
            Ok(c) => Ok(c),
            Err(_) => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "invalid UTF-8 sequence",
            )),
        })
    }
}

const LEXER_BUFFER_CAPACITY: usize = 16;

pub struct Lexer<T: Read> {
    reader: FileReader<T>,
    buffer: VecDeque<char>,
}

impl<T: Read> Lexer<T> {
    pub fn new(reader: T) -> Self {
        Lexer {
            reader: FileReader {
                reader: BufReader::new(reader),
            },
            buffer: VecDeque::with_capacity(LEXER_BUFFER_CAPACITY),
        }
    }

    fn next(&mut self) -> Option<Result<char, io::Error>> {
        if let Some(c) = self.buffer.pop_front() {
            return Some(Ok(c));
        }

        self.reader.next()
    }

    fn peek(&mut self, offset: usize) -> Option<Result<char, io::Error>> {
        let amount = offset.saturating_sub(self.buffer.len());
        for _ in 0..amount {
            self.buffer.push_back(test!(self.reader.next()?));
        }

        self.buffer.get(offset - 1).map(|c| Ok(*c))
    }

    fn has_next(&mut self, value: &str) -> bool {
        for (i, c) in value.chars().enumerate() {
            let p = self.peek(i + 1);
            match (c, p) {
                (c, Some(Ok(p))) if c == p => continue,
                _ => return false,
            }
        }

        true
    }

    fn consume_until(&mut self, value: char) -> Result<String, LexError> {
        self.consume_while(|c| c != value)
    }

    fn consume_while(&mut self, func: impl Fn(char) -> bool) -> Result<String, LexError> {
        let mut acc = Vec::new();
        loop {
            match self.peek(1) {
                Some(Ok(c)) if func(c) => match self.next() {
                    Some(Ok(c)) => acc.push(c),
                    Some(Err(err)) => return Err(LexError::from(err)),
                    None => unreachable!(),
                },
                Some(Err(err)) => return Err(LexError::from(err)),
                _ => break,
            }
        }

        Ok(acc.into_iter().collect())
    }

    fn consume_count(&mut self, amount: usize) -> Result<(), LexError> {
        for _ in 0..amount {
            if let Some(Err(err)) = self.next() {
                return Err(LexError::from(err));
            }
        }

        Ok(())
    }

    fn match_and_consume(&mut self, current: char, needle: &str) -> Result<bool, LexError> {
        for (i, c) in needle.chars().enumerate() {
            let p = match i {
                0 => Some(Ok(current)),
                i => self.peek(i),
            };

            match (c, p) {
                (c, Some(Ok(p))) if c == p => continue,
                _ => return Ok(false),
            }
        }

        self.consume_count(needle.len() - 1)?;

        Ok(true)
    }
}

impl<T: Read + 'static> Iterator for Lexer<T> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        match test!(self.next()?; LexError::from) {
            '/' => {
                if self.has_next("/") {
                    test!(self.next()?; LexError::from);

                    let contents = self.consume_until('\n');
                    match contents {
                        Ok(contents) => Some(Ok(Token::Comment { contents })),
                        Err(err) => Some(Err(err)),
                    }
                } else {
                    todo!();
                }
            }
            '\n' => Some(Ok(Token::NewLine)),
            ' ' => Some(Ok(Token::Space)),
            '(' => Some(Ok(Token::RParen)),
            ')' => Some(Ok(Token::LParen)),
            '{' => Some(Ok(Token::RBracket)),
            '}' => Some(Ok(Token::LBracket)),
            ';' => Some(Ok(Token::Semicolon)),
            ',' => Some(Ok(Token::Comma)),
            '>' => {
                if self.has_next("=") {
                    todo!()
                } else {
                    Some(Ok(Token::GT))
                }
            }
            '<' => {
                if self.has_next("=") {
                    todo!()
                } else {
                    Some(Ok(Token::LT))
                }
            }
            '=' => {
                if self.has_next("=") {
                    todo!()
                } else {
                    Some(Ok(Token::Equal))
                }
            }
            '+' => {
                if test!(self.match_and_consume('+', "+=")) {
                    Some(Ok(Token::AddAssign))
                } else {
                    Some(Ok(Token::Add))
                }
            }
            '-' => {
                if test!(self.match_and_consume('-', "-=")) {
                    Some(Ok(Token::SubAssign))
                } else {
                    Some(Ok(Token::Sub))
                }
            }
            '"' => {
                let inside = match self.consume_while(|c| c != '"') {
                    Ok(inside) => inside,
                    Err(err) => return Some(Err(err)),
                };

                test!(self.next()?; LexError::from);

                Some(Ok(Token::String(inside)))
            }
            c if test!(self.match_and_consume(c, "func")) => Some(Ok(Token::Func)),
            c if test!(self.match_and_consume(c, "return")) => Some(Ok(Token::Return)),
            c if test!(self.match_and_consume(c, "let")) => Some(Ok(Token::Let)),
            c if test!(self.match_and_consume(c, "if")) => Some(Ok(Token::If)),
            c if test!(self.match_and_consume(c, "else")) => Some(Ok(Token::Else)),
            c if test!(self.match_and_consume(c, "while")) => Some(Ok(Token::While)),
            c if test!(self.match_and_consume(c, "for")) => Some(Ok(Token::For)),
            c if c.is_alphabetic() => {
                let rest = match self.consume_while(|c| c.is_alphanumeric()) {
                    Ok(rest) => rest,
                    Err(err) => return Some(Err(err)),
                };

                Some(Ok(Token::Identifier(format!("{c}{rest}"))))
            }
            c if c.is_numeric() => {
                let rest = match self.consume_while(|c| c.is_numeric()) {
                    Ok(rest) => rest,
                    Err(err) => return Some(Err(err)),
                };

                Some(Ok(Token::Number(format!("{c}{rest}"))))
            }
            c => Some(Ok(Token::Unknown(c))),
        }
    }
}
