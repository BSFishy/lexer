use std::{
    collections::VecDeque,
    fmt,
    io::{self, Read},
};

use console::style;
use thiserror::Error;
use utf8_reader::Utf8Reader;

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
    Comment(String),
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
            Token::Comment(contents) => write!(f, "{}", style(format!("//{}", contents)).dim()),
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
            Token::LParen => write!(f, "{}", style("(").cyan()),
            Token::RParen => write!(f, "{}", style(")").cyan()),
            Token::LBracket => write!(f, "{}", style("{").cyan()),
            Token::RBracket => write!(f, "{}", style("}").cyan()),
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

const LEXER_BUFFER_CAPACITY: usize = 16;

pub struct Lexer<T: Read> {
    reader: Utf8Reader<T>,
    pos: usize,
    buffer: VecDeque<char>,
}

impl<T: Read> Lexer<T> {
    pub fn new(reader: T) -> Self {
        Self {
            reader: Utf8Reader::new(reader),
            pos: 0,
            buffer: VecDeque::with_capacity(LEXER_BUFFER_CAPACITY),
        }
    }

    fn next(&mut self) -> Option<Result<char, LexError>> {
        if let Some(c) = self.buffer.get(self.pos) {
            self.pos += 1;
            return Some(Ok(*c));
        }

        let c = test!(self.reader.next()?; LexError::from);
        self.buffer.push_back(c);
        self.pos += 1;

        Some(Ok(c))
    }

    fn reset(&mut self) {
        self.pos = 0;
    }

    fn backtrack(&mut self, amount: usize) {
        self.pos -= amount;
    }

    fn consume(&mut self, amount: usize) {
        for _ in 0..amount {
            let _ = self.buffer.pop_front();
        }

        self.backtrack(amount);
    }

    fn consume_comment(&mut self) -> Option<Result<Option<Token>, LexError>> {
        let mut buf = String::new();
        let c = test!(self.next()?);
        if c != '/' {
            self.reset();
            return Some(Ok(None));
        }

        let c = test!(self.next()?);
        if c != '/' {
            self.reset();
            return Some(Ok(None));
        }

        loop {
            let c = test!(self.next()?);
            if c == '\n' {
                self.backtrack(1);
                break;
            }

            buf.push(c);
        }

        self.consume(buf.len() + 2);
        Some(Ok(Some(Token::Comment(buf))))
    }

    fn consume_text(
        &mut self,
        token: Token,
        text: &str,
    ) -> Option<Result<Option<Token>, LexError>> {
        for c in text.chars() {
            let x = test!(self.next()?);
            if c != x {
                self.reset();
                return Some(Ok(None));
            }
        }

        self.consume(text.len());
        Some(Ok(Some(token)))
    }

    fn consume_identifier(&mut self) -> Option<Result<Option<Token>, LexError>> {
        let mut buf = String::new();
        loop {
            let c = test!(self.next()?);
            match (buf.len(), c) {
                (0, c) if c.is_alphabetic() => buf.push(c),
                (1.., c) if c.is_alphanumeric() => buf.push(c),
                _ => {
                    self.backtrack(1);
                    break;
                }
            }
        }

        if buf.is_empty() {
            Some(Ok(None))
        } else {
            self.consume(buf.len());
            Some(Ok(Some(Token::Identifier(buf))))
        }
    }

    fn consume_number(&mut self) -> Option<Result<Option<Token>, LexError>> {
        let mut buf = String::new();
        loop {
            let c = test!(self.next()?);
            if !c.is_numeric() {
                self.backtrack(1);
                break;
            }

            buf.push(c);
        }

        if buf.is_empty() {
            Some(Ok(None))
        } else {
            self.consume(buf.len());
            Some(Ok(Some(Token::Number(buf))))
        }
    }

    fn consume_string(&mut self) -> Option<Result<Option<Token>, LexError>> {
        let c = test!(self.next()?);
        if c != '"' {
            self.reset();
            return Some(Ok(None));
        }

        let mut buf = String::new();
        let mut len = 0;
        loop {
            let c = test!(self.next()?);
            // NOTE: does not support \"
            if c == '"' {
                break;
            }

            buf.push(c);
            len += 1;
        }

        self.consume(len + 2);
        Some(Ok(Some(Token::String(buf))))
    }
}

macro_rules! lex {
    ($e:expr) => {
        if let Some(token) = test!($e?) {
            return Some(Ok(token));
        }
    };
}

impl<T: Read> Iterator for Lexer<T> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        // language constructs
        lex!(self.consume_comment());

        // special characters
        lex!(self.consume_text(Token::NewLine, "\n"));
        lex!(self.consume_text(Token::NewLine, "\r\n"));
        lex!(self.consume_text(Token::Space, " "));
        lex!(self.consume_text(Token::Comma, ","));
        lex!(self.consume_text(Token::Semicolon, ";"));

        lex!(self.consume_text(Token::LParen, "("));
        lex!(self.consume_text(Token::RParen, ")"));
        lex!(self.consume_text(Token::LBracket, "{"));
        lex!(self.consume_text(Token::RBracket, "}"));

        // operators
        lex!(self.consume_text(Token::AddAssign, "+="));
        lex!(self.consume_text(Token::SubAssign, "-="));

        lex!(self.consume_text(Token::Add, "+"));
        lex!(self.consume_text(Token::Equal, "="));
        lex!(self.consume_text(Token::GT, ">"));
        lex!(self.consume_text(Token::LT, "<"));

        // keywords
        lex!(self.consume_text(Token::Func, "func"));
        lex!(self.consume_text(Token::Return, "return"));
        lex!(self.consume_text(Token::Let, "let"));
        lex!(self.consume_text(Token::If, "if"));
        lex!(self.consume_text(Token::Else, "else"));
        lex!(self.consume_text(Token::While, "while"));
        lex!(self.consume_text(Token::For, "for"));

        // special stuff
        lex!(self.consume_identifier());
        lex!(self.consume_number());
        lex!(self.consume_string());

        let c = test!(self.next()?; LexError::from);
        self.consume(1);
        Some(Ok(Token::Unknown(c)))
    }
}
