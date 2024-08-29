use std::{
    collections::VecDeque,
    io::{self, Read},
};

use common::{test, Token};
use thiserror::Error;
use utf8_reader::Utf8Reader;

#[derive(Debug, Error)]
pub enum LexError {
    #[error("failed to read")]
    ReadError(#[from] io::Error),
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
