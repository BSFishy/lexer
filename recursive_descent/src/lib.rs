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
    buffer: VecDeque<char>,
}

impl<T: Read> Lexer<T> {
    pub fn new(reader: T) -> Self {
        Lexer {
            reader: Utf8Reader::new(reader),
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
                        Ok(contents) => Some(Ok(Token::Comment(contents))),
                        Err(err) => Some(Err(err)),
                    }
                } else {
                    todo!();
                }
            }
            '\n' => Some(Ok(Token::NewLine)),
            '\r' => {
                if self.has_next("\n") {
                    test!(self.next()?; LexError::from);
                }

                Some(Ok(Token::NewLine))
            }
            ' ' => Some(Ok(Token::Space)),
            '(' => Some(Ok(Token::LParen)),
            ')' => Some(Ok(Token::RParen)),
            '{' => Some(Ok(Token::LBracket)),
            '}' => Some(Ok(Token::RBracket)),
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
