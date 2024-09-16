use std::{io::Read, iter::Peekable};

use thiserror::Error;
use utf8_reader::Utf8Reader;

mod derive;

pub use derive::Lexable;

#[derive(Debug, Lexable)]
pub enum Token {
    #[lex('/', '/', |c| c != '\n')]
    Comment,
    #[lex('/')]
    Division,
    #[lex('f', 'u', 'n', 'c')]
    Func,
}

#[derive(Debug, Error)]
pub enum LexError {
    #[error("unkown input: {0}")]
    UnknownInput(String),
}

struct Reader<T: Read>(Utf8Reader<T>);

impl<T: Read> Iterator for Reader<T> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|c| c.unwrap())
    }
}

pub struct Lexer<T: Read> {
    reader: Peekable<Reader<T>>,
    errored: bool,
}

impl<T: Read> Lexer<T> {
    pub fn new(reader: T) -> Self {
        Self {
            reader: Reader(Utf8Reader::new(reader)).peekable(),
            errored: false,
        }
    }
}

impl<T: Read> Iterator for Lexer<T> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.errored {
            None
        } else {
            match Token::lex(&mut self.reader) {
                Ok(Some(t)) => Some(Ok(t)),
                Err(err) => {
                    self.errored = true;
                    Some(Err(err))
                }
                Ok(None) => None,
            }
        }
    }
}
