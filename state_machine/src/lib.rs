use std::{io::Read, iter::Peekable};

use thiserror::Error;

mod derive;

pub use derive::Lexable;
use utf8_reader::Utf8Reader;

#[derive(Debug, Error)]
pub enum LexError {
    #[error("unkown input: {0}")]
    UnknownInput(String),
}

#[derive(Debug, Lexable)]
pub enum Token {
    #[lex("//([^\n]*)")]
    Comment,
    #[lex("/")]
    Division,
    #[lex("func")]
    Func,
    #[lex("(@a@A*)")]
    Identifier,
    #[lex(r#""(([^"]|\\")*)""#)]
    String,
    #[lex("(\n|\r)")]
    NewLine,
    #[lex(" ")]
    Space,
    #[lex(r#"\("#)]
    LParen,
    #[lex(r#"\)"#)]
    RParen,
    #[lex(",")]
    Comma,
    #[lex("{")]
    RBrace,
    #[lex("}")]
    LBrace,
    #[lex("+")]
    Add,
    #[lex("+=")]
    AddAssign,
    #[lex("-")]
    Sub,
    #[lex("-=")]
    SubAssign,
    #[lex(";")]
    Semicolon,
    #[lex("=")]
    Equals,
    #[lex("(@0@0*)")]
    Number,
    #[lex(">")]
    GT,
    #[lex("<")]
    LT,
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
