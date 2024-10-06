use std::{
    io::{self, Read},
    iter::Peekable,
};

use thiserror::Error;

mod derive;

pub use derive::Lexable;
use utf8_reader::Utf8Reader;

#[derive(Debug, Error)]
pub enum LexError {
    #[error("unkown input: {0}")]
    UnknownInput(String),
    #[error("io error: {0}")]
    IoError(#[from] io::Error),
}

#[derive(Debug, Lexable)]
pub enum Token {
    #[lex("//([^\n]*)")]
    Comment(#[capture(1)] String),
    #[lex("/")]
    Division,
    #[lex("func")]
    Func,
    #[lex("return")]
    Return,
    #[lex("while")]
    While,
    #[lex("for")]
    For,
    #[lex("if")]
    If,
    #[lex("else")]
    Else,
    #[lex("let")]
    Let,
    #[lex("@a@A*")]
    Identifier(#[capture(0)] String),
    #[lex(r#""(([^"]|\\")*)""#)]
    String(#[capture(1)] String),
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
    #[lex("@0@0*")]
    Number(#[capture(0)] String),
    #[lex(">")]
    GT,
    #[lex("<")]
    LT,
}

pub struct Lexer<T: Read> {
    reader: Peekable<Utf8Reader<T>>,
    errored: bool,
}

impl<T: Read> Lexer<T> {
    pub fn new(reader: T) -> Self {
        Self {
            reader: Utf8Reader::new(reader).peekable(),
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
