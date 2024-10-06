use std::{
    fmt,
    io::{self, Read},
    iter::Peekable,
};

use console::style;
use thiserror::Error;

mod derive;

pub use derive::Lexable;
use utf8_reader::Utf8Reader;

#[derive(Debug, Error)]
pub enum LexError {
    #[error("unkown input: {0}")]
    UnknownInput(String),
    #[error("io error: {0}")]
    Io(#[from] io::Error),
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
            Token::LParen => write!(f, "{}", style("(").cyan()),
            Token::RParen => write!(f, "{}", style(")").cyan()),
            Token::LBrace => write!(f, "{}", style("}").cyan()),
            Token::RBrace => write!(f, "{}", style("{").cyan()),
            Token::Semicolon => write!(f, "{}", style(";").cyan()),
            Token::Add => write!(f, "{}", style("+").cyan()),
            Token::Sub => write!(f, "{}", style("-").cyan()),
            Token::AddAssign => write!(f, "{}", style("+=").cyan()),
            Token::SubAssign => write!(f, "{}", style("-=").cyan()),
            Token::Equals => write!(f, "{}", style("=").cyan()),
            Token::Comma => write!(f, "{}", style(",").cyan()),
            Token::GT => write!(f, "{}", style(">").cyan()),
            Token::LT => write!(f, "{}", style("<").cyan()),
            Token::Division => write!(f, "{}", style("=").cyan()),
        }
    }
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
