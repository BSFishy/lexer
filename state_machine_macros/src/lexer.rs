use std::{iter::Peekable, str::Chars};

use thiserror::Error;

#[derive(Debug, Copy, Clone)]
pub enum Token {
    Char(char),
    LSqBracket,
    RSqBracket,
    Star,
    Carat,
    Sequence(char),
    Or,
    LParen,
    RParen,
}

#[derive(Debug, Error, Clone)]
pub enum LexError {
    #[error("unfinished sequence")]
    UnfinishedSequence,
    #[error("unexpected input (expected {expected}, found {found:?})")]
    UnexpectedToken { expected: String, found: Token },
    #[error("unexpected end of stream (expected {0})")]
    UnexpectedEos(String),
}

pub struct Lexer<'a> {
    pattern: Chars<'a>,
    errored: bool,
}

impl Lexer<'_> {
    pub fn new(pattern: &str) -> Lexer<'_> {
        Lexer {
            pattern: pattern.chars(),
            errored: false,
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.errored {
            return None;
        }

        let c = match self.pattern.next() {
            Some(c) => c,
            None => return None,
        };

        let c = match c {
            '*' => Some(Ok(Token::Star)),
            '[' => Some(Ok(Token::LSqBracket)),
            ']' => Some(Ok(Token::RSqBracket)),
            '^' => Some(Ok(Token::Carat)),
            '|' => Some(Ok(Token::Or)),
            '(' => Some(Ok(Token::LParen)),
            ')' => Some(Ok(Token::RParen)),

            '@' => {
                let c = match self.pattern.next() {
                    Some(c) => c,
                    None => {
                        self.errored = true;
                        return Some(Err(LexError::UnfinishedSequence));
                    }
                };

                Some(Ok(Token::Sequence(c)))
            }

            '\\' => {
                let c = match self.pattern.next() {
                    Some(c) => c,
                    None => {
                        self.errored = true;
                        return Some(Err(LexError::UnfinishedSequence));
                    }
                };

                Some(Ok(Token::Char(c)))
            }

            _ => Some(Ok(Token::Char(c))),
        };
        println!("next token: {c:?}");
        c
    }
}
