use std::{io::Read, iter::Peekable};

use utf8_reader::Utf8Reader;

mod derive;

pub use derive::Lexable;

#[derive(Debug, Lexable)]
pub enum Token {
    #[lex('/', '/', |c| c != '\n')]
    Comment,
    #[lex('/')]
    Division,
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
}

impl<T: Read> Lexer<T> {
    pub fn new(reader: T) -> Self {
        Self {
            reader: Reader(Utf8Reader::new(reader)).peekable(),
        }
    }
}

impl<T: Read> Iterator for Lexer<T> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        Token::lex(&mut self.reader)
    }
}
