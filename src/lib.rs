use std::{
    collections::VecDeque,
    io::{self, BufReader, Read},
};

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
    Eof,
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
}

impl<T: Read + 'static> Iterator for Lexer<T> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        let c = test!(self.next()?; LexError::from);

        let _p1 = test!(self.peek(1).unwrap_or(Ok(' ')); LexError::from);

        let _p2 = test!(self.peek(2).unwrap_or(Ok(' ')); LexError::from);

        let _p3 = test!(self.peek(3).unwrap_or(Ok(' ')); LexError::from);

        print!("{c}");

        Some(Ok(Token::Eof))
    }
}
