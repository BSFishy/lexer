use std::iter::Peekable;

use thiserror::Error;

use crate::{
    lexer::{LexError, Lexer, Token},
    parse_tree::{Char, Expr, Unit},
};

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("lex error: {0}")]
    LexError(#[from] LexError),
    #[error("unexpected input (expected {expected}, found {found:?})")]
    UnexpectedInput { expected: String, found: Token },
    #[error("groups cannot be empty")]
    EmptyGroup,
    #[error("unexpected end of input")]
    Eof,
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl Parser<'_> {
    pub fn new(pattern: &str) -> Parser<'_> {
        Parser {
            lexer: Lexer::new(pattern).peekable(),
        }
    }

    pub fn parse(mut self) -> Result<Expr, ParseError> {
        self.expr()
    }

    fn expr(&mut self) -> Result<Expr, ParseError> {
        let mut out = Expr::new();

        while let Some(unit) = self.unit()? {
            out.push(unit);
        }

        Ok(out)
    }

    fn unit(&mut self) -> Result<Option<Unit>, ParseError> {
        let t = match self.lexer.peek() {
            Some(t) => t.clone().map_err(ParseError::from)?,
            None => return Ok(None),
        };

        let u = match t {
            Token::Char(c) => {
                self.lexer.next().unwrap()?;
                Unit::Char(Char::Char(c))
            }
            Token::Sequence(c) => {
                self.lexer.next().unwrap()?;
                Unit::Char(Char::Sequence(c))
            }
            Token::LParen => {
                self.lexer.next().unwrap()?;
                self.group()?
            }
            Token::LSqBracket => {
                self.lexer.next().unwrap()?;
                self.negative_char()?
            }
            _ => return Ok(None),
        };

        let t = match self.lexer.peek() {
            Some(t) => t.clone().map_err(ParseError::from)?,
            None => return Ok(Some(u)),
        };

        if let Token::Star = t {
            self.lexer.next();

            Ok(Some(Unit::Expand(Box::new(u))))
        } else {
            Ok(Some(u))
        }
    }

    fn char(&mut self) -> Result<Char, ParseError> {
        let t = match self.lexer.next() {
            Some(t) => t.map_err(ParseError::from)?,
            None => return Err(ParseError::Eof),
        };

        match t {
            Token::Char(t) => Ok(Char::Char(t)),
            Token::Sequence(t) => Ok(Char::Sequence(t)),
            t => Err(ParseError::UnexpectedInput {
                expected: "CHAR".to_string(),
                found: t,
            }),
        }
    }

    fn group(&mut self) -> Result<Unit, ParseError> {
        println!("entering group");
        let mut options = vec![];
        let mut option = self.expr()?;

        loop {
            options.push(option);

            let t = match self.lexer.peek() {
                Some(t) => t.clone().map_err(ParseError::from)?,
                None => {
                    println!("eos");
                    break;
                }
            };
            println!("peek: {t:?}");

            match t {
                Token::Or => {
                    self.or()?;
                    option = self.expr()?;
                }
                _ => break,
            }
        }

        println!("exiting group, {options:?}");

        self.right_paren()?;

        match options.len() {
            0 => Err(ParseError::EmptyGroup),
            1 => Ok(Unit::Group(options[0].clone())),
            _ => Ok(Unit::Options(options)),
        }
    }

    fn negative_char(&mut self) -> Result<Unit, ParseError> {
        println!("entering negative char");
        self.carat()?;

        let c = self.char()?;

        self.right_square_bracket()?;

        Ok(Unit::NegativeChar(c))
    }
}

macro_rules! lex_token {
    ($e:expr; $path:path => $name:expr) => {
        match $e {
            Some(t) => match t? {
                $path => Ok(()),
                t => Err(LexError::UnexpectedToken {
                    expected: $name.to_string(),
                    found: t,
                }),
            },
            None => Err(LexError::UnexpectedEos($name.to_string())),
        }
    };
}

impl Parser<'_> {
    fn left_square_bracket(&mut self) -> Result<(), LexError> {
        lex_token!(self.lexer.next(); Token::LSqBracket => "LEFT SQUARE BRACKET")
    }

    fn right_square_bracket(&mut self) -> Result<(), LexError> {
        lex_token!(self.lexer.next(); Token::RSqBracket => "RIGHT SQUARE BRACKET")
    }

    fn star(&mut self) -> Result<(), LexError> {
        lex_token!(self.lexer.next(); Token::Star => "STAR")
    }

    fn carat(&mut self) -> Result<(), LexError> {
        lex_token!(self.lexer.next(); Token::Carat => "CARAT")
    }

    fn or(&mut self) -> Result<(), LexError> {
        lex_token!(self.lexer.next(); Token::Or => "OR")
    }

    fn left_paren(&mut self) -> Result<(), LexError> {
        lex_token!(self.lexer.next(); Token::LParen => "LEFT PARENTHESIS")
    }

    fn right_paren(&mut self) -> Result<(), LexError> {
        lex_token!(self.lexer.next(); Token::RParen => "RIGHT PARENTHESIS")
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;

    #[test]
    fn parses_comment() {
        let parser = Parser::new("//([^\n]*)");
        let tree = parser.parse();

        assert!(tree.is_ok());
    }

    #[test]
    fn parses_func() {
        let parser = Parser::new("func");
        let tree = parser.parse();

        assert!(tree.is_ok());
    }

    #[test]
    fn parses_ident() {
        let parser = Parser::new("(@a@A*)");
        let tree = parser.parse();

        assert!(tree.is_ok());
    }

    #[test]
    fn parses_string() {
        let parser = Parser::new("\"(([^\"]|\\\\\")*)\"");
        let tree = parser.parse();

        assert!(tree.is_ok());
    }
}
