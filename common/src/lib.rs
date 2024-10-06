use std::fmt;

use console::style;

pub const CODE: &str = include_str!("../../example.lang");
pub const BENCH_LEN: usize = 10;

pub fn code(len: usize) -> String {
    let mut out = String::new();

    for _ in 0..len {
        out.push_str(CODE);
    }

    out
}

#[macro_export]
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
    Comment(String),
    Identifier(String),
    NewLine,
    Space,
    // TODO: keyword token or just like this?
    Func,
    Return,
    Let,
    If,
    Else,
    While,
    For,
    String(String),
    // NOTE: we keep this as a string, because quite frankly, we don't really care about the value
    // right now
    Number(String),
    Unknown(char),
    RParen,
    LParen,
    RBracket,
    LBracket,
    Semicolon,
    Add,
    Sub,
    AddAssign,
    SubAssign,
    Equal,
    Comma,
    GT,
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
            Token::Unknown(c) => write!(f, "{}", style(c).dim()),
            Token::LParen => write!(f, "{}", style("(").cyan()),
            Token::RParen => write!(f, "{}", style(")").cyan()),
            Token::LBracket => write!(f, "{}", style("{").cyan()),
            Token::RBracket => write!(f, "{}", style("}").cyan()),
            Token::Semicolon => write!(f, "{}", style(";").cyan()),
            Token::Add => write!(f, "{}", style("+").cyan()),
            Token::Sub => write!(f, "{}", style("-").cyan()),
            Token::AddAssign => write!(f, "{}", style("+=").cyan()),
            Token::SubAssign => write!(f, "{}", style("-=").cyan()),
            Token::Equal => write!(f, "{}", style("=").cyan()),
            Token::Comma => write!(f, "{}", style(",").cyan()),
            Token::GT => write!(f, "{}", style(">").cyan()),
            Token::LT => write!(f, "{}", style("<").cyan()),
        }
    }
}
