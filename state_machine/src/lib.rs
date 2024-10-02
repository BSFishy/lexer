use thiserror::Error;

mod derive;

pub use derive::Lexable;

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
    #[lex("\"(([^\"]|\\\\\")*)\"")]
    String,
}
