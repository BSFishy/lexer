use std::{io, iter::Peekable};

pub use state_machine_macros::Lexable;

pub trait Lexable: Sized {
    fn lex(
        input: &mut Peekable<impl Iterator<Item = Result<char, io::Error>>>,
    ) -> Result<Option<Self>, crate::LexError>;
}
