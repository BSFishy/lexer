use std::iter::Peekable;

pub use pattern_macros::Lexable;

pub trait Lexable: Sized {
    fn lex(input: &mut Peekable<impl Iterator<Item = char>>) -> Option<Self>;
}
