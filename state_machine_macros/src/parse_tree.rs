pub type Expr = Vec<Unit>;

#[derive(Debug, Clone)]
pub enum Unit {
    Char(Char),
    Group(Expr),
    Options(Vec<Expr>),
    NegativeChar(Char),
    Expand(Box<Unit>),
}

#[derive(Debug, Clone)]
pub enum Char {
    Char(char),
    Sequence(char),
}
