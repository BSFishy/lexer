use crate::trie::{Branch, Trie, Variant};

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

impl From<Unit> for Vec<Branch> {
    fn from(value: Unit) -> Self {
        match value {
            Unit::Char(Char::Char(c)) => vec![Branch::Char(c)],
            Unit::Char(Char::Sequence(s)) => vec![Branch::Sequence(s)],
            Unit::NegativeChar(Char::Char(c)) => vec![Branch::NegativeChar(c)],
            Unit::NegativeChar(Char::Sequence(s)) => vec![Branch::NegativeSequence(s)],
            Unit::Expand(e) => vec![Branch::Expand({
                let vec = <Vec<Branch>>::from(*e);

                match vec.len() {
                    1 => Box::new(vec[0].clone()),
                    _ => panic!("non trivial expansion"),
                }
            })],
            Unit::Group(g) => g
                .into_iter()
                .flat_map(<Vec<Branch> as From<Unit>>::from)
                .collect(),
            Unit::Options(o) => vec![Branch::Options(
                o.into_iter()
                    .map(|e| {
                        e.into_iter()
                            .flat_map(<Vec<Branch> as From<Unit>>::from)
                            .collect()
                    })
                    .collect(),
            )],
        }
    }
}

pub fn into_trie(expr: Expr, variant: Variant) -> Trie {
    let mut t = Trie::new();

    into_trie_impl(expr.as_slice(), &mut t, variant);

    t
}

fn into_trie_impl(slice: &[Unit], parent: &mut Trie, variant: Variant) {
    let unit = slice[0].clone();
    let branches = <Vec<Branch>>::from(unit);

    let slice = &slice[1..];
    let parent = parent.search(&branches[..branches.len() - 1]);
    let last_branch = branches.last().expect("empty branches").clone();
    let search = parent.search(&[last_branch.clone()]);

    if slice.is_empty() {
        search.leaf = Some(variant.clone());

        if let Branch::Expand(_) = last_branch {
            if parent.leaf.is_none() {
                parent.leaf = Some(variant);
            }
        }
    } else {
        into_trie_impl(slice, search, variant);
    }
}
