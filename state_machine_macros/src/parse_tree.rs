use crate::trie::{Branch, GroupOp, GroupableBranch, Trie, Variant};

pub type Expr = Vec<Unit>;

#[derive(Debug, Clone)]
pub enum Unit {
    Char(Char),
    Group(usize, Expr),
    Options(Vec<Expr>),
    NegativeChar(Char),
    Expand(Box<Unit>),
}

#[derive(Debug, Clone)]
pub enum Char {
    Char(char),
    Sequence(char),
}

impl From<Unit> for Vec<GroupableBranch> {
    fn from(value: Unit) -> Self {
        match value {
            Unit::Char(Char::Char(c)) => vec![Branch::Char(c).into()],
            Unit::Char(Char::Sequence(s)) => vec![Branch::Sequence(s).into()],
            Unit::NegativeChar(Char::Char(c)) => vec![Branch::NegativeChar(c).into()],
            Unit::NegativeChar(Char::Sequence(s)) => vec![Branch::NegativeSequence(s).into()],
            Unit::Expand(e) => vec![Branch::Expand({
                let vec: Vec<GroupableBranch> = (*e).into();

                match vec.len() {
                    1 => Box::new(vec[0].clone()),
                    _ => panic!("non trivial expansion"),
                }
            })
            .into()],
            Unit::Group(i, g) => {
                let mut g = g
                    .into_iter()
                    .flat_map(<Vec<GroupableBranch> as From<Unit>>::from)
                    .collect::<Vec<_>>();

                if let Some(start) = g.first_mut() {
                    start.op.insert(GroupOp::Start(i));
                }

                if let Some(end) = g.last_mut() {
                    end.op.insert(GroupOp::Stop(i));

                    if let Branch::Expand(_) = end.branch {
                        if g.len() > 1 {
                            let idx = g.len() - 2;
                            g[idx].op.insert(GroupOp::Stop(i));
                        }
                    }
                }

                g
            }
            Unit::Options(o) => vec![Branch::Options(
                o.into_iter()
                    .map(|e| {
                        e.into_iter()
                            .flat_map(<Vec<GroupableBranch> as From<Unit>>::from)
                            .collect()
                    })
                    .collect(),
            )
            .into()],
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
    let branches = <Vec<GroupableBranch>>::from(unit);

    let slice = &slice[1..];
    let parent = parent.search(&branches[..branches.len() - 1]);
    let last_branch = branches.last().expect("empty branches").clone();
    let search = parent.search(&[last_branch.clone()]);

    if slice.is_empty() {
        search.leaf = Some(variant.clone());

        if let Branch::Expand(_) = last_branch.branch {
            if parent.leaf.is_none() {
                parent.leaf = Some(variant);
            }
        }
    } else {
        into_trie_impl(slice, search, variant);
    }
}
