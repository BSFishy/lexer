use std::{collections::HashMap, ops::Deref};

use crate::dict::OrderedDict;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Branch {
    Char(char),
    Sequence(char),
    NegativeChar(char),
    NegativeSequence(char),
    Expand(Box<Branch>),
    Options(Vec<Vec<Branch>>),
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub(crate) name: String,
}

#[derive(Debug, Clone)]
pub struct Trie {
    pub(crate) branches: OrderedDict<Branch, Trie>,
    pub(crate) leaf: Option<Variant>,
}

impl Trie {
    pub fn new() -> Trie {
        Trie {
            branches: OrderedDict::new(),
            leaf: None,
        }
    }

    pub fn insert(&mut self, branch: Branch) -> &mut Trie {
        self.branches.get_mut_or_insert_with(branch, Trie::new)
    }

    pub fn search(&mut self, branches: &[Branch]) -> &mut Trie {
        if branches.is_empty() {
            return self;
        }

        let branch = branches[0].clone();
        self.insert(branch).search(&branches[1..])
    }

    pub fn merge(&mut self, other: Trie) {
        for (branch, t) in other.branches {
            self.insert(branch).merge(t);
        }

        if let Some(leaf) = other.leaf {
            self.leaf = Some(leaf.clone());
        }
    }

    fn clone(&mut self, other: &Trie) {
        for (branch, t) in other.branches.iter() {
            self.insert(branch.clone()).merge(t.clone());
        }

        if let Some(leaf) = &other.leaf {
            if self.leaf.is_none() {
                self.leaf = Some(leaf.clone());
            }
        }
    }

    pub fn expand(&mut self) {
        self.expand_non_trivial();
        self.expand_expansions();

        for branch in self.branches.map.values_mut() {
            branch.expand()
        }
    }

    fn expand_non_trivial(&mut self) {
        let mut non_trivial_branches = HashMap::new();
        for (key, non_trivial) in self.branches.iter() {
            match key {
                Branch::Sequence(_) | Branch::NegativeChar(_) | Branch::NegativeSequence(_) => {
                    non_trivial_branches.insert(key.clone(), non_trivial.clone());
                }
                _ => continue,
            }
        }

        for (c, trivial) in self.branches.map.iter_mut() {
            match c {
                Branch::Char(c) => {
                    for (k, non_trivial) in non_trivial_branches.iter() {
                        match k {
                            Branch::Sequence(k) => {
                                if sequence(*k, *c) {
                                    trivial.clone(non_trivial);
                                }
                            }
                            Branch::NegativeChar(k) => {
                                if c != k {
                                    trivial.clone(non_trivial);
                                }
                            }
                            Branch::NegativeSequence(k) => {
                                if !sequence(*k, *c) {
                                    trivial.clone(non_trivial);
                                }
                            }
                            // SAFETY: filtered out above
                            _ => unreachable!(),
                        }
                    }
                }
                _ => continue,
            }
        }
    }

    fn expand_expansions(&mut self) {
        let mut expansion_branches = HashMap::new();
        for (key, non_trivial) in self.branches.iter() {
            match key {
                Branch::Expand(_) => {
                    expansion_branches.insert(key.clone(), non_trivial.clone());
                }
                _ => continue,
            }
        }

        for (c, trivial) in self.branches.map.iter_mut() {
            match c {
                Branch::Char(c) => {
                    for (k, expansion) in expansion_branches.iter() {
                        match k {
                            Branch::Expand(e) => match e.deref() {
                                Branch::Sequence(s) => {
                                    if sequence(*s, *c) {
                                        trivial.insert(k.clone()).merge(expansion.clone());

                                        if trivial.leaf.is_none() {
                                            if let Some(leaf) = &expansion.leaf {
                                                trivial.leaf = Some(leaf.clone());
                                            }
                                        }
                                    }
                                }
                                Branch::NegativeChar(s) => {
                                    if c != s {
                                        trivial.insert(k.clone()).merge(expansion.clone());

                                        if trivial.leaf.is_none() {
                                            if let Some(leaf) = &expansion.leaf {
                                                trivial.leaf = Some(leaf.clone());
                                            }
                                        }
                                    }
                                }
                                Branch::NegativeSequence(s) => {
                                    if !sequence(*s, *c) {
                                        trivial.insert(k.clone()).merge(expansion.clone());

                                        if trivial.leaf.is_none() {
                                            if let Some(leaf) = &expansion.leaf {
                                                trivial.leaf = Some(leaf.clone());
                                            }
                                        }
                                    }
                                }
                                _ => panic!("non trivial expansion"),
                            },
                            // SAFETY: filtered out above
                            _ => unreachable!(),
                        }
                    }
                }
                _ => continue,
            }
        }
    }
}

fn sequence(seq: char, c: char) -> bool {
    match seq {
        'a' => c.is_alphabetic(),
        'A' => c.is_alphanumeric(),
        _ => panic!("unknown sequence {seq}"),
    }
}

#[cfg(test)]
fn repeat(c: char, amount: usize) -> String {
    let mut out = String::new();

    for _ in 0..amount {
        out.push(c);
    }

    out
}

#[cfg(test)]
pub(crate) fn print(trie: &Trie, level: usize) -> String {
    let mut out = String::new();

    for (k, branch) in trie.branches.iter() {
        out.push_str(&format!(
            "{}{:?}{}\n",
            repeat(' ', level * 4),
            k,
            match &branch.leaf {
                Some(leaf) => format!(" - {:?}", leaf),
                None => "".to_string(),
            }
        ));

        out.push_str(&print(branch, level + 1));
    }

    out
}

#[cfg(test)]
mod tests {
    use crate::{parse_tree::into_trie, parser::Parser};

    use super::{print, Trie, Variant};

    #[test]
    fn it_works() {
        let patterns = vec!["func", "(@a@A*)"];
        let mut trie = Trie::new();
        for pattern in patterns {
            let t = into_trie(
                Parser::new(pattern).parse().unwrap(),
                Variant {
                    name: pattern.to_string(),
                },
            );
            trie.merge(t);
        }

        trie.expand();

        println!("{}", print(&trie, 0));
    }
}
