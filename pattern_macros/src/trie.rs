use std::str::FromStr;

use proc_macro2::TokenStream;
use quote::quote;

use crate::{dict::OrderedDict, MatchType};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Branch {
    Char(String),
    Func(String),
}

impl Branch {
    pub fn str(&self) -> &String {
        match self {
            Branch::Char(s) => s,
            Branch::Func(s) => s,
        }
    }
}

#[derive(Debug)]
pub struct Trie {
    pub branches: OrderedDict<Branch, Trie>,
    pub leaf: Option<TokenStream>,
}

impl Trie {
    pub fn new() -> Self {
        Self {
            branches: OrderedDict::new(),
            leaf: None,
        }
    }

    pub fn insert(&mut self, branches: impl IntoIterator<Item = MatchType>, leaf: TokenStream) {
        let branches: Vec<MatchType> = branches.into_iter().collect();
        self.insert_recursive(&branches, &leaf, None);
    }

    fn insert_recursive(
        &mut self,
        branches: &[MatchType],
        leaf: &TokenStream,
        parent_char: Option<TokenStream>,
    ) {
        if branches.is_empty() {
            self.leaf = Some(leaf.clone());
            return;
        }

        let branch = &branches[0];
        match branch {
            MatchType::Char(_) | MatchType::Func(_) => {
                let branch_key = match branch {
                    MatchType::Char(_) => Branch::Char(branch.key()),
                    MatchType::Func(_) => Branch::Func(branch.key()),
                    _ => unreachable!(),
                };

                let node = self.branches.get_mut_or_insert_with(branch_key, Trie::new);
                node.insert_recursive(&branches[1..], leaf, None);
            }
            MatchType::ForFunc(key) => {
                self.insert_depth(parent_char.clone(), key, leaf);

                // Continue processing the rest of the branches
                let branch_key = Branch::Func(branch.key());
                let node = self.branches.get_mut_or_insert_with(branch_key, Trie::new);
                node.insert_recursive(&branches[1..], leaf, None);
            }
        }
    }

    fn insert_depth(&mut self, parent: Option<TokenStream>, key: &TokenStream, leaf: &TokenStream) {
        {
            let mut child = Trie::new();
            child.leaf = Some(leaf.clone());
            let key = match parent {
                Some(parent) => quote! { c if const { (#key)(#parent) } && (#key)(c) },
                None => quote! { c if (#key)(c) },
            };

            self.branches.insert(Branch::Func(key.to_string()), child);
        }

        let mut o = vec![];
        for (k, v) in self.branches.map.iter_mut() {
            o.push(k.str().clone());
            if let Branch::Char(k) = k {
                v.insert_depth(
                    Some(TokenStream::from_str(k).expect("failed to reparse")),
                    key,
                    leaf,
                );
            }
            // let k: MatchType = TokenStream::from_str(k).expect("failed to reparse").into();
            // if let MatchType::Char(c) = k {
            //     v.insert_depth(Some(c), key, leaf);
            // }
        }

        // panic!("{o:?}");
    }
}
