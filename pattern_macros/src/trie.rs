use std::collections::HashMap;

use proc_macro2::{TokenStream, TokenTree};

#[derive(Debug)]
pub struct Trie {
    // TODO: this should be a FIFO hashmap when iterating entries
    pub branches: HashMap<String, Trie>,
    pub leaf: Option<TokenStream>,
}

impl Trie {
    pub fn new() -> Self {
        Self {
            branches: HashMap::new(),
            leaf: None,
        }
    }

    pub fn insert(&mut self, branches: impl IntoIterator<Item = TokenTree>, leaf: TokenStream) {
        let mut node = self;
        for branch in branches {
            let branch = branch.to_string();
            let new_node = node.branches.entry(branch).or_insert_with(Trie::new);

            node = new_node;
        }

        node.leaf = Some(leaf);
    }
}
