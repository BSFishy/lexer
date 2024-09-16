use std::str::FromStr;

use proc_macro2::{Delimiter, Group, TokenStream, TokenTree};
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, LitChar, Meta};

mod trie;
use trie::Trie;

#[proc_macro_derive(Lexable, attributes(lex))]
pub fn derive_lexable(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let generics = input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let data = match input.data {
        Data::Enum(e) => e,
        Data::Union(_) | Data::Struct(_) => panic!("Lexer can only be derived on enums"),
    };

    let mut trie = Trie::new();
    for variant in data.variants {
        let variant_ident = &variant.ident;
        for attr in variant.attrs {
            let attr = match attr.meta {
                Meta::List(list) => list,
                Meta::NameValue(_) | Meta::Path(_) => continue,
            };

            let ident = match attr.path.get_ident() {
                Some(ident) => ident,
                None => continue,
            };

            if *ident != "lex" {
                continue;
            }

            let tokens = attr.tokens;
            let tokens = split_tokenstream_into_tokens(tokens);
            trie.insert(tokens, quote! { #name::#variant_ident});
        }
    }

    assert!(
        trie.leaf.is_none(),
        "every token needs something to match on (token with nothing: {:?}",
        trie.leaf
    );

    let expanded = expand(trie);
    let expanded = quote! {
        impl #impl_generics crate::Lexable for #name #ty_generics #where_clause {
            fn lex(input: &mut ::std::iter::Peekable<impl Iterator<Item = char>>) -> Option<Self> {
                #expanded
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

fn split_tokenstream_into_tokens(input: TokenStream) -> Vec<TokenTree> {
    let mut tokens = Vec::new();
    let mut current_stream = TokenStream::new();

    for token in input.into_iter() {
        match &token {
            TokenTree::Punct(punct) => {
                if punct.as_char() == ',' {
                    tokens.push(TokenTree::Group(Group::new(
                        Delimiter::None,
                        current_stream,
                    )));
                    current_stream = TokenStream::new();
                } else {
                    current_stream.extend(Some(token));
                }
            }
            _ => {
                current_stream.extend(Some(token));
            }
        }
    }

    if !current_stream.is_empty() {
        tokens.push(TokenTree::Group(Group::new(
            Delimiter::None,
            current_stream,
        )));
    }

    tokens
}

fn expand(trie: Trie) -> TokenStream {
    if let Some(ident) = trie.leaf {
        if trie.branches.is_empty() {
            // only has leaf, so so just return that value.
            quote! { Some(#ident) }
        } else {
            // has both a leaf and branches. means that we need to peek to see if we need to
            // continue lexing or just return the thing.
            let branches = trie.branches.into_iter().map(|(key, value)| {
                let key: MatchType = TokenStream::from_str(&key)
                    .expect("failed to reparse branch token stream")
                    .into();
                let value = expand(value);

                match key {
                    MatchType::Char(key) => quote! {
                        #key => {
                            input.next()?;

                            #value
                        }
                    },
                    MatchType::Func(key) => quote! {
                        c in (#key)(c) => {
                            input.next()?;

                            #value
                        }
                    },
                }
            });
            let branches = join_tokenstreams(branches);

            quote! {
                match input.peek() {
                    Some(p) => match p {
                        #branches
                        _ => Some(#ident),
                    }
                    None => Some(#ident),
                }
            }
        }
    } else {
        // has no leaf. always consume the next token and match on the character.
        let branches = trie.branches.into_iter().map(|(key, value)| {
            let key: MatchType = TokenStream::from_str(&key)
                .expect("failed to reparse branch token stream")
                .into();
            let value = expand(value);

            match key {
                MatchType::Char(key) => quote! {
                    #key => {
                        #value
                    }
                },
                MatchType::Func(key) => quote! {
                    c if (#key)(c) => {
                        #value
                    }
                },
            }
        });
        let branches = join_tokenstreams(branches);

        quote! {
            match input.next()? {
                #branches

                _ => None,
            }
        }
    }
}

#[derive(Debug)]
enum MatchType {
    Char(TokenStream),
    Func(TokenStream),
}

impl From<TokenStream> for MatchType {
    fn from(value: TokenStream) -> Self {
        let mut iter = value.clone().into_iter();

        // Check if the TokenStream is a single token
        if let Some(token) = iter.next() {
            // Ensure the token stream has exactly one token
            if iter.next().is_none() {
                if let TokenTree::Literal(literal) = token {
                    // Parse the literal as a char
                    if let Ok(char_literal) = syn::parse_str::<LitChar>(&literal.to_string()) {
                        return MatchType::Char(quote::quote! { #char_literal });
                    }
                }
            }
        }

        // If it's not a single char literal, return it as a Func
        MatchType::Func(value)
    }
}

fn join_tokenstreams(streams: impl IntoIterator<Item = TokenStream>) -> TokenStream {
    let mut combined = TokenStream::new();
    for stream in streams {
        combined.extend(stream);
    }

    combined
}
