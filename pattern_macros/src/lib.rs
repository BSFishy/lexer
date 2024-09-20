use std::str::FromStr;

use proc_macro2::{TokenStream, TokenTree};
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, LitChar, Meta};

mod trie;
use trie::Trie;

mod dict;

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
            let tokens: Vec<MatchType> = split_tokenstream_into_tokens(tokens)
                .into_iter()
                .map(|s| s.into())
                .collect();

            trie.insert(tokens, quote! { #name::#variant_ident });
        }
    }

    assert!(
        trie.leaf.is_none(),
        "every token needs something to match on (token with nothing: {:?}",
        trie.leaf
    );

    let expanded = expand(trie, true);
    let expanded = quote! {
        impl #impl_generics crate::Lexable for #name #ty_generics #where_clause {
            fn lex(input: &mut ::std::iter::Peekable<impl Iterator<Item = char>>) -> Result<Option<Self>, crate::LexError> {
                let mut current_text = String::new();

                #expanded
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

fn split_tokenstream_into_tokens(input: TokenStream) -> Vec<TokenStream> {
    let mut tokens = Vec::new();
    let mut current_stream = TokenStream::new();

    for token in input.into_iter() {
        match &token {
            TokenTree::Punct(punct) => {
                if punct.as_char() == ',' {
                    tokens.push(current_stream);
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
        tokens.push(current_stream);
    }

    tokens
}

fn expand(trie: Trie, root: bool) -> TokenStream {
    if let Some(ident) = trie.leaf {
        if trie.branches.is_empty() {
            // only has leaf, so so just return that value.
            quote! { Ok(Some(#ident)) }
        } else {
            // has both a leaf and branches. means that we need to peek to see if we need to
            // continue lexing or just return the thing.
            let branches = trie.branches.into_iter().map(|(key, value)| {
                let key = TokenStream::from_str(key.str())
                    .expect("failed to reparse branch token stream");
                let value = expand(value, false);

                // we already peeked the next character, so consuming that character should never
                // return none
                quote! {
                    #key =>  {
                        let c = match input.next() {
                            Some(c) => c,
                            None => unreachable!(),
                        };

                        current_text.push(c);

                        #value
                    }
                }
            });
            let branches = join_tokenstreams(branches);

            quote! {
                match input.peek() {
                    Some(p) => match p {
                        #branches
                        _ => Ok(Some(#ident)),
                    }
                    None => Ok(Some(#ident)),
                }
            }
        }
    } else {
        // has no leaf. always consume the next token and match on the character.
        let branches = trie.branches.into_iter().map(|(key, value)| {
            let key =
                TokenStream::from_str(key.str()).expect("failed to reparse branch token stream");
            let value = expand(value, false);

            quote! {
                #key => {
                    #value
                }
            }
        });
        let branches = join_tokenstreams(branches);

        let return_value = if root {
            quote! { Ok(None) }
        } else {
            quote! { Err(crate::LexError::UnknownInput(current_text)) }
        };

        quote! {
            let c = match input.next() {
                Some(c) => c,
                None => return #return_value,
            };

            current_text.push(c);

            match c {
                #branches

                _ => Err(crate::LexError::UnknownInput(current_text)),
            }
        }
    }
}

#[derive(Debug)]
enum MatchType {
    Char(TokenStream),
    Func(TokenStream),
    ForFunc(TokenStream),
}

impl MatchType {
    fn key(&self) -> String {
        match self {
            MatchType::Char(key) => quote! {
                #key
            },
            MatchType::Func(key) => quote! {
                c if (#key)(c)
            },
            MatchType::ForFunc(key) => quote! {
                c if (#key)(c)
            },
        }
        .to_string()
    }
}

impl From<TokenStream> for MatchType {
    fn from(value: TokenStream) -> Self {
        let mut iter = value.clone().into_iter();

        // Check if the TokenStream starts with 'for'
        if let Some(TokenTree::Ident(ident)) = iter.next() {
            if ident == "for" {
                // Collect the rest of the tokens without the 'for' keyword
                let rest_of_tokens: TokenStream = iter.collect();
                return MatchType::ForFunc(rest_of_tokens);
            }
        }

        // Reset the iterator for the next check
        let mut iter = value.clone().into_iter();

        // Check if the TokenStream is a single char literal
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

        // If it's not a single char literal or 'for' expression, return it as a Func
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
