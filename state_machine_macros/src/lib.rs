use proc_macro2::{Span, TokenStream, TokenTree};
use quote::{quote, ToTokens};
use syn::{parse_macro_input, Data, DeriveInput, Fields, LitChar, LitInt, LitStr, Meta};

mod dict;

mod lexer;

mod parse_tree;
use parse_tree::into_trie;

mod parser;
use parser::Parser;

mod trie;
use trie::{Branch, Trie, TupleArg, Variant, VariantBody};

#[proc_macro_derive(Lexable, attributes(lex, capture))]
pub fn derive_lexable(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let generics = input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let data = match input.data {
        Data::Enum(e) => e,
        Data::Union(_) | Data::Struct(_) => panic!("Lexable can only be derived on enums"),
    };

    let mut trie = Trie::new();
    for variant in data.variants {
        // TODO: make this get all necessary info to create this token
        let variant_name = &variant.ident;
        let body = match variant.fields {
            Fields::Unit => VariantBody::Unit,
            Fields::Unnamed(unnamed) => {
                let fields = unnamed
                    .unnamed
                    .iter()
                    .map(|field| {
                        field
                            .attrs
                            .iter()
                            .find_map(|attr| match &attr.meta {
                                Meta::List(list) => match list.path.get_ident() {
                                    Some(ident) => match ident.to_string().as_ref() {
                                        "capture" => {
                                            let capture =
                                                extract_number_literal(list.tokens.clone())
                                                    .expect("not a number");

                                            Some(TupleArg::Capture(
                                                capture
                                                    .base10_parse()
                                                    .expect("unable to parse capture"),
                                            ))
                                        }
                                        _ => None,
                                    },
                                    None => None,
                                },
                                _ => None,
                            })
                            .expect("no relevant attributes")
                    })
                    .collect::<Vec<_>>();

                VariantBody::Tuple(fields)
            }
            _ => unimplemented!(),
        };

        let token_variant = Variant {
            name: quote! { #name::#variant_name }.to_string(),
            body,
        };

        for attr in variant.attrs {
            let attr = match attr.meta {
                Meta::List(list) => list,
                _ => continue,
            };

            let ident = match attr.path.get_ident() {
                Some(ident) => ident,
                None => continue,
            };

            if *ident != "lex" {
                continue;
            }

            let pattern = match extract_string_literal(attr.tokens) {
                Some(pattern) => pattern.value(),
                None => panic!("could not parse lex pattern"),
            };

            let parser = Parser::new(&pattern);
            let t = into_trie(
                parser.parse().expect("failed to parse"),
                token_variant.clone(),
            );

            trie.merge(t);
        }
    }

    trie.expand();

    let expanded = expand(&trie, true);

    let expanded = quote! {
        impl #impl_generics crate::Lexable for #name #ty_generics #where_clause {
            #[inline]
            fn lex(input: &mut ::std::iter::Peekable<impl Iterator<Item = char>>) -> Result<Option<Self>, crate::LexError> {
                let mut current_text = String::new();

                #expanded
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

fn extract_string_literal(stream: TokenStream) -> Option<LitStr> {
    let mut stream = stream.into_iter();
    if let Some(TokenTree::Literal(token)) = stream.next() {
        if stream.next().is_none() {
            if let Ok(token) = syn::parse_str::<LitStr>(&token.to_string()) {
                return Some(token);
            }
        }
    }

    None
}

fn extract_number_literal(stream: TokenStream) -> Option<LitInt> {
    let mut stream = stream.into_iter();
    if let Some(TokenTree::Literal(token)) = stream.next() {
        if stream.next().is_none() {
            if let Ok(token) = syn::parse_str::<LitInt>(&token.to_string()) {
                return Some(token);
            }
        }
    }

    None
}

fn expand(trie: &Trie, root: bool) -> TokenStream {
    if let Some(leaf) = &trie.leaf {
        if trie.branches.is_empty() {
            // unambiguous branch, only has a leaf so return that variant
            quote! { Ok(Some(#leaf)) }
        } else {
            // ambiguous branch: has both branches and a leaf so we need to peek to see if we
            // should continue lexing or not
            let branches = trie
                .branches
                .iter()
                .fold(TokenStream::new(), |acc, (k, v)| {
                    let k = to_condition(k);
                    let v = expand(v, false);

                    let branch = condition_to_tokens(k, &v, true);

                    quote! {
                        #acc
                        #branch
                    }
                });

            quote! {
                let p = match input.peek().copied() {
                    Some(p) => p,
                    None => return Ok(Some(#leaf)),
                };

                match p {
                    #branches
                    _ => Ok(Some(#leaf)),
                }
            }
        }
    } else if trie.branches.is_empty() {
        // no leaf and no branches. this should not happen
        unreachable!();
    } else {
        // branches but no leaf: just consume next char
        let branches = trie
            .branches
            .iter()
            .map(|(k, v)| {
                let k = to_condition(k);
                let v = expand(v, false);

                condition_to_tokens(k, &v, false)
            })
            .collect::<Vec<_>>();
        let branches = flatten(branches);

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
                _ => return Err(crate::LexError::UnknownInput(current_text)),
            }
        }
    }
}

fn flatten(streams: Vec<TokenStream>) -> TokenStream {
    let mut out = TokenStream::new();

    for stream in streams {
        out.extend(stream);
    }

    out
}

fn condition_to_tokens(
    (map, expansion): (Vec<Vec<TokenStream>>, bool),
    child: &TokenStream,
    peeking: bool,
) -> TokenStream {
    let streams = map
        .iter()
        .map(|steps| {
            let key = steps.first().expect("empty condition");
            let rest = &steps[1..];
            let peeked = if peeking {
                quote! {
                    input.next();
                    current_text.push(p);
                }
            } else {
                TokenStream::new()
            };

            let prefix = rest.iter().fold(TokenStream::new(), |acc, e| {
                let aarms = map
                    .iter()
                    .filter(|x| x[0].to_string() != key.to_string())
                    .fold(TokenStream::new(), |accu, el| {
                        let el = &el[0];

                        quote! {
                            #accu
                            #el => {}
                        }
                    });
                let arms = quote! {
                    #aarms
                    #e => {
                        input.next();
                        current_text.push(p);

                        #acc
                    }
                };

                quote! {
                    let p = match input.peek().copied() {
                        Some(p) => p,
                        None => return Err(crate::LexError::UnknownInput(current_text)),
                    };

                    match p {
                        #arms
                        _ => return Err(crate::LexError::UnknownInput(current_text)),
                    }
                }
            });

            let expanded = if expansion {
                let arms = map.iter().fold(TokenStream::new(), |acc, conds| {
                    let first = match conds.first() {
                        Some(first) => quote! { #first },
                        None => TokenStream::new(),
                    };

                    let rest = if conds.len() > 1 {
                        let arms = conds[1..].iter().enumerate().fold(TokenStream::new(), |acc, (i, cond)| {
                            if i == conds.len() - 2 {
                                quote! {
                                    #cond => continue,
                                }
                            } else {
                                quote! {
                                    #cond => {
                                        let c = match input.next() {
                                            Some(c) => c,
                                            None => return Err(crate::LexError::UnknownInput(current_text)),
                                        };

                                        current_text.push(c);

                                        match c {
                                            #acc
                                            _ => return Err(crate::LexError::UnknownInput(current_text)),
                                        }
                                    }
                                }
                            }
                        });

                        quote! {
                            let c = match input.next() {
                                Some(c) => c,
                                None => return Err(crate::LexError::UnknownInput(current_text)),
                            };

                            current_text.push(c);

                            match c {
                                #arms
                                _ => return Err(crate::LexError::UnknownInput(current_text)),
                            }
                        }
                    } else {
                        TokenStream::new()
                    };

                    quote! {
                        #acc
                        #first => {
                            input.next();
                            current_text.push(p);

                            #rest
                        }
                    }
                });

                quote! {
                    loop {
                        match input.peek().copied() {
                            Some(p) => {
                                match p {
                                    #arms
                                    _ => break,
                                }
                            }
                            None => break,
                        }
                    }
                }
            } else {
                TokenStream::new()
            };

            quote! {
                #key => {
                    #peeked
                    #prefix
                    #expanded
                    #child
                }
            }
        })
        .collect();

    flatten(streams)
}

fn to_condition(branch: &Branch) -> (Vec<Vec<TokenStream>>, bool) {
    match branch {
        Branch::Char(c) => (
            vec![vec![{
                let c = to_char_token(*c);
                quote! { #c }
            }]],
            false,
        ),
        Branch::Sequence(s) => (
            vec![vec![match s {
                'a' => quote! { c if c.is_alphabetic() },
                'A' => quote! { c if c.is_alphanumeric() },
                '0' => quote! { c if c.is_numeric() },
                _ => unreachable!(),
            }]],
            false,
        ),
        Branch::NegativeChar(c) => (
            vec![vec![{
                let c = to_char_token(*c);
                quote! { c if c != #c }
            }]],
            false,
        ),
        Branch::NegativeSequence(s) => (
            vec![vec![match s {
                'a' => quote! { c if !c.is_alphabetic() },
                'A' => quote! { c if !c.is_alphanumeric() },
                '0' => quote! { c if !c.is_numeric() },
                _ => unreachable!(),
            }]],
            false,
        ),
        Branch::Expand(e) => (to_condition(e).0, true),
        Branch::Options(o) => (
            o.iter()
                .map(|options| {
                    options
                        .iter()
                        .flat_map(|option| to_condition(option).0)
                        .flatten()
                        .collect::<Vec<_>>()
                })
                .collect(),
            false,
        ),
    }
}

fn to_char_token(c: char) -> TokenStream {
    LitChar::new(c, Span::call_site()).to_token_stream()
}

#[cfg(test)]
mod tests {
    use crate::{
        expand,
        parse_tree::into_trie,
        parser::Parser,
        trie::{self, Trie, Variant, VariantBody},
    };

    #[test]
    fn it_works() {
        let patterns = [
            "func",
            "(@a@A*)",
            "//([^\n]*)",
            "/",
            "\"(([^\"]|\\\\\")*)\"",
        ];
        let mut trie = Trie::new();
        for (i, pattern) in patterns.iter().enumerate() {
            let t = into_trie(
                Parser::new(pattern).parse().unwrap(),
                Variant {
                    name: format!("Variant{i}"),
                    body: VariantBody::Unit,
                },
            );
            trie.merge(t);
        }

        trie.expand();

        println!("{}", trie::print(&trie, 0));
        println!("{}", expand(&trie, true));
    }
}
