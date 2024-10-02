use parser::Parser;
use proc_macro2::{TokenStream, TokenTree};
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, LitStr, Meta};

mod lexer;
use lexer::Lexer;

mod parse_tree;
mod parser;

#[proc_macro_derive(Lexable, attributes(lex))]
pub fn derive_lexable(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let generics = input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let data = match input.data {
        Data::Enum(e) => e,
        Data::Union(_) | Data::Struct(_) => panic!("Lexable can only be derived on enums"),
    };

    let mut strs = vec![];
    for variant in data.variants {
        // TODO: make this get all necessary info to create this token
        let _variant_name = &variant.ident;
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

            let pattern = match extract_string_literal(attr.tokens) {
                Some(pattern) => pattern.value(),
                None => panic!("could not parse lex pattern"),
            };

            // let lexer = Lexer::new(&pattern);
            let parser = Parser::new(&pattern);
            // panic!("{:?}", parser.parse());

            strs.push(parser.parse());
        }
    }

    panic!("{:?}", strs);

    let expanded = quote! {
        impl #impl_generics crate::Lexable for #name #ty_generics #where_clause {
            fn lex(input: &mut ::std::iter::Peekable<impl Iterator<Item = char>>) -> Result<Option<Self>, crate::LexError> {
                let mut current_text = String::new();

                Ok(None)
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
