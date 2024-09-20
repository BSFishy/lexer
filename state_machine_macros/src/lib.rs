use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Lexable, attributes(lex))]
pub fn derive_lexable(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let generics = input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

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
