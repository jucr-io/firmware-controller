use proc_macro::TokenStream;
use syn::{parse_macro_input, punctuated::Punctuated, ItemImpl, ItemStruct, Meta, Token};

mod controller;
mod util;

/// TODO:
#[proc_macro_attribute]
pub fn controller(attr: TokenStream, item: TokenStream) -> TokenStream {
    let _args = parse_macro_input!(attr with Punctuated<Meta, Token![,]>::parse_terminated);

    if let Ok(input) = syn::parse::<ItemStruct>(item.clone()) {
        controller::item_struct::expand(input)
            .unwrap_or_else(|e| e.to_compile_error().into())
            .into()
    } else if let Ok(input) = syn::parse::<ItemImpl>(item) {
        controller::item_impl::expand(input)
            .unwrap_or_else(|e| e.to_compile_error().into())
            .into()
    } else {
        panic!("Expected struct or trait")
    }
}
