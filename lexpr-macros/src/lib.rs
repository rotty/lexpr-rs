//! Internal crate implementing macros exposed by the `lexpr` crate.

#![recursion_limit = "128"]
#![warn(rust_2018_idioms)]

mod generator;
mod parser;
mod value;

use proc_macro2::TokenStream;
use quote::quote;

#[proc_macro]
pub fn sexp(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let output = match expand(TokenStream::from(input)) {
        Err(e) => {
            let msg = format!("could not parse s-expression: {:?}", e);
            quote! { compile_error!(#msg) }
        }
        Ok(output) => output,
    };

    proc_macro::TokenStream::from(output)
}

fn expand(input: TokenStream) -> Result<TokenStream, parser::ParseError> {
    let value = parser::parse(input)?;
    Ok(generator::generate(value))
}
