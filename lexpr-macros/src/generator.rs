use crate::value::{Value, Atom};

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

impl ToTokens for Atom {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let expanded = match self {
            Atom::Nil => quote! { ::lexpr::Atom::Nil },
            Atom::Literal(lit) => quote! { ::lexpr::Atom::from(#lit) },
            Atom::Negated(lit) => quote! { ::lexpr::Atom::from(-#lit) },
            Atom::Bool(value) => quote! { ::lexpr::Atom::from(#value) },
            Atom::Symbol(name) => quote! { ::lexpr::Atom::symbol(#name) },
            Atom::Keyword(name) => quote! { ::lexpr::Atom::keyword(#name) },
            Atom::Unquoted(tt) => quote! { ::lexpr::Atom::from(#tt) },
        };
        tokens.extend(expanded);
    }
}

impl ToTokens for Value {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let expanded = match self {
            Value::Atom(atom) => {
                quote! { ::lexpr::Value::Atom(#atom) }
            }
            Value::List(elements) => {
                quote! { ::lexpr::Value::List(vec![#(#elements),*]) }
            }
            Value::ImproperList(elements, rest) => {
                match rest {
                    Atom::Unquoted(tt) => quote! {{
                        let value = ::lexpr::Value::from(#tt);
                        match value {
                            ::lexpr::Value::Atom(atom) => ::lexpr::Value::ImproperList(vec![#(#elements),*], atom),
                            ::lexpr::Value::ImproperList(rest_elements, rest) => {
                                let mut elements = vec![#(#elements),*];
                                elements.extend(rest_elements);
                                ::lexpr::Value::ImproperList(elements, rest)
                            }
                            ::lexpr::Value::List(rest_elements) => {
                                let mut elements = vec![#(#elements),*];
                                elements.extend(rest_elements);
                                ::lexpr::Value::List(elements)
                            }
                        }
                    }},
                    _ => quote! { ::lexpr::Value::ImproperList(vec![#(#elements),*], #rest) }
                }
            }
        };
        tokens.extend(expanded);
    }
}

pub fn generate(value: Value) -> TokenStream {
    value.into_token_stream()
}
