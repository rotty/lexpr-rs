use crate::value::Value;

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

impl ToTokens for Value {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let expanded = match self {
            Value::Nil => quote! { ::lexpr::Value::Nil },
            Value::Literal(lit) => quote! { ::lexpr::Value::from(#lit) },
            Value::Negated(lit) => quote! { ::lexpr::Value::from(-#lit) },
            Value::Bool(value) => quote! { ::lexpr::Value::from(#value) },
            Value::Symbol(name) => quote! { ::lexpr::Value::symbol(#name) },
            Value::Keyword(name) => quote! { ::lexpr::Value::keyword(#name) },
            Value::Unquoted(tt) => quote! { ::lexpr::Value::from(#tt) },
            Value::UnquoteSplicing(tt) => quote! { ::lexpr::Value::from(#tt) },
            Value::List(elements) => {
                if elements.is_empty() {
                    quote! { ::lexpr::Value::Null }
                } else {
                    quote! { ::lexpr::Value::list(vec![#(#elements),*]) }
                }
            }
            Value::ImproperList(elements, rest) => match &**rest {
                Value::Unquoted(tt) => quote! {{
                    ::lexpr::Value::append(vec![#(#elements),*], ::lexpr::Value::from(#tt))
                }},
                _ => quote! { ::lexpr::Value::append(vec![#(#elements),*], #rest) },
            },
            Value::Vector(elements) => quote! {
                ::lexpr::Value::Vector(vec![#(#elements),*].into())
            },
        };
        tokens.extend(expanded);
    }
}

pub fn generate(value: Value) -> TokenStream {
    value.into_token_stream()
}
