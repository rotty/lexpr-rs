#[derive(Debug)]
pub enum Value {
    Nil,
    Literal(proc_macro2::Literal),
    Negated(proc_macro2::Literal),
    Bool(bool),
    Symbol(String),
    Keyword(String),
    Unquoted(proc_macro2::TokenTree),
    List(Vec<Value>),
    ImproperList(Vec<Value>, Box<Value>),
}
