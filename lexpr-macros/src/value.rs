#[derive(Debug)]
pub enum Value {
    Atom(Atom),
    List(Vec<Value>),
    ImproperList(Vec<Value>, Atom),
}

#[derive(Debug)]
pub enum Atom {
    Nil,
    Literal(proc_macro2::Literal),
    Negated(proc_macro2::Literal),
    Bool(bool),
    Symbol(String),
    Keyword(String),
    Unquoted(proc_macro2::TokenTree),
}
