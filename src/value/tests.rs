use crate::{Cons, Number, Value};

type Predicate = fn(&Value) -> bool;

// Type predicates for disjoint types
static TYPE_PREDICATES: &[(&'static str, Predicate)] = &[
    ("string", Value::is_string),
    ("symbol", Value::is_symbol),
    ("keyword", Value::is_keyword),
    ("nil", Value::is_nil),
    ("number", Value::is_number),
    ("list", Value::is_list),
];

fn check_type_predicates(value: &Value, type_name: &str) {
    let predicate = TYPE_PREDICATES
        .iter()
        .find(|(name, _)| *name == type_name)
        .map(|(_, p)| p)
        .unwrap();
    assert!(
        predicate(value),
        "{} type predicate returned false: {:?}",
        type_name,
        value
    );
    for (name, predicate) in TYPE_PREDICATES
        .iter()
        .filter(|(name, _)| *name != type_name)
    {
        assert!(
            !predicate(value),
            "{} type predicate returned true for {}: {:?}",
            name,
            type_name,
            value
        );
    }
}

#[test]
fn test_strings() {
    let s = Value::from("hello");
    check_type_predicates(&s, "string");
    assert_eq!(s.as_str(), Some("hello"));
    assert_eq!(s.as_name(), Some("hello"));
    assert_eq!(s, String::from("hello"));
}

#[test]
fn test_symbols() {
    let sym = Value::symbol("a-symbol");
    assert!(sym.is_symbol());
    check_type_predicates(&sym, "symbol");
    assert_eq!(sym.as_symbol(), Some("a-symbol"));
    assert_eq!(sym.as_name(), Some("a-symbol"));
    assert_eq!(sym, Value::symbol("a-symbol"));
}

#[test]
fn test_keywords() {
    let kw = Value::keyword("a-keyword");
    check_type_predicates(&kw, "keyword");
    assert_eq!(kw.as_keyword(), Some("a-keyword"));
    assert_eq!(kw.as_name(), Some("a-keyword"));
    assert_eq!(kw, Value::keyword("a-keyword"));
}

#[test]
fn test_numbers() {
    for n in &[
        Number::from(-123),
        Number::from(0),
        Number::from(1),
        Number::from(1001),
    ] {
        let n_value = Value::from(n.clone());
        check_type_predicates(&n_value, "number");
        assert_eq!(n_value.as_number(), Some(n));
        assert_eq!(n_value.as_name(), None);
    }
}

#[test]
fn test_lists() {
    for elts in &[
        vec![],
        vec![Value::symbol("singleton")],
        vec![Value::from(1), Value::from(2)],
        vec![Value::symbol("answer"), Value::from(42)],
    ] {
        let l = Value::list(elts.clone());
        check_type_predicates(&l, "list");
        assert_eq!(l.to_vec(), Some(elts.clone()));
    }
}

#[test]
fn test_dotted_lists() {
    for (elts, rest) in &[
        (vec![Value::from(1), Value::from(2)], Value::from(3)),
        (vec![Value::symbol("answer")], Value::from(42)),
    ] {
        let l = Value::append(elts.clone(), rest.clone());
        assert!(l.is_dotted_list());
        assert_eq!(
            l.as_cons().map(Cons::to_vec),
            Some((elts.clone(), rest.clone()))
        );
    }
}
