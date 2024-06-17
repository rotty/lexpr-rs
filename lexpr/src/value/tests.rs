#![cfg_attr(tarpaulin, skip)]

use std::iter;

use crate::{Cons, Number, Value};

type Predicate = fn(&Value) -> bool;

// Type predicates for disjoint types
static TYPE_PREDICATES: &[(&str, Predicate)] = &[
    ("string", Value::is_string),
    ("symbol", Value::is_symbol),
    ("keyword", Value::is_keyword),
    ("nil", Value::is_nil),
    ("number", Value::is_number),
    ("char", Value::is_char),
    ("list", Value::is_list),
    ("vector", Value::is_vector),
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
fn test_chars() {
    for &c in &['x', '\u{203D}', '\u{10FFFF}'] {
        let c_value = Value::from(c);
        check_type_predicates(&c_value, "char");
        assert_eq!(c_value.as_char(), Some(c));
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

#[test]
fn test_vectors() {
    for elts in &[
        vec![],
        vec![Value::symbol("singleton")],
        vec![Value::from(1), Value::from(2)],
        vec![Value::symbol("answer"), Value::from(42)],
    ] {
        let v = Value::vector(elts.clone());
        check_type_predicates(&v, "vector");
        assert_eq!(v.as_slice(), Some(elts.as_slice()));
    }
}

#[test]
fn drop_long_list() {
    let _long = Value::list(iter::repeat(Value::from(42)).take(1_000_000));
}

fn with_deep_tree_on_small_stack(name: &str, func: impl FnOnce(Value) + Send + 'static) {
    // This needs to be large enough to cause stack overflow with a call stack of size
    // `STACK_SIZE`, on all platforms.
    const DEPTH: usize = 20_000;
    // This needs to be small enough to cause stack overflow when dropping a depth of `DEPTH`, on
    // all platforms.  Windows sometimes (usually?) has a default stack size of 1 MB for the main
    // thread, so we choose that.
    const STACK_SIZE: usize = 2_usize.pow(20);

    // Call `func` in a new thread so we can control its stack size.
    let thread = std::thread::Builder::new()
        .name(format!("{}--{}", name, STACK_SIZE))
        .stack_size(STACK_SIZE)
        .spawn(|| {
            let mut deep_tree = Value::Nil;
            for i in 0..DEPTH {
                let mut v = [1, 2, 3].map(Value::from);
                v[i % v.len()] = deep_tree;
                deep_tree = Value::vector(v);
            }
            func(deep_tree);
        })
        .expect("spawn should succeed");

    // Make the test thread block until `func` finishes.
    thread.join().expect("join should succeed");
}

// Expected to crash the test program due to stack overflow in the compiler-added dropping of fields
// via recursive calls.  Exists to demonstrate the problem, when you choose to not ignore this
// test. Unfortunately, we cannot use the `should_panic` attribute instead of `ignore` here, as
// running the test leads to an abort due to stack overflow, instead of a regular panic.
#[test]
#[ignore]
fn test_drop_cause_stack_overflow() {
    with_deep_tree_on_small_stack("test_drop_cause_stack_overflow", drop);
    unreachable!(); // The other thread is expected to crash the program before here.
}

#[cfg(feature = "deep-safe-drop")]
#[test]
fn test_drop_prevent_stack_overflow() {
    use crate::DeepSafeValueDropper;

    with_deep_tree_on_small_stack("test_drop_prevent_stack_overflow", |deep_tree| {
        let wrapped = DeepSafeValueDropper(deep_tree);
        drop(wrapped);
    });
}
