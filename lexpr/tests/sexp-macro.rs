use lexpr::Value;
use lexpr_macros::sexp;

#[test]
fn test_numbers() {
    assert_eq!(sexp!(123), Value::from(123));
    assert_eq!(sexp!(123.4), Value::from(123.4));
    assert_eq!(sexp!(-123), Value::from(-123));
    assert_eq!(sexp!(-64.0), Value::from(-64.0));
}

#[test]
fn test_symbols() {
    assert_eq!(sexp!(foo), Value::symbol("foo"));
    assert_eq!(sexp!(#"a-symbol"), Value::symbol("a-symbol"));
}

#[test]
fn test_keywords() {
    assert_eq!(sexp!(#:foo), Value::keyword("foo"));
    assert_eq!(sexp!(#:"a-keyword"), Value::keyword("a-keyword"));
    assert_eq!(sexp!(:foo), Value::keyword("foo"));
    assert_eq!(sexp!(:"a-keyword"), Value::keyword("a-keyword"));
}

#[test]
fn test_cons() {
    assert_eq!(sexp! {(1 . 2)}, Value::cons(1, 2));
    assert_eq!(
        sexp! {(a . 64)},
        Value::cons(Value::symbol("a"), Value::from(64))
    );
    assert_eq!(
        sexp! {(a . -64)},
        Value::cons(Value::symbol("a"), Value::from(-64))
    );
    assert_eq!(
        sexp! {((a . 256.0))},
        Value::list(vec![Value::cons(Value::symbol("a"), Value::from(256.0))])
    );
    assert_eq!(
        sexp!((#:foo)),
        Value::cons(Value::keyword("foo"), Value::Null)
    );
    assert_eq!(
        sexp!((:foo)),
        Value::cons(Value::keyword("foo"), Value::Null)
    );
}

#[test]
fn test_dotted_list() {
    assert_eq!(
        sexp! {(1 2 3 . tail)},
        Value::append([1, 2, 3].map(Value::from), Value::symbol("tail"))
    );
    assert_eq!(sexp! {(1 2 3 . (4 5))}, Value::list([1, 2, 3, 4, 5]));
}

#[test]
fn test_unquote() {
    let three = 3;
    assert_eq!(sexp!((1 2 ,three)), Value::list(vec![1, 2, 3]));
    assert_eq!(sexp!((1 2 . ,three)), Value::append(vec![1, 2], 3));

    let big = i64::max_value();
    assert_eq!(
        sexp!((b . ,big)),
        Value::cons(Value::symbol("b"), Value::from(big))
    );
}

#[test]
fn test_special_tokens() {
    assert_eq!(sexp!(-), Value::symbol("-"));
    assert_eq!(sexp!(+), Value::symbol("+"));
    assert_eq!(sexp!(+++), Value::symbol("+++"));
    assert_eq!(sexp!(...), Value::symbol("..."));
    assert_eq!(sexp!(.++), Value::symbol(".++"));
    assert_eq!(sexp!(!$%&*+-./:<=>?@^~), Value::symbol("!$%&*+-./:<=>?@^~"));
    assert_eq!(sexp!(:!$%&*+-./<=>?@^~), Value::symbol(":!$%&*+-./<=>?@^~"));
    assert_eq!(
        sexp!((+ 1 2)),
        Value::list(vec![Value::symbol("+"), 1.into(), 2.into()])
    );
    assert_eq!(
        sexp!((! $ % & * + - / : < = > ? @ ^ _ ~)),
        Value::list(
            ["!", "$", "%", "&", "*", "+", "-", "/", ":", "<", "=", ">", "?", "@", "^", "_", "~"]
                .iter()
                .map(|name| Value::symbol(*name))
        )
    );
}
