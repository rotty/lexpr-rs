use lexpr::{sexp, Value};

fn check_roundtrip(input: Value, printed: &str) {
    let string = lexpr::to_string(&input).expect("printing failed");
    assert_eq!(&string, printed);
    let output = lexpr::from_str(&string).expect("parsing failed");
    assert_eq!(input, output);
}

#[test]
fn test_number() {
    check_roundtrip(sexp!(1.5), "1.5");
    check_roundtrip(sexp!(-1.0015065576612683), "-1.0015065576612683");
}

#[test]
fn test_symbol() {
    check_roundtrip(sexp!(#"$?:!"), "$?:!");
}

#[test]
fn test_keyword() {
    check_roundtrip(sexp!(#:foo), "#:foo");
    check_roundtrip(sexp!(#:"kebab-keyword"), "#:kebab-keyword");
}

#[test]
fn test_improper_lists() {
    let value = sexp!((((((#:"$?:!" . #"a-symbol") . #"$?:!") . "") . #f) . #nil));
    dbg!(lexpr::to_string(&value));

    check_roundtrip(sexp!((((#nil . #"$?:!")))), "(((#nil . $?:!)))");

    //List([List([ImproperList([Atom(Number(Float(-1.0015065576612683)))], Bool(false))])])
    //check_roundtrip(sexp!((((-1.0015065576612683))) . #f))])))
}
