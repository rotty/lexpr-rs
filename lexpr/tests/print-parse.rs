use lexpr::{parse, print, Value};
use lexpr_macros::sexp;

fn check_roundtrip_default(input: Value, printed: &str) {
    let string = lexpr::to_string(&input).expect("printing failed");
    assert_eq!(&string, printed);
    let output = lexpr::from_str(&string).expect("parsing failed");
    assert_eq!(input, output);
}

fn check_roundtrip_elisp(input: Value, printed: &str) {
    let string = lexpr::to_string_custom(&input, print::Options::elisp()).expect("printing failed");
    assert_eq!(&string, printed);
    let output = lexpr::from_str_custom(&string, parse::Options::elisp()).expect("parsing failed");
    assert_eq!(input, output);
}

#[test]
fn test_number() {
    check_roundtrip_default(sexp!(1.5), "1.5");
    if !cfg!(feature = "fast-float-parsing") {
        check_roundtrip_default(sexp!(-1.0015065576612683), "-1.0015065576612683");
        check_roundtrip_default(sexp!(-1.360438755021694e308), "-1.360438755021694e308");
    }
}

#[test]
fn test_symbol() {
    check_roundtrip_default(sexp!(#"$?:!"), "$?:!");
}

static SPECIAL_INITIALS: &str = "!$%&*/:<=>?@^_~";

#[test]
fn test_special_symbols() {
    for initial in SPECIAL_INITIALS.chars() {
        let s = initial.to_string();
        check_roundtrip_default(Value::symbol(s.as_ref()), &s);
    }
}

#[test]
fn test_peculiar_symbols() {
    for &peculiar in &["+", "+foo", "-", "-foo", "..", ".foo"] {
        check_roundtrip_default(Value::symbol(peculiar), peculiar);
    }
}

#[test]
fn test_keyword_default() {
    check_roundtrip_default(sexp!(#:foo), "#:foo");
    check_roundtrip_default(sexp!(#:"kebab-keyword"), "#:kebab-keyword");
}

#[test]
fn test_improper_lists() {
    check_roundtrip_default(sexp!((((#nil . #"$?:!")))), "(((#nil . $?:!)))");
    check_roundtrip_default(
        sexp!((((((42 . #"a-symbol") . #"$?:!") . "") . #f) . #nil)),
        r#"(((((42 . a-symbol) . $?:!) . "") . #f) . #nil)"#,
    );
}

#[test]
fn test_unicode_chars() {
    for c in ['ﬁ', 'ā', 'ł', 'ﬂ'] {
        check_roundtrip_default(Value::Char(c), &format!("#\\x{:x}", u32::from(c)));
        assert_eq!(lexpr::from_str(&format!("#\\{c}")).unwrap(), Value::Char(c));
    }
}

#[test]
fn test_chars_elisp() {
    for (value, printed) in [
        (sexp!('x'), "?x"),
        (sexp!('\\'), "?\\\\"),
        (sexp!('\u{1B}'), "?\\x1b"),
    ] {
        check_roundtrip_elisp(value, printed);
    }
}

#[test]
fn test_strings_elisp() {
    check_roundtrip_elisp(sexp!("\x01\x02\x03\x7F"), r#""\u0001\u0002\u0003\u007F""#);
}

#[test]
fn test_vectors() {
    check_roundtrip_default(sexp!(#()), "#()");
    check_roundtrip_default(sexp!(#(1 2 3 4)), "#(1 2 3 4)");
}

#[test]
fn test_vectors_elisp() {
    check_roundtrip_elisp(sexp!(#()), "[]");
    check_roundtrip_elisp(sexp!(#(1 2 3 4)), "[1 2 3 4]");
}

#[test]
fn test_bytes() {
    check_roundtrip_default(Value::bytes(vec![1, 2, 3]), "#u8(1 2 3)");
}

#[test]
fn test_bytes_elisp() {
    check_roundtrip_elisp(Value::bytes(vec![1, 255, 3]), r#""\001\377\003""#);
}
