use super::*;
use crate::Value;

use std::io::Cursor;

#[test]
fn test_atoms_default() {
    let mut parser =
        Parser::from_str("foo-symbol :prefix-keyword postfix-keyword: #t #f #nil 100 -42 4.5");
    for value in vec![
        Value::symbol("foo-symbol"),
        Value::symbol(":prefix-keyword"),
        Value::symbol("postfix-keyword:"),
        Value::from(true),
        Value::from(false),
        Value::Nil,
        Value::from(100),
        Value::from(-42),
        Value::from(4.5),
    ] {
        assert_eq!(parser.parse().unwrap(), Some(value));
    }
    parser.end().unwrap();
}

#[test]
fn test_atom_failures() {
    assert!(from_str_custom("#:octothorpe-keyword", Options::new()).is_err());
}

#[test]
fn test_chars_default() {
    for &c in &['x', 'y', 'z', '\u{203D}', ' '] {
        assert_eq!(from_str(&format!("#\\{}", c)).unwrap(), Value::Char(c));
    }
    for &(name, code) in &[
        ("nul", 0x00),
        ("alarm", 0x07),
        ("backspace", 0x08),
        ("tab", 0x09),
        ("linefeed", 0x0A),
        ("newline", 0x0A),
        ("vtab", 0x0B),
        ("page", 0x0C),
        ("return", 0x0D),
        ("esc", 0x1B),
        ("space", 0x20),
        ("delete", 0x7F),
    ] {
        assert_eq!(
            from_str(&format!("#\\{}", name)).unwrap(),
            Value::Char(char::from(code))
        );
    }
}

// This is generic over the parser to allow testing both the slice-based and
// I/O-based `Read` trait implementations.
fn check_strings_default<F>(parse: F)
where
    F: Fn(&str) -> Result<Value>,
{
    assert_eq!(
        parse(r#""A plain string""#).unwrap(),
        Value::string("A plain string")
    );
    assert_eq!(
        parse(r#""\a\b\t\n\v\f\r\"\\""#).unwrap(),
        Value::string("\x07\x08\t\n\x0B\x0C\r\"\\")
    );
    // Examples taken from R6RS 4.2.4
    assert_eq!(parse(r#""\x41;bc""#).unwrap(), Value::string("Abc"));
    assert_eq!(parse(r#""\x41; bc""#).unwrap(), Value::string("A bc"));
    assert_eq!(parse(r#""\x41bc;""#).unwrap(), Value::string("\u{41BC}"));
    assert!(parse(r#""\x41""#).is_err());
    assert!(parse(r#""\x;"#).is_err());
    assert!(parse(r#""\x41bx;""#).is_err());
    assert_eq!(parse(r#""\x00000041;""#).unwrap(), Value::string("A"));
    assert_eq!(
        parse(r#""\x0010FFFF;""#).unwrap(),
        Value::string("\u{10FFFF}")
    );
    assert!(parse(r#""\x00110000;""#).is_err());
    assert_eq!(parse(r#""\x000000001;""#).unwrap(), Value::string("\u{01}"));
    assert!(parse(r#""\xD800;""#).is_err());

    // Check that u32 overflow is detected
    assert!(parse(r#""\x100000001;""#).is_err());

    // Check that raw control characters are accepted
    let control_chars: String = (0..32).map(char::from).collect();
    assert_eq!(
        parse(&format!("\"{}\"", control_chars)).unwrap(),
        Value::from(control_chars)
    );
}

#[test]
fn test_strings_default() {
    check_strings_default(from_str)
}

#[test]
fn test_strings_io_default() {
    check_strings_default(|input| from_reader(Cursor::new(input.as_bytes())))
}

fn check_strings_elisp<F>(parse: F)
where
    F: Fn(&str) -> Result<Value>,
{
    assert_eq!(
        parse(r#""A plain string""#).unwrap(),
        Value::string("A plain string")
    );
    // Control character sanity check
    assert_eq!(parse(r#""\t\^B""#).unwrap(), Value::string("\t\x01"));

    // Special escapes
    assert_eq!(
        parse(r#""\a\b\t\n\v\f\r\"\\\s\d""#).unwrap(),
        Value::string("\x07\x08\t\n\x0B\x0C\r\"\\ \x7F")
    );

    // Escaped blank will be ignored
    assert_eq!(
        parse(r#""Hello\ World""#).unwrap(),
        Value::string("HelloWorld")
    );

    // Unicode escapes, 4-digit variant
    assert_eq!(
        parse(r#""\u41bcabc""#).unwrap(),
        Value::string("\u{41bc}abc")
    );
    // Unicode escapes, 8-digit variant
    assert_eq!(
        parse(r#""\U0010FFFFABC""#).unwrap(),
        Value::string("\u{10FFFF}ABC")
    );
    // Unicode escapes, "named" variant
    assert_eq!(
        parse(r#""\N{U+10FFFF}\N{U+203D}""#).unwrap(),
        Value::string("\u{10FFFF}\u{203D}")
    );

    // Hex escapes in ASCII range, combined with non-ASCII UTF-8 text leads to a
    // string.
    assert_eq!(
        parse("\"\\x01\\x02Hello World\u{203D}\"").unwrap(),
        Value::string("\u{01}\u{02}Hello World\u{203D}")
    );
    // Octal escapes lead to byte vector
    assert_eq!(
        parse(r#""\001\002\377""#).unwrap(),
        Value::from([1, 2, 255].as_ref())
    );
    // Hexadecimal escapes lead to byte vector
    assert_eq!(
        parse(r#""\x01\x02\xFF""#).unwrap(),
        Value::from([1, 2, 255].as_ref())
    );

    // Mixing non-ASCII single-byte escapes and unicode will result in an error
    assert!(parse(r#""\xFC\N{U+203D}""#).is_err());
}

#[test]
fn test_strings_elisp() {
    check_strings_elisp(from_str_elisp)
}

#[test]
fn test_strings_elisp_io() {
    check_strings_elisp(|input| from_reader_elisp(Cursor::new(input.as_bytes())))
}

#[test]
fn test_keyword_styles() {
    let prefix = Options::new().with_keyword_style(KeywordStyle::ColonPrefix);
    let postfix = Options::new().with_keyword_style(KeywordStyle::ColonPostfix);
    let octothorpe = Options::new().with_keyword_style(KeywordStyle::Octothorpe);

    assert_eq!(
        from_str_custom(":prefix", prefix).unwrap(),
        Value::keyword("prefix")
    );
    assert_eq!(
        from_str_custom("postfix:", postfix).unwrap(),
        Value::keyword("postfix")
    );
    assert_eq!(
        from_str_custom("#:octothorpe", octothorpe).unwrap(),
        Value::keyword("octothorpe")
    );
}

#[test]
fn test_lists_default() {
    assert_eq!(from_str("()").unwrap(), Value::Null);
    assert_eq!(
        from_str("(hello)").unwrap(),
        Value::list(vec![Value::symbol("hello")])
    );
    assert_eq!(
        from_str("(1 . (2 . (3 . ())))").unwrap(),
        Value::list(vec![1u32, 2, 3])
    );
    assert_eq!(
        from_str("(1 . 2)").unwrap(),
        Value::append(vec![Value::from(1)], Value::from(2))
    );
    assert_eq!(
        from_str("(1 hello . 2)").unwrap(),
        Value::append(vec![Value::from(1), Value::symbol("hello")], Value::from(2))
    );
}

#[test]
fn test_broken_lists_default() {
    assert!(from_str("(.)").is_err());
    assert!(from_str("(1 2 .)").is_err());
    assert!(from_str("(1 2 . 3 4)").is_err());
    assert!(from_str("(1 2 . 3 . 4)").is_err());
}

#[test]
fn test_list_nil_default() {
    assert_eq!(
        from_str("(1 . nil)").unwrap(),
        Value::append(vec![1], Value::symbol("nil"))
    );
    assert_eq!(
        from_str("(nil)").unwrap(),
        Value::list(vec![Value::symbol("nil")])
    );
}

#[test]
fn test_list_elisp() {
    let elisp = Options::elisp();
    assert_eq!(
        from_str_custom("(1 . nil)", elisp.clone()).unwrap(),
        Value::list(vec![1]),
    );
    assert_eq!(
        from_str_custom("(nil)", elisp.clone()).unwrap(),
        Value::list(vec![Value::Null])
    );
}

#[test]
fn test_byte_vectors() {
    assert_eq!(from_str("#u8(1 2 3)").unwrap(), Value::from(vec![1, 2, 3]));
    for input in &["#u8(0 256 3)", "#u8(0.0 1 2)", "#u8(test 1 2)"] {
        assert!(from_str(input).is_err());
    }
}
