#![cfg_attr(tarpaulin, skip)]

use super::*;
use crate::parse::error::Category;
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
fn test_comments() {
    let input = r"1 (2 3)
;; A comment
4";
    assert_eq!(
        Parser::from_str(input).collect::<Result<Vec<_>>>().unwrap(),
        vec![1.into(), Value::list(vec![2i32, 3]), 4.into()],
    );
}

#[test]
fn test_symbols() {
    assert!(from_str(".").is_err());
}

#[test]
fn test_atom_failures() {
    assert_eq!(
        from_str_custom("#:octothorpe-keyword", Options::new())
            .err()
            .map(|e| e.classify()),
        Some(Category::Syntax)
    );
}

#[test]
fn test_decimal_numbers() {
    assert_eq!(from_str("42").unwrap(), Value::from(42));
    assert_eq!(from_str("-23").unwrap(), Value::from(-23));
    assert_eq!(from_str("+23").unwrap(), Value::from(23));
    assert_eq!(from_str("0.5e10").unwrap(), Value::from(0.5e10));
    assert_eq!(from_str("-0.5e10").unwrap(), Value::from(-0.5e10));
}

#[test]
fn test_hex_numbers() {
    assert_eq!(from_str("#x42").unwrap(), Value::from(0x42));
    assert_eq!(
        from_str("#xDEADBEEF").unwrap(),
        Value::from(0xDEAD_BEEF_u32)
    );
    assert_eq!(from_str("#x-23").unwrap(), Value::from(-0x23));
    assert_eq!(from_str("#x+23").unwrap(), Value::from(0x23));
}

#[test]
fn test_bin_numbers() {
    assert_eq!(from_str("#b1010").unwrap(), Value::from(10));
    assert_eq!(from_str("#b-100").unwrap(), Value::from(-4));
    assert_eq!(from_str("#b-111").unwrap(), Value::from(-7));
}

#[test]
fn test_oct_numbers() {
    assert_eq!(from_str("#o0777").unwrap(), Value::from(0o777));
    assert_eq!(from_str("#o1764").unwrap(), Value::from(0o1764));
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

#[test]
fn test_chars_elisp() {
    // Regular characters, ASCII and non-ASCII
    for &c in &['x', 'y', 'z', '\u{203D}', ' '] {
        assert_eq!(from_str_elisp(&format!("?{}", c)).unwrap(), Value::Char(c));
    }
    // Memnonic escapes
    for &(name, code) in &[
        ("a", 0x07),
        ("b", 0x08),
        ("t", 0x09),
        ("n", 0x0A),
        ("v", 0x0B),
        ("f", 0x0C),
        ("r", 0x0D),
        ("e", 0x1B),
        ("s", 0x20),
        ("d", 0x7F),
    ] {
        assert_eq!(
            from_str_elisp(&format!("?\\{}", name)).unwrap(),
            Value::Char(char::from(code))
        );
    }
    // Unicode escapes (4 chars)
    for &c in &['a', 'z', '\x7F', '\u{203D}'] {
        assert_eq!(
            from_str_elisp(&format!("?\\u{:04x}", c as u32)).unwrap(),
            Value::Char(c)
        );
    }
    // Unicode escapes (8 chars)
    for &c in &['a', 'z', '\x7F', '\u{203D}', '\u{10FFFF}'] {
        assert_eq!(
            from_str_elisp(&format!("?\\U{:08x}", c as u32)).unwrap(),
            Value::Char(c)
        );
    }
    // "Named" unicode variant
    for &c in &['a', 'z', '\x7F', '\u{203D}', '\u{10FFFF}'] {
        assert_eq!(
            from_str_elisp(&format!("?\\N{{U+{:x}}}", c as u32)).unwrap(),
            Value::Char(c)
        );
    }
    // Hexadecimal escapes
    for &c in &['a', 'z', '\x7F', '\u{203D}', '\u{10FFFF}'] {
        assert_eq!(
            from_str_elisp(&format!("?\\x{:x}", c as u32)).unwrap(),
            Value::Char(c)
        );
    }
    // Octal escapes
    for &c in &['a', 'z', '\x7F', '\u{1FF}'] {
        assert_eq!(
            from_str_elisp(&format!("?\\{:o}", c as u32)).unwrap(),
            Value::Char(c)
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
    assert_eq!(
        parse(r#""\x41""#)
            .err()
            .map(|e| (e.classify(), e.location().is_some())),
        Some((Category::Eof, true))
    );
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
    // Check an escaped vertical bar (pipe) is accepted
    assert_eq!(parse(r#""foo\|bar""#).unwrap(), Value::string("foo|bar"));
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
fn test_keyword_syntaxes() {
    let prefix = Options::new().with_keyword_syntax(KeywordSyntax::ColonPrefix);
    let postfix = Options::new().with_keyword_syntax(KeywordSyntax::ColonPostfix);
    let octothorpe = Options::new().with_keyword_syntax(KeywordSyntax::Octothorpe);

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
fn test_list_peculiar() {
    assert!(from_str("(. foo)").is_err());
    assert!(from_str("(.)").is_err());
    for (peculiar, value) in &[
        ("(-)", Value::list(vec![Value::symbol("-")])),
        ("(-foo)", Value::list(vec![Value::symbol("-foo")])),
        ("(+)", Value::list(vec![Value::symbol("+")])),
        ("(+foo)", Value::list(vec![Value::symbol("+foo")])),
        ("(..)", Value::list(vec![Value::symbol("..")])),
        ("(.foo)", Value::list(vec![Value::symbol(".foo")])),
        (
            "(+ #nil 1)",
            Value::list(vec![Value::symbol("+"), Value::Nil, Value::from(1)]),
        ),
        (
            "(- . #())",
            Value::cons(Value::symbol("-"), Value::Vector(vec![].into())),
        ),
        (
            "(+ . #())",
            Value::cons(Value::symbol("+"), Value::Vector(vec![].into())),
        ),
    ] {
        assert_eq!(&from_str(dbg!(peculiar)).unwrap(), value);
    }
}

#[test]
fn test_list_elisp() {
    let elisp = Options::elisp();
    assert_eq!(
        from_str_custom("(1 . nil)", elisp).unwrap(),
        Value::list(vec![1]),
    );
    assert_eq!(
        from_str_custom("(nil)", elisp).unwrap(),
        Value::list(vec![Value::Null])
    );
}

#[test]
fn test_list_brackets() {
    let options = Options::default().with_brackets(Brackets::List);
    for (input, value) in &[
        ("[]", Value::Null),
        (
            "[1 2 3]",
            Value::list(vec![Value::from(1), Value::from(2), Value::from(3)]),
        ),
    ] {
        assert_eq!(&from_str_custom(input, options).unwrap(), value);
    }
}

#[test]
fn test_vectors_default() {
    assert_eq!(from_str("#()").unwrap(), Value::Vector(vec![].into()));
    assert_eq!(from_str("#(1 2)").unwrap(), Value::vector(vec![1, 2]));
}

#[test]
fn test_vector_peculiar() {
    assert!(from_str("#(. foo)").is_err());
    assert!(from_str("#(.)").is_err());
    for &peculiar in &["-", "-foo", "+", "+foo", "..", ".foo"] {
        assert_eq!(
            from_str(&format!("#({})", peculiar)).unwrap(),
            Value::vector(vec![Value::symbol(peculiar)])
        );
    }
}

#[test]
fn test_vectors_elisp() {
    assert_eq!(from_str_elisp("[]").unwrap(), Value::Vector(vec![].into()));
    assert_eq!(from_str_elisp("[1 2]").unwrap(), Value::vector(vec![1, 2]));
}

#[test]
fn test_byte_vectors() {
    assert_eq!(from_str("#u8(1 2 3)").unwrap(), Value::from(vec![1, 2, 3]));
    for input in &["#u8(0 256 3)", "#u8(0.0 1 2)", "#u8(test 1 2)"] {
        assert!(from_str(input).is_err());
    }
}

#[test]
fn test_options_size() {
    // Parser options should fit in 2 machine words on 32-bit architectures.
    assert!(std::mem::size_of::<Options>() <= std::mem::size_of::<u32>() * 2);
}

#[test]
fn test_parser_iterator() {
    assert_eq!(
        Parser::from_str("1 (2 3) 4")
            .collect::<Result<Vec<_>>>()
            .unwrap(),
        vec![1.into(), Value::list(vec![2i32, 3]), 4.into()],
    );
}

#[test]
fn test_quote_shorthand() {
    assert_eq!(
        from_str("'1").unwrap(),
        Value::list(vec![Value::symbol("quote"), Value::from(1)])
    );
    assert_eq!(
        from_str("'(1 2 3)").unwrap(),
        Value::list(vec![Value::symbol("quote"), Value::list(vec![1, 2, 3])])
    );
}

#[test]
fn test_quasiquote_shorthand() {
    assert_eq!(
        from_str("`1").unwrap(),
        Value::list(vec![Value::symbol("quasiquote"), Value::from(1)])
    );
    assert_eq!(
        from_str("`(1 2 3)").unwrap(),
        Value::list(vec![
            Value::symbol("quasiquote"),
            Value::list(vec![1, 2, 3])
        ])
    );
}

#[test]
fn test_unquote_shorthand() {
    assert_eq!(
        from_str(",1").unwrap(),
        Value::list(vec![Value::symbol("unquote"), Value::from(1)])
    );
    assert_eq!(
        from_str(",(1 2 3)").unwrap(),
        Value::list(vec![Value::symbol("unquote"), Value::list(vec![1, 2, 3])])
    );
}

#[test]
fn test_unquote_splicing_shorthand() {
    assert_eq!(
        from_str(",@(1 2 3)").unwrap(),
        Value::list(vec![
            Value::symbol("unquote-splicing"),
            Value::list(vec![1, 2, 3])
        ])
    );
}

#[test]
fn test_atoms_location_info() {
    let mut parser = Parser::from_str(
        "foo-symbol :prefix-keyword postfix-keyword:\n\
                          #t #f #nil\n\
                          100 -42 4.5",
    );
    for (value, start, end) in vec![
        (Value::symbol("foo-symbol"), (1, 0), (1, 10)),
        (Value::symbol(":prefix-keyword"), (1, 11), (1, 26)),
        (Value::symbol("postfix-keyword:"), (1, 27), (1, 43)),
        (Value::from(true), (2, 0), (2, 2)),
        (Value::from(false), (2, 3), (2, 5)),
        (Value::Nil, (2, 6), (2, 10)),
        (Value::from(100), (3, 0), (3, 3)),
        (Value::from(-42), (3, 4), (3, 7)),
        (Value::from(4.5), (3, 8), (3, 11)),
    ] {
        let datum = parser.expect_datum().expect("parse error");
        assert_eq!(datum.value(), &value);
        assert_eq!(datum.span(), make_span(start, end));
    }
    parser.end().unwrap();
}

#[test]
fn test_vector_location_info() {
    #[rustfmt::skip]
    let mut parser = Parser::from_str(
        "#(1 2 3) #(4 foo)\n\
         #(a b) #()",
    );

    for (value, start, end, inner) in vec![
        (
            Value::vector(vec![1, 2, 3]),
            (1, 0),
            (1, 8),
            vec![((1, 2), (1, 3)), ((1, 4), (1, 5)), ((1, 6), (1, 7))],
        ),
        (
            Value::vector(vec![Value::from(4), Value::symbol("foo")]),
            (1, 9),
            (1, 17),
            vec![((1, 11), (1, 12)), ((1, 13), (1, 16))],
        ),
        (
            Value::vector(vec![Value::symbol("a"), Value::symbol("b")]),
            (2, 0),
            (2, 6),
            vec![((2, 2), (2, 3)), ((2, 4), (2, 5))],
        ),
        (Value::Vector(vec![].into()), (2, 7), (2, 10), vec![]),
    ] {
        let datum = parser.expect_datum().expect("parse error");
        assert_eq!(datum.value(), &value);
        assert_eq!(datum.span(), make_span(start, end));
        for ((start, end), element) in inner.into_iter().zip(datum.vector_iter().unwrap()) {
            assert_eq!(make_span(start, end), element.span());
        }
    }
    parser.end().unwrap();
}

#[test]
fn test_list_location_info() {
    #[rustfmt::skip]
    let mut parser = Parser::from_str(
        "(1 2 3) (4 .foo)\n\
         (a b) ()",
    );

    for (value, start, end, inner) in vec![
        (
            Value::list(vec![1, 2, 3]),
            (1, 0),
            (1, 7),
            vec![((1, 1), (1, 2)), ((1, 3), (1, 4)), ((1, 5), (1, 6))],
        ),
        (
            Value::list(vec![Value::from(4), Value::symbol(".foo")]),
            (1, 8),
            (1, 16),
            vec![((1, 9), (1, 10)), ((1, 11), (1, 15))],
        ),
        (
            Value::list(vec![Value::symbol("a"), Value::symbol("b")]),
            (2, 0),
            (2, 5),
            vec![((2, 1), (2, 2)), ((2, 3), (2, 4))],
        ),
        (Value::Null, (2, 6), (2, 8), vec![]),
    ] {
        let datum = parser.expect_datum().expect("parse error");
        assert_eq!(datum.value(), &value);
        assert_eq!(datum.span(), make_span(start, end));
        let mut items = datum.list_iter().unwrap();
        for ((start, end), element) in inner.into_iter().zip(items.by_ref()) {
            assert_eq!(make_span(start, end), element.span());
        }
        assert!(items.is_empty());
    }
    parser.end().unwrap();
}

#[test]
fn test_dotted_list_location_info() {
    let mut parser = Parser::from_str("(1 2 . 3)");
    let datum = parser.expect_datum().expect("parse error");
    parser.end().unwrap();
    assert_eq!(datum.value(), &Value::append(vec![1, 2], 3));
    assert_eq!(datum.span(), make_span((1, 0), (1, 9)));
    let mut items = datum.list_iter().unwrap();
    for (&(start, end), element) in [(1, 2), (3, 4)].iter().zip(items.by_ref()) {
        assert_eq!(make_span((1, start), (1, end)), element.span());
    }
    for (&(start, end), element) in [(7, 8)].iter().zip(items.by_ref()) {
        assert_eq!(make_span((1, start), (1, end)), element.span());
    }
}

#[test]
fn test_quoted_location_info() {
    let mut parser = Parser::from_str("`(foo ,@bar ,baz 5)");
    let datum = parser.expect_datum().expect("parse error");
    parser.end().unwrap();
    assert_eq!(
        datum.value(),
        &Value::list(vec![
            Value::symbol("quasiquote"),
            Value::list(vec![
                Value::symbol("foo"),
                Value::list(vec![
                    Value::symbol("unquote-splicing"),
                    Value::symbol("bar")
                ]),
                Value::list(vec![Value::symbol("unquote"), Value::symbol("baz")]),
                Value::from(5),
            ])
        ])
    );
    assert_eq!(datum.span(), make_span((1, 0), (1, 19)));
    let mut items = datum.list_iter().unwrap();
    for (&(start, end), element) in [(0, 1), (1, 19)].iter().zip(items.by_ref()) {
        assert_eq!(make_span((1, start), (1, end)), element.span());
    }
}

fn make_span(
    (start_line, start_column): (usize, usize),
    (end_line, end_column): (usize, usize),
) -> Span {
    Span::new(
        Position::new(start_line, start_column),
        Position::new(end_line, end_column),
    )
}
