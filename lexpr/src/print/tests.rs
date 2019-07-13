#![cfg_attr(tarpaulin, skip)]

use super::*;
use crate::print;

#[test]
fn test_options_size() {
    // Parser options should fit in 2 machine words on 32-bit architectures.
    assert!(std::mem::size_of::<Options>() <= std::mem::size_of::<u32>() * 2);
}

#[test]
fn test_keyword_syntax() {
    let keyword = Value::keyword("hello-world");
    for (syntax, expected) in &[
        (KeywordSyntax::Octothorpe, "#:hello-world"),
        (KeywordSyntax::ColonPrefix, ":hello-world"),
        (KeywordSyntax::ColonPostfix, "hello-world:"),
    ] {
        let options = Options::default().with_keyword_syntax(*syntax);
        let printed = to_string_custom(&keyword, options).unwrap();
        assert_eq!(printed, *expected);
    }
}

#[test]
fn test_str_escapes_default() {
    // Only the R7RS memnonic shorthands are used, other control characters are
    // represented as `\xNN;`.
    assert_eq!(
        to_string(&Value::string(
            "\u{07}\u{08}\u{09}\n\u{0b}\u{0c}\r\u{0e}\u{0f}\u{10}"
        ))
        .unwrap(),
        r#""\a\b\t\n\x0B;\x0C;\r\x0E;\x0F;\x10;""#
    );
}

#[test]
fn test_str_escapes_elisp() {
    // Not all memnonic shorthands are supported, unsupported or control
    // characters without shorthand are represented as `\uNNNN` to ensure the
    // resulting string gets interpreted as Emacs Lisp multibyte string.
    assert_eq!(
        to_string_custom(
            &Value::string("\u{07}\u{08}\u{09}\n\u{0b}\u{0c}\r\u{0e}\u{0f}\u{10}"),
            print::Options::elisp()
        )
        .unwrap(),
        r#""\a\b\t\n\u000B\u000C\r\u000E\u000F\u0010""#
    );
}
