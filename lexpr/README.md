# lexpr: S-expressions for Rust [![Latest Version]][crates.io] [![Rustc Version 1.32+]][rustc]

[Latest Version]: https://img.shields.io/crates/v/lexpr.svg
[crates.io]: https://crates.io/crates/lexpr
[Rustc Version 1.32+]: https://img.shields.io/badge/rustc-1.32+-lightgray.svg
[rustc]: https://blog.rust-lang.org/2019/01/17/Rust-1.32.0.html

```toml
[dependencies]
lexpr = "0.2.1"
```

You may be looking for:

- [API Documentation](https://docs.rs/crate/lexpr/)
- [API Documentation for master branch](https://rotty.github.io/lexpr-rs/master/lexpr/)
- [Serde support for S-expressions](https://github.com/rotty/lexpr-rs/serde-lexpr)
- [TODO](./TODO.md)
- [Goals and a survey of other S-expression crates](./docs/why.md)
- [Release Notes](./NEWS.md)

[S-expressions](https://en.wikipedia.org/wiki/S-expression) are the
human-readable, textual representation of code and data in the Lisp
family of languages. `lexpr` aims to provide the tools to:

- Embed S-expression data into Rust programs using the `sexp` macro:

  ```rust
  use lexpr::sexp;

  let address = sexp!(((name . "Jane Doe") (street . "4026 Poe Lane")));
  ```

- Construct and destructure S-expression data using a full-featured
  API:

  ```rust
  use lexpr::Value;

  let names = Value::list(vec!["Alice", "Bob", "Mallory"]);
  println!("The bad guy is {}", names[2].as_str().unwrap());
  ```

- Parse and serialize S-expression data from and to its textual
  representation.

To get a better idea of the direction `lexpr` is headed, you may want
to take at the [TODO](./TODO.md) or the ["why"](./docs/why.md)
document.

## Supported Lisp dialects

Currently, `lexpr` focuses on Scheme, mostly based on R6RS and R7RS
syntax, with some extensions, and Emacs Lisp. The following features,
common across dialects, are not yet implemented:

- Syntactic shorthands for `quote`, `quasiquote`, `unquote` and
  `unquote-splicing`. Again, these are not usually important when
  using S-expressions as a data exchange format.
- Support for number syntax is currently quite limited. Integers and
  floating point values written in decimal notation should work
  though.

Further dialect-specific omissions, both ones that are planned to be
fixed in the future, and deliberate ones, are listed below.

### Scheme

- For strings, continuation line syntax (using a trailing slash) is
  not yet implemented.
- Block comments.
- Directives, such as `#!fold-case` and `#!no-fold-case` are not
  implemented. It's not clear if these will be implemented at all.

### Emacs Lisp

Strings in Emacs Lisp are somewhat difficult to deal with, for the
following reasons:

- They can be either "unibyte" strings, which correspond to byte
  vectors in Scheme, and "multibyte" strings, which can handle
  unicode. Whether a string is considered unibyte or multibyte depends
  on its contents; see Section 2.3.8.2, "Non-ASCII Characters in
  Strings" in the Emacs Lisp manual for details.

- Whether a string is considered unibyte or multibyte not only depends
  on its contents, but also the source it is read from.

- A multibyte string can include characters outside of the unicode
  codepoint range. This happens for instance when the string includes
  a hexadecimal or octal escape interpreted as a single byte,
  potentially violating the encoding rules of the multibyte source.

- Emacs Lisp string syntax supports a multitude of escaping modes,
  some of which originate from representing keyboard event sequences
  in strings. Using these "keyboard-oriented" escapes inside strings
  is explicitly discouraged in the Emacs Lisp manual.

The way `lexpr` deals with this complexity is the following:

- The input source is always considered to be "multibyte" using the
  UTF-8 encoding; other encodings are not supported.

- Mixing non-ASCII UTF-8 characters, either directly part of the input
  or represented using escape sequences, and hexadecimal or octal
  escape sequences resulting in a single byte outside of the ASCII
  range will result in a parse error. For instance, the following
  string cannot be parsed by `lexpr`:

  `"\xFC\N{U+203D}"`

  Emacs, however, would parse this as a string containing the
  "character" sequence `#x3ffffc`, `#x203d`. Note that the first
  "character" is not a valid unicode codepoint.

- Strings containing only ASCII characters and at least one
  single-byte hexadecimal or octal escape will be parsed as byte
  vectors instead of strings. This mirrors the Emacs Lisp rules for
  when a string will be considered to be "unibyte".

  When producing S-expression text, byte vectors will always be
  represented as a sequence of octal-escaped bytes.

- The escaping styles supported by `lexpr` are:

  - Hexadecimal (`\xN...`) and octal (`\N...`)
  - Unicode (`\uNNNN`, `\U00NNNNNN`)
  - Named unicode (`\N{U+X...}`). Note that the syntax that refers to
    codepoints using their full name (e.g. `\N{LATIN SMALL LETTER A
    WITH GRAVE}`) is deliberately not supported.

It is expected that these restrictions will not be an impediment when
using S-expressions as a data exchange format between Emacs Lisp and
Rust programs. In short, S-expressions produced by Rust should be
always be parsable by Emacs, and the other direction should work as
long as there are no strings with non-unicode "characters" are
involved.

## Licensing

The code and documentation in the `lexpr` crate is [free
software](https://www.gnu.org/philosophy/free-sw.html), dual-licensed
under the [MIT](./LICENSE-MIT) or [Apache-2.0](./LICENSE-APACHE)
license, at your choosing.

The `lexpr` repository contains code and documentation adapted from
the following projects:

- [`serde_json`](https://github.com/serde-rs/json), also dual-licensed
  under MIT/Apache-2.0 licenses.
- [`sexpr`](https://github.com/zv/sexpr), Copyright 2017 Zephyr
  Pellerin, dual-licensed under the same licenses.
