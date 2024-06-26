# lexpr: S-expressions for Rust [![Latest Version]][crates.io] [![Rustc Version 1.56+]][rustc]

[Latest Version]: https://img.shields.io/crates/v/lexpr.svg
[crates.io]: https://crates.io/crates/lexpr
[Rustc Version 1.56+]: https://img.shields.io/badge/rustc-1.56+-lightgray.svg
[rustc]: https://blog.rust-lang.org/2021/10/21/Rust-1.56.0.html

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

  As the `sexp` macro is implemented as a proc-macro, and may not be
  needed in all cases, it is needs to be enabled explicitly using the
  `sexp-macro` feature flag since `lexpr` version 0.3.0:

  ```toml
  [dependencies]
  lexpr = { version = "0.3", features = ["sexp-macro"] }
  ```

  Note that many of the documentation examples make use of the `sexp`
  macro, without hinting that the feature needs to be enabled
  explicitly.

- Construct and destructure S-expression data using a full-featured
  API:

  ```rust
  use lexpr::Value;

  let names = Value::list(vec!["Alice", "Bob", "Mallory"]);
  println!("The bad guy is {}", names[2].as_str().unwrap());
  ```

- Parse and serialize S-expression data from and to its textual
  representation.

- Use S-expressions as surface syntax for a configuration file format
  or other domain-specific language (DSL). `lexpr` allows obtaining
  source location information for the parsed data, so you can give
  error messages that point to the exact place a problem was detected,
  in case the source was parsed successfully as S-expression data, but
  the parsed data does not meet the requirements of your DSL.

To get a better idea of the direction `lexpr` is headed, you may want
to take at the [TODO](./TODO.md) or the ["why"](./docs/why.md)
document.

## Rust version requirements

`lexpr` is CI-tested on current stable, beta and nightly channels of
Rust. Additionally, it is made sure that the code still compiles on
Rust 1.56.0. However, no tests are run for that build.

## Supported Lisp dialects

Currently, `lexpr` focuses on Scheme, mostly based on R6RS and R7RS
syntax, with some extensions, and Emacs Lisp.

Dialect-specific omissions, both ones that are planned to be fixed in
the future, and deliberate ones, are listed below. If you are missing
a feature that is not yet listed here, please [file an issue]!.

### Scheme

- Support for number syntax is currently quite limited, compared to
  Scheme's numeric tower. Integers (including hexadecimal, octal and
  binary notation) as well as floating point values should work
  though.
- For strings, continuation line syntax (using a trailing slash) is
  not yet implemented.
- Block comments.
- Directives, such as `#!fold-case` and `#!no-fold-case` are not
  implemented. It's not clear if these will be implemented at all.

### Emacs Lisp

- A main area where `lexpr` only supports a subset of Emacs Lisp are
  strings and characters. You can have a look at [the gory
  details](./docs/elisp-strings.md).
- Integer literals with an arbitrary base (radix), are not yet
  supported.

### KiCad

- Since version 0.3.0, reading and writing S-expression files produced
  by recent versions of KiCad should be supported given the new
  `with_leading_digit_symbols` parser option.

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
