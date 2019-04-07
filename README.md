# lexpr: S-expressions for Rust [![Build Status]][travis] [![Codecov]][codecov] [![Rustc Version 1.32+]][rustc]

[Build Status]: https://api.travis-ci.org/rotty/lexpr-rs.svg?branch=master
[travis]: https://travis-ci.org/rotty/lexpr-rs
[codecov]: https://codecov.io/github/rotty/lexpr-rs/coverage.svg?branch=master
[rustc]: https://blog.rust-lang.org/2019/01/17/Rust-1.32.0.html

This repositories hosts the following crates:

- [`lexpr`](./lexpr), providing the `lexpr::Value` type for
  representing S-expression data, the `sexp` macro to embed
  S-expression values into Rust code, as well as a parser and printer
  for S-expressions. Please refer to crate's
  [`README`](./lexpr/README.md) for more information.

- [`serde-lexpr`](./serde-lexpr), integrating `lexpr` with Serde,
  allowing to serialize and deserialize Rust data structures to and
  from S-expressions.

- [`lexpr-macros`](./lexpr-macros), an internal crate actually
  implementing the `sexp` macro. This crate should be considered an
  implementation detail, and not be used directly, only via the
  `lexpr` crate.

## Licensing

The code and documentation in the `lexpr-rs` git repository is [free
software](https://www.gnu.org/philosophy/free-sw.html), dual-licensed
under the [MIT](./LICENSE-MIT) or [Apache-2.0](./LICENSE-APACHE)
license, at your choosing.

The `lexpr` repository contains code and documentation adapted from
the following projects:

- [`serde_json`](https://github.com/serde-rs/json), also dual-licensed
  under MIT/Apache-2.0 licenses.
- [`sexpr`](https://github.com/zv/sexpr), Copyright 2017 Zephyr
  Pellerin, dual-licensed under the same licenses.
