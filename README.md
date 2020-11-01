# lexpr: S-expressions for Rust [![Build Status]][gh-actions] [![Codecov]][codecov] [![Rustc Version 1.32+]][rustc]

[Build Status]: https://github.com/rotty/lexpr-rs/workflows/CI/badge.svg
[gh-actions]: https://github.com/rotty/lexpr-rs/actions
[codecov]: https://codecov.io/github/rotty/lexpr-rs/coverage.svg?branch=master
[Rustc Version 1.32+]: https://img.shields.io/badge/rustc-1.32+-lightgray.svg
[rustc]: https://blog.rust-lang.org/2019/01/17/Rust-1.32.0.html

This repository hosts the following crates:

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
the following projects, all licensed under the same conditions,
i.e. dual-licensed under MIT or Apache-2.0 license:

- [`serde_json`](https://github.com/serde-rs/json)
- [`serde_yaml`](https://github.com/dtolnay/serde-yaml)
- [`sexpr`](https://github.com/zv/sexpr), Copyright 2017 Zephyr
  Pellerin.
