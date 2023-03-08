# serde-lexpr: Serde S-expressions [![Latest Version]][crates.io] [![Rustc Version 1.45+]][rustc]

[Latest Version]: https://img.shields.io/crates/v/serde-lexpr.svg
[crates.io]: https://crates.io/crates/serde-lexpr
[Rustc Version 1.45+]: https://img.shields.io/badge/rustc-1.45+-lightgray.svg
[rustc]: https://blog.rust-lang.org/2020/07/16/Rust-1.45.0.html

```toml
[dependencies]
serde-lexpr = "0.1.0"
```

You may be looking for:

- [API Documentation for master branch](https://rotty.github.io/lexpr-rs/master/serde_lexpr/)

This crate is a Rust library for using the [Serde] serialization
framework with data in [S-expression] format, which are the
human-readable, textual representation of code and data in the Lisp
family of languages.

[Serde]: https://github.com/serde-rs/serde
[S-expression]: https://en.wikipedia.org/wiki/S-expression

This library does not implement an S-expression parser; it is built
upon [`lexpr`], which provides a parser and serializer.

[`lexpr`]: https://github.com/rotty/lexpr-rs/lexpr

## Licensing

The code and documentation in the `lexpr` crate is [free software],
dual-licensed under the [MIT](./LICENSE-MIT) or
[Apache-2.0](./LICENSE-APACHE) license, at your choosing.

[free software]: https://www.gnu.org/philosophy/free-sw.html

The `lexpr` repository contains code and documentation adapted from
the following projects, all licensed under the same conditions as
`serde-lexpr` itself:

- [`serde_json`](https://github.com/serde-rs/json)
- [`serde_yaml`](https://github.com/dtolnay/serde-yaml)
