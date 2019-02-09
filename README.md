# lexpr: A S-expression library for Rust

The crate is named `lexpr` (for "Lisp expressions"), as `sexpr` and
several variants thereof are already taken on
[crates.io](https://crates.io). If there are s-expression crates
already out there, you might wonder about the justificiation for
`lexpr`'s existence. The goals of this crate are:

- [x] Provide a data type that can represent a decent subset of the
  various S-expression formats in common use. Put in another way, it
  aims to model a large subset of the union of S-expression formats
  out there, covering:

  - Keywords
  - Symbols
  - Strings
  - Integers (excluding bignums, at least initially)
  - Floats
  - Proper lists
  - Improper lists (excluding circular lists)

- [x] Provide a Rust macro that can be used to embed S-expressions
  into Rust code in a natural way, staying as close to "traditional"
  S-expression representation as possible within the syntactic
  constraints imposed by Rust's macro system.

- [ ] Implement a parser, serializer, and pretty-printer for the
  S-expression value type. This one will not be based on Serde, see
  below why this is not possible while covering the complete universe
  of S-expression values.

  The goal for the parser and serializer goal is to also cover the
  notational differences between the S-expression formats, for example
  allowing for both Guile/Racket keyword notation (`#:foo`) and Emacs
  Lisp notation (`:foo`) by providing parser (or serializer) options.

- [ ] Provide, probably in a companion crate, a
  [Serde](https://serde.rs/) serialization and deserialization
  implementation. Due to the partial misalignment between Serde's data
  model and the S-expression data model, this will probably come with
  syntactic restrictions, but should still allow serializing and
  deserializing arbitrary Rust data types to and from S-expression
  syntax.

## Survey of other crates

The following S-expression crates could be found on
[crates.io](https://crates.io) at the time of writing:


- [sexp](https://crates.io/crates/sexp), last updated in 2016. Seems
  to have a sensible data structure, but lacks:

  - Symbols
  - Keywords
  - Improper lists
  - 64-bit unsigned values
  - A macro for embedding S-expression in Rust
  - Serde support

- [atoms](https://crates.io/crates/atoms), forked from `sexp`, last
  updated in 2017. Points of critique:

  - It lacks keywords.
  - It introduces a distinction between "code" and "data" that is not
    present in the S-expression surface syntax. Quote (`'`) and
    unquote (`````) are just syntactic sugar in a Lisp parser.
  - The macro provided for embedding S-expressions feels heavyweight
    and not close to regular S-expression syntax.
  - No Serde support.

  An interesting feature provided by `atoms` API is a customizable
  representation of symbols. It might be a worthwhile avenue to take
  this further and allow for reference types to represent the contents
  of symbols, keywords and strings. This would allow to avoid copying
  strings, and might make "interning" of symbols possible.

  The API for constructing S-expression values seems something worth
  copying.

- [symbolic_expressions](https://crates.io/crates/symbolic_expressions),
  last updated 2017.

  - Has a very limited data type, just strings, lists and "empty".
  - No embedding macro.
  - No Serde support.

- [asexp](https://crates.io/crates/asexp), last updated 2016, lacks:

  - support for keywords and symbols,
  - an embedding macro,
  - and Serde support.

  This crate actually has reverse dependencies, so providing a similar
  API (or a compatibility layer) may make sense.

- [ess](https://crates.io/crates/ess), last updated 2018, lacks:

  - keyword support,
  - an embedding macro,
  - and Serde support.

  This crate embeds location information in the S-expression data
  type. This is an interesting feature, but not one `lexpr` aims
  for. If support for location information is implemented in `lexpr`,
  it would likely be done as a separate data type.

- [sexpr](https://crates.io/crates/sexpr), last updated 2016. This
  crate is almost empty, and has no documentation whatsoever.

## Inspiration for `lexpr`

In addition to the crates listed above, there is one codebase (not the
one with the same name found on `crates.io`) that implements Serde
support: [sexpr](https://github.com/zv/sexpr). According to its
documentation, its goals are similar to the ones of `lexpr`, and
`lexpr` will be largely based `sexpr`'s ideas, documentation and code,
attempting to rectify the following issues with the `sexpr` codebase:

- The `sexpr` code comes with a lot of documentation, both in the
  README and API docs, but much of the example code is not working, as
  the corresponding implementation is incomplete or missing. The
  `lexpr` crate will only include code examples that are actually
  working, by running them through the Rust compiler via doctests.

- The `sexpr` code is centered around a [serde](https://serde.rs/)
  serializer and deserializer implementation. Serde's data model and
  S-expressions are unfortunatly misaligned in several aspects, and
  hence the Serde mechanism can not be used as a complete S-expression
  parser or serializer.

  To be more specific, it cannot turn the text format as understood by
  various Lisp implementations into a "dynamically typed" (think Rust
  enums) S-expression data type, as the Serde data model has no direct
  mapping for improper lists, symbols, or keywords. In the other
  direction, there is no direct mapping of these "problematic"
  S-expression values to text via the Serde traits.

  However, when using static types (which Serde is primarily designed
  for), these mismatches can be dealt with in a "natural way", e.g. a
  Rust `Map` type can be turned to a Lisp "alist", and the other way
  around.

  The approach `lexpr` will take to Serde support is the following:

  - Provide a fully-featured parser and formatter for S-expressions in
    the base crate, without Serde integration.

  - Implement serde support (possibly in a companion crate) making use
    of parts of the full-featured parser and serializer.

## Licensing

The code and documentation in the `lexpr` git repository is [free
software](https://www.gnu.org/philosophy/free-sw.html), dual-licensed
under the [MIT](./LICENSE-MIT) or [Apache-2.0](./LICENSE-APACHE), at
your choosing.

The `lexpr` repository contains code and documentation adapted from
the following projects:

- [`serde_json`](https://github.com/serde-rs/json), also dual-licensed
  under MIT/Apache-2.0 licenses.
- [`sexpr`](https://github.com/zv/sexpr), Copyright 2017 Zephyr
  Pellerin, dual-licensed under the same licenses.
