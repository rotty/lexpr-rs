# Missing features

- [X] Proper string escape syntax, instead of using JSON's rules
- [X] Serde support
- [X] Syntactic sugar for quote, quasiquote, unquote and unquote-splicing
- [X] Support for characters
- [X] Support for vectors
- [X] Support for byte vectors
- [ ] Pretty-printing
- [ ] Implement `VectorSyntax::Parens` (see #116).

## The `sexp` Macro

- [ ] Quote syntactic sugar
- [ ] Unquote-splicing
- [ ] Improve error reporting

## Lisp dialects

The aim of `lexpr` is to provide functionality making S-expressions
usable for domain-specific languages (including configuration files),
data storage, and data exchange with Lisp implementations (in this
order of priority). As such, it does not aim to fully and faithfully
capture the formal syntax of any Lisp dialect in the world, but
instead provide a customizable parser that can consume a large-enough
subset of a given (supported) Lisp dialect to be useful for data
exchange, or parsing configuration files that can (optionally) also be
consumed by a targeted Lisp dialect.

### Scheme (R6RS, R7RS, Guile/Racket extensions)

Partially supported, see the documentation for details.

- [X] Different numeric bases
- [ ] NaNs and infinities
- [ ] Scheme numeric tower (complex numbers, rationals, bignums)
- [ ] R7RS multi-line comments
- [ ] R7RS "datum comments"

### Emacs Lisp

Partially supported.

- [ ] Trailing dot for integers
- [ ] Arbitrary radixes (`#RADIXrINTEGER`)
- [ ] NaNs and infinities

### Common Lisp

Very minimal support, but the most basic stuff should work, mostly as
a result of Emacs Lisp and Common Lisp having a common subset.

- [ ] Options to enable case conversion

## Architectural considerations

- [ ] Split off the low-level part of the parser into its own module
  (or even crate), using an event-based API. This should allow for
  efficient implementation of the Serde deserializer. See
  [`yaml_rust::parser::Parser`] for an example and [`serde_yaml`] for
  an example of how that might look like. A reason for splitting this
  functionality into its own crate might be that the functionality can
  the re-used for, say, a Lisp implementation which does not use
  `lexpr::Value` as data representation, and hence does not need all
  the related code in `lexpr`. The same reasoning might apply to the
  printing API.

  It might be a good idea to code some benchmarks before doing the
  restructuring to ensure it doesn't affect performance too much --
  let's put that "zero-cost abstractions" concept to the test!

  [`yaml_rust::parser::Parser`]: https://docs.rs/yaml-rust/*/yaml_rust/parser/struct.Parser.html
  [`serde_yaml`]: https://github.com/dtolnay/serde-yaml/blob/master/src/de.rs
