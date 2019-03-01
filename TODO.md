# Missing features

- [ ] Comments (both line and R6RS multi-line comments)
- [ ] Proper string escape syntax, instead of using JSON's rules
- [ ] Serde support
- [ ] Syntactic sugar for quote, quasiquote, unquote and unquote-splicing
- [ ] Support for character atoms
- [ ] Support for vectors and byte vectors
- [ ] Pretty-printing

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

## The `sexp` Macro

- [ ] Quote syntactic sugar
- [ ] Unquote-splicing
- [ ] Improve error reporting

## Documentation

- [ ] Atom docs
- [ ] Mention differences from `serde_json`

## Lisp dialects

- [ ] Scheme (R6RS, R7RS, Guile/Racket extensions)
- [ ] Emacs Lisp
- [ ] Common Lisp

## Numbers

- [ ] Complete support for number syntax (e.g., different bases)
- [ ] Scheme numeric tower (complex numbers, rationals, bignums)
- [ ] NaNs and infinities (fixup `from` implementations vs. `from_f64`)
