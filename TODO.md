# Missing features

- [ ] Text parser
- [ ] Text deserialization and pretty-printing
- [ ] Comments (both line and R6RS multi-line comments)
- [ ] Proper string escape syntax, instead of using JSON's rules
- [ ] Syntactic sugar for quote, quasiquote, unquote and unquote-splicing
- [ ] Support for character atoms
- [ ] Support for vectors and byte vectors

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
