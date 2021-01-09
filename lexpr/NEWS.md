# 0.2.6

This is a maintenance and bugfix release:

- Fix reading close square delimiter when interpreting square brackets
  as list delimiters. Issue reported and fixed by @raffalevy.

# 0.2.5

New features:

- An additional API that allows obtaining source location information
  is now available, thanks to the request of @emoon, who opened the
  very first issue on lexpr. The central point of the new API is the
  new `Datum` type, which combines a `Value` with location information.
- The iterators for cons cell chains now have a `peek` method.
- Number literals with R7RS radix prefixes are now understood. This
  allows for parsing of binary, octal, hexidecimal, and
  explicitly-decimal literals, e.g., `#b10101110`. `#o0777`,
  `#xDEADBEEF` and `#d42`. A sign is allowed to follow the radix
  prefix, as per R7RS formal syntax.

Besides some CI churn, the `quickcheck_macros` dev-dependency has been
updated, which eliminates old versions of `syn` and `quote` from the
transitive development build dependencies.

# 0.2.4

This is dependency-update release:

- The procedural `sexp` macro is now implemented on top of
  `proc-macro2` 1.0 and `quote` 1.0.
- `lexpr` now uses newer versions of `criterion`, `quickcheck` and
  `rand` in its `dev-dependencies`.

In addition, some minor code cleanups have been done, including slight
simplification of some doctest examples.

# 0.2.3

New features:

- Line comments are now recognized (and ignored) by the parser.
- The `Parser` type now implements `Iterator`. This should make
  reading multiple S-expressions from a single source considerably
  more ergonomic.

# 0.2.2

New features:

- The `Value` type now implements `FromStr` using the default syntax.

Parser bugfixes:

- "Peculiar identifiers", i.e. those starting with "+", "-" or ".",
  should now be handled properly.

- Number literals that start with a "+" are no longer erroneously
  parsed as symbols.

- The handling of dots inside lists should now be more robust,
  detecting invalid syntax that was accepted before.

# 0.2.1

Fix version number in README; no other changes.

# 0.2.0

Incompatible changes:

- The `Atom` enum no longer exists. All its variants are now part of
  the `Value` enum itself.
- List values are no longer represented as `List` and `ImproperList`
  variants, but are more faithfully modeled as chains of "cons"
  cells. See the `Value::Cons` variant and the new `Cons` data
  type. Conversion to vectors and iteration over cons cell chains is
  supported.
- The `String`, `Symbol` and `Keyword` variants no longer contain a
  `String`, but a `Box<str>` to reduce the memory footprint of
  `Value`. As in-place modification of these variants is expected to
  be a seldom-required operation, the ergonomic impact is deemed
  acceptable.
- The `Error` type, as it currently only relates to S-expression
  parsing, is now found in the `lexpr::parse` module.

New features:

- Serde support is now available via the companion crate
  [`serde-lexpr`], which is developed in parallel with `lexpr`,
  sharing the same git repository.
- There is now the `Value::Vector` variant for representing Lisp
  vectors. Vectors can be constructed via the `sexp!` macro using
  Scheme syntax, e.g. `sexp!(#(1 2 3))`. Both Emacs Lisp and Scheme
  syntax is supported for reading and printing.
- Characters are now supported with the `Value::Char` variant. Both
  Emacs Lisp and Scheme syntax is supported.
- Byte vectors are now supported with the `Value::Bytes` variant. This
  is supported for Scheme R6RS and R7RS syntax. For Emacs Lisp, this
  data type maps to unibyte strings.
- Escapes sequences in string literals now support the appropriate
  syntax for the selected Lisp dialect, instead of using JSON syntax.
- There is now a `Number::visit` method, which can be used to dispatch
  on the internal type of the number.

[`serde-lexpr`]: https://github.com/rotty/lexpr-rs/tree/master/serde-lexpr

# 0.1.3

Noteworthy changes:

- Initial parsing and printing support.

# 0.1.2

Noteworthy changes:

- New constructors `Value::list()`, `Value::improper_list()`, and
  `Value::empty_list()`.
- New predicates `Value::is_symbol()`, `Value::is_list()`, and
  `Value::is_improper_list()`.
- New accessor `Value::as_symbol()`.
- New conversions using the `From`/`Into` traits:
  - `Value` can now additionally be constructed from `Atom` and `Cow<str>`.
  - `Atom` can now additionally be constructed from `Cow<str>`.
- A few initial, basic API tests added.

# 0.1.1

Noteworthy changes:

- Fix in `Atom::keyword()`
- More accesor functions for `Value` and `Atom`
- Documentation expanded a bit
- We now build on travis-ci

# 0.1.0

Initial release, features:

- `Value` type and `sexp` macro and corresponding documentation.
