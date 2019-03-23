# 0.2.0 (unreleased)

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

- There is now a `Number::visit` method, which can be used to dispatch
  on the internal type of the number.
- Serde support is now available via the companion crate
  [`serde-lexpr`], which is developed in parallel with `lexpr`,
  sharing the same git repository.

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
