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
