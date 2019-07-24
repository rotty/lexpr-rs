# Emacs Lisp Strings

Strings in Emacs Lisp are somewhat difficult to deal with, for the
following reasons:

- They can be either "unibyte" strings, which correspond to byte
  vectors in Scheme, and "multibyte" strings, which can handle
  unicode. Whether a string is considered unibyte or multibyte depends
  on its contents; see Section 2.3.8.2, "Non-ASCII Characters in
  Strings" in the Emacs Lisp manual for details.

- Whether a string is considered unibyte or multibyte not only depends
  on its contents, but also the source it is read from.

- A multibyte string can include characters outside of the unicode
  codepoint range. This happens for instance when the string includes
  a hexadecimal or octal escape interpreted as a single byte,
  potentially violating the encoding rules of the multibyte source.

- Emacs Lisp string syntax supports a multitude of escaping modes,
  some of which originate from representing keyboard event sequences
  in strings. Using these "keyboard-oriented" escapes inside strings
  is explicitly discouraged in the Emacs Lisp manual.

The way `lexpr` deals with this complexity is the following:

- The input source is always considered to be "multibyte" using the
  UTF-8 encoding; other encodings are not supported.

- Mixing non-ASCII UTF-8 characters, either directly part of the input
  or represented using escape sequences, and hexadecimal or octal
  escape sequences resulting in a single byte outside of the ASCII
  range will result in a parse error. For instance, the following
  string cannot be parsed by `lexpr`:

  `"\xFC\N{U+203D}"`

  Emacs, however, would parse this as a string containing the
  "character" sequence `#x3ffffc`, `#x203d`. Note that the first
  "character" is not a valid unicode codepoint.

- Strings containing only ASCII characters and at least one
  single-byte hexadecimal or octal escape will be parsed as byte
  vectors instead of strings. This mirrors the Emacs Lisp rules for
  when a string will be considered to be "unibyte".

  When producing S-expression text, byte vectors will always be
  represented as a sequence of octal-escaped bytes.

- The escaping styles supported by `lexpr` are:

  - Hexadecimal (`\xN...`) and octal (`\N...`)
  - Unicode (`\uNNNN`, `\U00NNNNNN`)
  - Named unicode (`\N{U+X...}`). Note that the syntax that refers to
    codepoints using their full name (e.g. `\N{LATIN SMALL LETTER A
    WITH GRAVE}`) is deliberately not supported.

It is expected that these restrictions will not be an impediment when
using S-expressions as a data exchange format between Emacs Lisp and
Rust programs. In short, S-expressions produced by Rust should be
always be parsable by Emacs, and the other direction should work as
long as there are no strings with non-unicode "characters" are
involved.
