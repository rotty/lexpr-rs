#![deny(missing_docs)]
#![warn(rust_2018_idioms)]
#![cfg_attr(docsrs, feature(doc_cfg))]

//! This crate provides facilities for parsing, printing and
//! manipulating S-expression data. S-expressions are the format used
//! to represent code and data in the Lisp language family.
//!
//! ```scheme
//! ((name . "John Doe")
//!  (age . 43)
//!  (address
//!   (street "10 Downing Street")
//!   (city "London"))
//!  (phones "+44 1234567" "+44 2345678"))
//! ```
//!
//! `lexpr` also supports more complex types; including keywords and
//! configurable tokens for `true`, `false` and `nil`, by default
//! using Scheme syntax:
//!
//! ```scheme
//! (define-class rectangle ()
//!  (width
//!    #:init-value #nil ;; Nil value
//!    #:settable #t     ;; true
//!    #:guard (> width 10)
//!  )
//!  (height
//!    #:init-value 10
//!    #:writable #f ;; false
//!   ))
//! ```
//!
//! Note that keywords, and the corresponding `#:` notation, is not
//! part of standard Scheme, but is supported by `lexpr`'s default
//! parser settings.
//!
//! There are three common ways that you might find yourself needing
//! to work with S-expression data in Rust:
//!
//! - **As text data**. An unprocessed string of S-expression data
//!   that you receive from a Lisp program, read from a file, or
//!   prepare to send to a Lisp program.
//!
//! - **As an dynamically typed representation**. Maybe you want to check that
//!   some S-expression data is valid before passing it on, but without knowing
//!   the structure of what it contains. Or you want to handle arbirarily
//!   structured data, like Lisp code.
//!
//! - **As a statically typed Rust data structure**. When you expect all
//!   or most of your data to conform to a particular structure and
//!   want to get real work done without the dynamically typed nature
//!   of S-expressions tripping you up.
//!
//! Only the first two items of this list are handled by `lexpr`; for conversion
//! from and to statically typed Rust data structures see the [`serde-lexpr`]
//! crate.
//!
//! # Operating on dynamically typed S-expression data
//!
//! Any valid S-expression can be manipulated using the [`Value`] data
//! structure.
//!
//! ## Constructing S-expression values
//!
//! ```
//!  use lexpr::{Value, parse::Error};
//!
//! # fn main() -> Result<(), Error> {
//! // Some s-expressions a &str.
//! let data = r#"((name . "John Doe")
//!                (age . 43)
//!                (phones "+44 1234567" "+44 2345678"))"#;
//!
//! // Parse the string of data into lexpr::Value.
//! let v = lexpr::from_str(data)?;
//!
//! // Access parts of the data by indexing with square brackets.
//! println!("Please call {} at the number {}", v["name"], v["phones"][1]);
//!
//! Ok(())
//! # }
//! ```
//!
//! # What are S-expressions?
//!
//! S-expressions, as mentioned above, are the notation used by various dialects
//! of Lisp to represent data (and code). As a data format, it is roughly
//! comparable to JSON (JavaScript Object Notation), but syntactically more
//! lightweight. Also, JSON is designed for consumption and generation by
//! machines, which is reflected by the fact that it does not specify a syntax
//! for comments. S-expressions on the other hand, are intended to be written
//! and read by humans as well as machines. In this respect, they are more like
//! YAML, but have a simpler and less syntactically rigid structure. For
//! example, indentation does not convey any information to the parser, but is
//! used only to allow for easier digestion by humans.
//!
//! Different Lisp dialects have notational differences for some data types, and
//! some may lack specific data types completely. This section tries to give an
//! overview over the different types of values representable by the [`Value`]
//! data type and how it relates to different Lisp dialects. All examples are
//! given in the syntax used in [Guile](https://www.gnu.org/software/guile/)
//! Scheme implementation.
//!
//! The parser and serializer implementation in `lexpr` can be
//! tailored to parse and generate S-expression data in various
//! "dialects" in use by different Lisp variants; the aim is to cover
//! large parts of R6RS and R7RS Scheme with some Guile and Racket
//! extensions, as well as Emacs Lisp.
//!
//! In the following, the S-expression values that are modeled by
//! `lexpr` are introduced, In general, S-expression values can be
//! split into the two categories primitive types and compound types.
//!
//! ## Primitive types
//!
//! Primitive, or non-compound types are those that can not
//! recursively contain arbitrary other values. Numbers,
//! strings and booleans fall into this category.
//!
//! ### Symbols and keywords
//!
//! Lisp has a data type not commonly found in other languages, namely
//! "symbols". A symbol is conceptually similar to identifiers in other
//! languages, but allow for a much richer set of characters than typically
//! allowed for identifiers in other languages. Also, identifiers in other
//! languages can usually not be used in data; Lisps expose them as a
//! primitive data type, a result of the
//! [homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity) of the Lisp
//! language family.
//!
//!
//! ```scheme
//! this-is-a-symbol ; A single symbol, dashes are allowed
//! another.symbol   ; Periods are allowed as well
//! foo$bar!<_>?     ; As are quite a few other characters
//! ```
//!
//! Another data type, present in some Lisp dialects, such as Emacs
//! Lisp, Common Lisp, and several Scheme implementations, are
//! keywords. These are also supported by `lexpr`. Keywords are very
//! similiar to symbols, but are typically prefixed by `:` or `#:` and
//! are used for different purposes in the language.
//!
//! ```lisp
//! #:foo ; A keyword named "foo", written in Guile/Racket notation
//! :bar  ; A keyword named "bar", written in Emacs Lisp or Common Lisp notation
//! ```
//!
//! ### Booleans
//!
//! While Scheme has a primitive boolean data type, more traditional Lisps such
//! as Emacs Lisp and Common Lisp do not; they instead use the symbols `t` and
//! `nil` to represent boolean values. Using parser options, `lexpr` allows to
//! parse these symbols as booleans, which may be desirable in some
//! circumstances, as booleans are simpler to handle than symbols.
//!
//!  ```scheme
//!  #t ; The literal representing true
//!  #f ; The literal representing false
//!  ```
//!
//! ### The empty list and "nil"
//!
//! In traditional Lisps, the end of list is represented as by a
//! special atom written as `nil`. In Scheme, the empty list is an
//! atom written as `()`, and there `nil` is just a regular
//! symbol. Both `nil` and the empty list are present and
//! distinguishable in `lexpr`.
//!
//! ### Numbers
//!
//! Numbers are represented by the [`Number`] abstract data type. It can handle
//! signed and unsigned integers, each up to 64 bit size, as well as floating
//! point numbers. The Scheme syntax for hexadecimal, octal, and binary literals
//! is supported.
//!
//! ```scheme
//! 1 -4 3.14  ; A postive, negative, and a floating point number
//! #xDEADBEEF ; An integer written using decimal notation
//! #o0677     ; Octal
//! #b10110    ; Binary
//! ```
//!
//! Scheme has an elaborate numerical type hierarchy (called "numeric tower"),
//! which supports fractionals, numbers of arbitrary size, and complex
//! numbers. These more advanced number types are not yet supported by `lexpr`.
//!
//!
//! ### Characters
//!
//! Characters are unicode codepoints, represented by Rust's `char` data type
//! embedded in the [`Value::Char`] variant.
//!
//! ### Strings
//!
//! ```scheme
//! "Hello World!"
//! ```
//!
//! ## Lists
//!
//! Lists are a sequence of values, of either atoms or lists. In fact,
//! Lisp does not have a "real" list data type, but instead lists are
//! represented by chains of so-called "cons cells", which are used to
//! form a singly-linked list, terminated by the empty list (or `nil`
//! in tradional Lisps). It is also possible for the terminator to not
//! be the empty list, but instead be af an arbitrary other data type.
//! In this case, the list is refered to as an "improper" or "dotted"
//! list. Here are some examples:
//!
//! ```scheme
//! ("Hello" "World")   ; A regular list
//! ;; A list having with another, single-element, list as
//! ;; its second item
//! ("Hello" ("World"))
//! (1 . 2) ; A cons cell, represented as an improper list by `lexpr`
//! (1 2 . 3) ; A dotted (improper) list
//! ```
//!
//! Lists are not only used to represent sequences of values, but also
//! associative arrays, also known as maps. A map is represented as a list
//! containing cons cells, where the first field of each cons cell, called
//! `car`, for obscure historical reasons, is the key, and the second field
//! (`cdr`) of the cons cell is the associated value.
//!
//! ```scheme
//! ;; An association list with the symbols `a` and `b` as keys
//! ((a . 42) (b . 43))
//! ```
//!
//! ## Vectors
//!
//! In contrast to lists, which are represented as singly-linked chains of "cons
//! cells", vectors allow O(1) indexing, and thus are quite similar to Rusts
//! `Vec` datatype.
//!
//! ```scheme
//! #(1 2 "three") ; A vector in Scheme notation
//! ```
//!
//! ## Byte vectors
//!
//! Byte vectors are similar to regular vectors, but are uniform: each element
//! only holds a single byte, i.e. an exact integer in the range of 0 to 255,
//! inclusive.
//!
//! ```scheme
//! #u8(41 42 43) ; A byte vector
//! ```
//!
//! [Serde]: https://crates.io/crates/serde
//! [`serde-lexpr`]: https://docs.rs/serde-lexpr

/// Construct a [`Value`] using syntax similar to regular S-expressions.
///
/// The macro is intended to have a feeling similiar to an implicitly
/// quasiquoted Scheme expression.
///
/// For interpolation, use `unquote` (aka "`,`"), like this:
///
/// ```
/// # use lexpr::{sexp, Value};
/// let number = 42;
/// let list = sexp!((41 ,number 43));
/// assert_eq!(list.to_vec(), Some(vec![Value::from(41), Value::from(42), Value::from(43)]));
/// ```
///
/// You can also provide a *Rust* expression to interpolate by using
/// parentheses:
///
/// ```
/// # use lexpr::{sexp, Value};
/// let number = 40;
/// let alist = sexp!(((answer . ,(number + 2))));
/// assert_eq!(alist.get("answer"), Some(&Value::from(42)));
/// ```
///
/// The interpolated variable (or expression) must yield a value that
/// is convertible to [`Value`] using the `From` trait.
///
/// # Booleans
///
/// ```
/// # use lexpr_macros::sexp;
/// let t = sexp!(#f);
/// let f = sexp!(#t);
/// ```
///
/// # Symbols and keywords
///
/// Due to syntactic restrictions of Rust's macro system, to use
/// kebab-case, you need to use the `#"..."` (or `#:"...", for
/// keywords) syntax.
///
/// ```
/// # use lexpr_macros::sexp;
/// let sym = sexp!(symbol);
/// let kw = sexp!(#:keyword);
/// assert!(sym.is_symbol());
/// assert!(kw.is_keyword());
///
/// let kebab_sym = sexp!(#"kebab-symbol");
/// let kebab_kw = sexp!(#:"kebab-keyword");
/// assert!(kebab_sym.is_symbol());
/// assert!(kebab_kw.is_keyword());
/// ```
///
/// For convenience, since `lexpr` version 0.3.0, keywords can also be
/// written using the Emacs Lisp (or Common Lisp) syntax, leaving off
/// the Scheme-ish octothorpe:
///
/// ```
/// # use lexpr_macros::sexp;
/// let kw = sexp!(:keyword);
/// assert!(kw.is_keyword());
///
/// let kebab_kw = sexp!(#:"kebab-keyword");
/// assert!(kebab_kw.is_keyword());
/// ```
///
/// Since `lexpr` version 0.2.7, symbols following the R7RS (Scheme)
/// syntax, which additionally consist of *only* characters that Rust
/// considers punctuation can be written without double quotes:
///
/// ```
/// # use lexpr_macros::sexp;
/// let expr = sexp!((+ 1 2));
/// assert!(expr.is_list());
///
/// let strange_symbol = sexp!(!$%&*+-./:<=>?@^~);
/// assert_eq!(strange_symbol.as_symbol(), Some("!$%&*+-./:<=>?@^~"));
/// ```
///
/// # Characters
///
/// Characters can be written using Rust's character syntax:
///
/// ```
/// # use lexpr_macros::sexp;
/// let ch = sexp!('λ');
/// assert!(ch.is_char());
/// assert_eq!(ch.as_char(), Some('λ'));
/// ```
///
/// # Lists
///
/// Lists can be formed by using the same syntax as in Lisp, including dot
/// notation.
///
/// ```
/// # use lexpr_macros::sexp;
/// let l1 = sexp!((1 2 3));
/// let l2 = sexp!((1 . (2 . (3 . ()))));
/// let l3 = sexp!((1 2 . (3 . ())));
/// assert_eq!(l1, l2);
/// assert_eq!(l2, l3);
/// ```
///
/// Improper (aka dotted) lists are supported as well:
///
/// ```
/// # use lexpr_macros::sexp;
/// let dotted = sexp!((1 2 . three));
/// assert!(dotted.is_dotted_list());
/// let tail = dotted.as_cons().unwrap().cdr();
/// assert!(tail.is_cons());
/// assert_eq!(tail, &sexp!((2 . three)));
/// ```
///
/// # Vectors
///
/// Vectors can be written using Scheme notation, e.g.:
///
/// ```
/// # use lexpr_macros::sexp;
/// let v = sexp!(#(1 2 "three"));
/// assert!(v.is_vector());
/// assert_eq!(v[2], sexp!("three"));
/// ```
///
/// [`Value`]: enum.Value.html
#[cfg(feature = "sexp-macro")]
#[cfg_attr(docsrs, doc(cfg(feature = "sexp-macro")))]
pub use lexpr_macros::sexp;

mod syntax;

pub mod cons;
pub mod datum;
pub mod number;
pub mod parse;
pub mod print;
pub mod value;

#[doc(inline)]
pub use self::parse::{
    from_reader, from_reader_custom, from_slice, from_slice_custom, from_str, from_str_custom,
    Parser,
};

#[doc(inline)]
pub use self::print::{
    to_string, to_string_custom, to_vec, to_vec_custom, to_writer, to_writer_custom, Printer,
};

#[doc(inline)]
pub use value::Value;

#[doc(inline)]
pub use datum::Datum;

#[doc(inline)]
pub use cons::Cons;

#[doc(inline)]
pub use value::Index;

#[doc(inline)]
pub use number::Number;

#[cfg(test)]
mod tests;
