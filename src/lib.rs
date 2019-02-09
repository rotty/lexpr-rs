#![deny(missing_docs)]

//! This crate provides a data structure that can values as typically
//! found in Lisp-like languages, as well as a macro for embedding
//! such values into Rust code, using a Lisp-like syntax, ususally
//! refered to as S-expression syntax.
//!
//! # What are S-expressions?
//!
//! S-expressions, as mentioned above, is the notation used by various
//! dialects of Lisp to represent data (and code). As a data format,
//! it is roughly comparable to JSON (JavaScript Object Notation), but
//! syntactically more lightweight and simpler. Note that different
//! Lisp dialects have notational differences for some data types, and
//! some may lack specific data types completely. This section tries
//! to give an overview over the different types of values
//! representable by the [`Value`] data type and how it relates to
//! different Lisp dialects. All examples are given in the syntax used
//! in [Guile](https://www.gnu.org/software/guile/) Scheme
//! implementation.
//!
//! While `lexpr` does not implemented a textual parser and serializer
//! yet, the intention is that it will be able to parse and generate a
//! S-expression data in various "dialects" in use by different Lisp
//! variants.
//!
//! In the following, the S-expression values that are modeled by
//! `lexpr` are introduced, In general, S-expression values can be
//! split into the two categories of "atoms" and lists.
//!
//! ## Atoms
//!
//! Atoms are primitive (i.e., non-compound) data type such as
//! numbers, strings and booleans.
//!
//! ### Symbols and keywords
//!
//! Lisp also has a data type not commonly found in other languages,
//! namely "symbols". A symbol is conceptually similar to identifiers
//! in other languages, but allow for a much richer set of characters
//! than allowed for identifiers in other languages. Also, identifiers
//! in other languages can typically not be used in data; lisps expose
//! them as a primitive data type, a result of the
//! [homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity) of
//! the Lisp language family.
//!
//! Another data type, present in some Lisp dialects, such as Emacs
//! Lisp, Common Lisp, and several Scheme implementations, are
//! keywords. These are also supported by `lexpr`. Keywords are very
//! similiar to symbols, but use a different syntax and are used for
//! different purposes in the language.
//!
//!
//!
//! ### Booleans
//!
//!  ```scheme
//!  #t ; The literal representing true
//!  #f ; The literal representing false
//!  ```
//!
//! ### The empty list and "nil"
//!
//! In traditional lisps, the end of list is represented as by a
//! special atom referred to as "nil". In Scheme, the empty list is a
//! separate atom written as `()`. Both values are present and
//! distinguishable in `lexpr`, but the empty list s not considered an
//! atom (see also below for more on list representation in `lexpr`).
//!
//! ### Numbers
//!
//! Numbers are represented by the ['Number'] abstract data type. It
//! can handle signed and unsigned integers, each up to 64 bit size,
//! as well as floating point numbers.
//!
//! ### Strings
//!
//! ```scheme
//! "Hello World!"
//! ```
//!
//! ## Lists
//!
//! Lists, which are a sequence of values (of either atoms or
//! lists). In fact, Lisp does not have a "real" list data type, but
//! instead lists are represented by chains of so-called cons cells",
//! which form a singly-linked lists, terminated by the empty list. It
//! is also possible for the terminator to not be the empty list, but
//! an arbitrary primitive data type (i.e., an atom). In this case,
//! the list is refered to as an "improper" or "dotted" list. Here are
//! some examples
//!
//! Lists are not only used to represent sequences of values, but also
//! associative arrays, also known as maps. A map is represented as a
//! list containing sub-lists, where the first element of each
//! sub-list is the key, and the remainder of the list is the
//! associated value.
//!
//! In `lexpr`, lists are implemented not as singly-linked lists, but
//! using vectors, which is more efficient generally. However, that
//! choice precludes an efficient implementation of taking a suffix of
//! an existing list.
//!
//! ```scheme
//! ("Hello" "World")   ; A regular list
//! ;; A list having with another, single-element, list as
//! ;; its second item
//! ("Hello" ("World"))
//! (1 . 2) ; A cons cell, represented as an improper list by `lexpr`
//! (1 2 . 3) ; A dotted (improper) list
//! ;; An association list with the symbols `a` and `b` as keys
//! ((a . 42) (b . 43))
//! ```
//!
//! [`Value`]: enum.Value.html

use proc_macro_hack::proc_macro_hack;

/// Construct a [`Value`] using syntax similar to regular S-expressions.
///
/// The macro is intended to have a feeling similiar to an implicitly
/// quasiquoted Scheme expression.
///
/// # Booleans
///
/// ```
/// # use lexpr::sexp;
///
/// let t = sexp!(#f);
/// let f = sexp!(#t);
/// ```
///
/// # Symbols and keywords
///
/// Due to syntactic restrictions of Rust's macro system, to use
/// kebab-case, you need to use the `#"..."` syntax.
///
/// ```
/// # use lexpr::sexp;
///
/// let sym = sexp!(symbol);
/// let kw = sexp!(#:keyword);
/// let kebab_sym = sexp!(#"kebab-symbol");
/// let kebab_kw = sexp!(#"kebab-keyword");
/// ```
/// # Lists
///
/// ```
/// # use lexpr::sexp;
///
/// let l1 = sexp!((1 2 3));
/// let l2 = sexp!((1 . (2 . (3 . ()))));
/// let l3 = sexp!((1 2 . (3 . ())));
/// assert_eq!(l1, l2);
/// assert_eq!(l2, l3);
/// ```
///
/// [`Value`]: enum.Value.html
#[proc_macro_hack]
pub use lexpr_macros::sexp;

mod error;
pub mod value;

pub mod atom;
pub mod number;

#[doc(inline)]
pub use value::Value;

#[doc(inline)]
pub use atom::Atom;

#[doc(inline)]
pub use number::Number;

#[doc(inline)]
pub use error::{Error, Result};
