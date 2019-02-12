//! The Value enum, a dynamically typed way of representing any valid S-expression value.
//!
//! # Constructing S-Expressions
//!
//! Lexpr provides a [`sexp!` macro][macro] to build `lexpr::Value`
//! objects with very natural S-expression syntax.
//!
//! One neat thing about the `sexp!` macro is that variables and
//! expressions can be interpolated directly into the S-expression
//! value as you are building it. The macro will check at compile time
//! that the value you are interpolating is able to be represented as
//! S-expression data.
//!
//! To interpolate, use the comma (`,`, also known as "unqote" in
//! Lisp). The interpolated expression must either be a single token,
//! or surrounded by round or curly braces.
//!
//! ```
//! # use lexpr::sexp;
//! #
//! # fn random_phone() -> u16 { 0 }
//! #
//! let full_name = "John Doe";
//! let age_last_year = 42;
//!
//! // The type of `john` is `lexpr::Value`
//! let john = sexp!((
//!     (name . ,full_name)
//!     (age . ,(age_last_year + 1))
//!     (phones ,{ format!("+44 {}", random_phone()) })
//! ));
//! ```
//!
//! A string of S-expression data can be parsed into a `lexpr::Value` by the
//! [`lexpr::from_str`][from_str] function. There is also
//! [`from_slice`][from_slice] for parsing from a byte slice `&[u8]` and
//! [`from_reader`][from_reader] for parsing from any `io::Read` like a File or
//! a TCP stream.
//!
//! [macro]: macro.sexp.html
//! [from_str]: fn.from_str.html
//! [from_slice]: fn.from_slice.html
//! [from_reader]: fn.from_reader.html

use std::borrow::Cow;

use crate::atom::Atom;
use crate::number::Number;

pub use self::index::Index;

/// Represents an S-expression value.
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    /// Represents a lisp atom (non-list).
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let atoms = vec![
    ///     sexp!(#nil),
    ///     sexp!(5.0),
    ///     sexp!("Hello"),
    ///     sexp!(symbol),
    ///     sexp!(#:keyword),
    ///     sexp!(#:"kebab-keyword"),
    /// ];
    /// ```
    Atom(Atom),

    /// Represents a proper Lisp list.
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let v = sexp!((a list 1 2 3));
    /// ```
    List(Vec<Value>),

    /// Represents an improper (aka dotted) Lisp list.
    ///
    /// Note that this may also represent a single atom, if the `Vec`
    /// field has zero length. This degenerate case should never be
    /// constructed by the `sexp` macro, but the API does not prevent
    /// users from constructing such values. Use [`Value::as_atom`]
    /// when you need to treat these two cases identically.
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let v = sexp!((a dotted . list));
    /// ```
    ImproperList(Vec<Value>, Atom),
}

impl Value {
    /// Construct the nil value.
    pub fn nil() -> Self {
        Value::Atom(Atom::Nil)
    }

    /// Construct a symbol, given its name.
    pub fn symbol(name: impl Into<String>) -> Self {
        Value::Atom(Atom::Symbol(name.into()))
    }

    /// Construct a keyword, given its name.
    ///
    /// ```
    /// # use lexpr::Value;
    /// let value = Value::keyword("foo");
    /// assert!(value.is_keyword());
    /// assert_eq!(value.as_name().unwrap(), "foo");
    /// ```
    pub fn keyword(name: impl Into<String>) -> Self {
        Value::Atom(Atom::Keyword(name.into()))
    }

    /// Create a list value from elements convertible into `Value`.
    ///
    /// ```
    /// # use lexpr::{sexp, Value};
    /// assert_eq!(Value::list(vec![1, 2, 3]), sexp!((1 2 3)));
    /// ```
    pub fn list<I>(elements: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Value>,
    {
        Value::List(elements.into_iter().map(Into::into).collect())
    }

    /// Create a list value from elements convertible into `Value`.
    ///
    /// ```
    /// # use lexpr::{sexp, Value};
    /// assert_eq!(Value::improper_list(vec![1u32, 2], 3), sexp!((1 2 . 3)));
    /// ```
    pub fn improper_list<I, T>(elements: I, tail: T) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Value>,
        T: Into<Atom>,
    {
        Value::ImproperList(elements.into_iter().map(Into::into).collect(), tail.into())
    }

    /// Returns true if the `Value` is a String. Returns false otherwise.
    ///
    /// For any Value on which `is_string` returns true, `as_str` is guaranteed
    /// to return the string slice.
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let v = sexp!(((a "some string") (b #f)));
    ///
    /// assert!(v["a"][1].is_string());
    ///
    /// // The boolean `false` is not a string.
    /// assert!(!v["b"][1].is_string());
    /// ```
    pub fn is_string(&self) -> bool {
        self.as_str().is_some()
    }

    /// If the `Value` is a String, returns the associated str. Returns `None`
    /// otherwise.
    pub fn as_str(&self) -> Option<&str> {
        self.as_atom().and_then(Atom::as_str)
    }

    /// Returns true if the `Value` is a symbol. Returns false otherwise.
    ///
    /// For any Value on which `is_symbol` returns true, `as_symbol` is guaranteed
    /// to return the string slice.
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let v = sexp!((#:foo bar "baz"));
    ///
    /// assert!(v[1].is_symbol());
    ///
    /// // Keywords and strings are not symbols.
    /// assert!(!v[0].is_symbol());
    /// assert!(!v[2].is_symbol());
    /// ```
    pub fn is_symbol(&self) -> bool {
        self.as_symbol().is_some()
    }

    /// If the `Value` is a symbol, returns the associated str. Returns `None`
    /// otherwise.
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let v = sexp!(foo);
    ///
    /// assert_eq!(v.as_symbol(), Some("foo"));
    /// ```
    pub fn as_symbol(&self) -> Option<&str> {
        self.as_atom().and_then(Atom::as_symbol)
    }

    /// Returns true if the `Value` is a keyword. Returns false otherwise.
    ///
    /// For any Value on which `is_keyword` returns true, `as_keyword` is guaranteed
    /// to return the string slice.
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let v = sexp!((#:foo bar "baz"));
    ///
    /// assert!(v[0].is_keyword());
    ///
    /// // Symbols and strings are not keywords.
    /// assert!(!v[1].is_keyword());
    /// assert!(!v[2].is_keyword());
    /// ```
    pub fn is_keyword(&self) -> bool {
        self.as_keyword().is_some()
    }

    /// If the `Value` is a keyword, returns the associated str. Returns `None`
    /// otherwise.
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let v = sexp!(#:foo);
    ///
    /// assert_eq!(v.as_keyword(), Some("foo"));
    /// ```
    pub fn as_keyword(&self) -> Option<&str> {
        self.as_atom().and_then(Atom::as_keyword)
    }

    /// Get the name of a symbol or keyword, or the value of a string.
    pub fn as_name(&self) -> Option<&str> {
        self.as_atom().and_then(Atom::as_name)
    }

    /// Lossless conversion to an `Atom`.
    ///
    /// Returns the `Atom` directly corresponding to this value, or
    /// `None`. This returns a `Some` only if the value itself is an
    /// atom, or it is an improper list of zero length.
    pub fn as_atom(&self) -> Option<&Atom> {
        match self {
            Value::Atom(atom) => Some(atom),
            Value::ImproperList(elements, atom) if elements.is_empty() => Some(atom),
            _ => None,
        }
    }

    /// For improper lists, return their non-list tail.
    ///
    /// For proper lists and atoms, this will return `None`.
    pub fn rest(&self) -> Option<&Atom> {
        match self {
            Value::ImproperList(_, atom) => Some(atom),
            _ => None,
        }
    }

    /// Return the tail of an improper list as a value.
    ///
    /// This is the sloppy version of the `rest` method. It will, for
    /// improper lists, return a value constructed from their non-list
    /// tail, while for all other values, it will return the nil
    /// value.
    pub fn tail(&self) -> Value {
        self.rest()
            .map_or_else(Value::nil, |atom| Value::Atom(atom.clone()))
    }

    /// Returns true if the `Value` is an integer between `i64::MIN` and
    /// `i64::MAX`.
    ///
    /// For any Value on which `is_i64` returns true, `as_i64` is guaranteed to
    /// return the integer value.
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let big = i64::max_value() as u64 + 10;
    /// let v = sexp!(((a 64) (b ,big) (c 256.0)));
    ///
    /// assert!(v["a"][1].is_i64());
    ///
    /// // Greater than i64::MAX.
    /// assert!(!v["b"][1].is_i64());
    ///
    /// // Numbers with a decimal point are not considered integers.
    /// assert!(!v["c"][1].is_i64());
    /// ```
    pub fn is_i64(&self) -> bool {
        match self.as_atom() {
            Some(Atom::Number(n)) => n.is_i64(),
            _ => false,
        }
    }

    /// Returns true if the `Value` is an integer between zero and `u64::MAX`.
    ///
    /// For any Value on which `is_u64` returns true, `as_u64` is guaranteed to
    /// return the integer value.
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let v = sexp!(((a 64) (b -64) (c 256.0)));
    ///
    /// assert!(v["a"][1].is_u64());
    ///
    /// // Negative integer.
    /// assert!(!v["b"][1].is_u64());
    ///
    /// // Numbers with a decimal point are not considered integers.
    /// assert!(!v["c"][1].is_u64());
    /// ```
    pub fn is_u64(&self) -> bool {
        match self.as_atom() {
            Some(atom) => atom.is_u64(),
            _ => false,
        }
    }

    /// Returns true if the `Value` is a number that can be represented by f64.
    ///
    /// For any Value on which `is_f64` returns true, `as_f64` is guaranteed to
    /// return the floating point value.
    ///
    /// Currently this function returns true if and only if both `is_i64` and
    /// `is_u64` return false but this is not a guarantee in the future.
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let v = sexp!(((a 256.0) (b 64) (c -64)));
    ///
    /// assert!(v["a"][1].is_f64());
    ///
    /// // Integers.
    /// assert!(!v["b"][1].is_f64());
    /// assert!(!v["c"][1].is_f64());
    /// ```
    pub fn is_f64(&self) -> bool {
        match self.as_atom() {
            Some(atom) => atom.is_f64(),
            _ => false,
        }
    }

    /// If the `Value` is an integer, represent it as i64 if possible. Returns
    /// None otherwise.
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let big = i64::max_value() as u64 + 10;
    /// let v = sexp!(((a 64) (b ,big) (c 256.0)));
    ///
    /// assert_eq!(v["a"][1].as_i64(), Some(64));
    /// assert_eq!(v["b"][1].as_i64(), None);
    /// assert_eq!(v["c"][1].as_i64(), None);
    /// ```
    pub fn as_i64(&self) -> Option<i64> {
        self.as_atom().and_then(Atom::as_i64)
    }

    /// If the `Value` is an integer, represent it as u64 if possible. Returns
    /// None otherwise.
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let v = sexp!(((a 64) (b -64) (c 256.0)));
    ///
    /// assert_eq!(v["a"][1].as_u64(), Some(64));
    /// assert_eq!(v["b"][1].as_u64(), None);
    /// assert_eq!(v["c"][1].as_u64(), None);
    /// ```
    pub fn as_u64(&self) -> Option<u64> {
        self.as_atom().and_then(Atom::as_u64)
    }

    /// If the `Value` is a number, represent it as f64 if possible. Returns
    /// None otherwise.
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let v = sexp!(((a 256.0) (b 64) (c -64)));
    ///
    /// assert_eq!(v["a"][1].as_f64(), Some(256.0));
    /// assert_eq!(v["b"][1].as_f64(), Some(64.0));
    /// assert_eq!(v["c"][1].as_f64(), Some(-64.0));
    /// ```
    pub fn as_f64(&self) -> Option<f64> {
        self.as_atom().and_then(Atom::as_f64)
    }

    /// Returns true if the `Value` is a Boolean. Returns false otherwise.
    ///
    /// For any Value on which `is_boolean` returns true, `as_bool` is
    /// guaranteed to return the boolean value.
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let v = sexp!(((a #f) (b #nil)));
    ///
    /// assert!(v["a"][1].is_boolean());
    ///
    /// // The nil value is special, and not a boolean.
    /// assert!(!v["b"][1].is_boolean());
    /// ```
    pub fn is_boolean(&self) -> bool {
        self.as_bool().is_some()
    }

    /// If the `Value` is a Boolean, returns the associated bool. Returns None
    /// otherwise.
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let v = sexp!(((a #f) (b "false")));
    ///
    /// assert_eq!(v["a"][1].as_bool(), Some(false));
    ///
    /// // The string `"false"` is a string, not a boolean.
    /// assert_eq!(v["b"][1].as_bool(), None);
    /// ```
    pub fn as_bool(&self) -> Option<bool> {
        self.as_atom().and_then(Atom::as_bool)
    }

    /// Returns true if the `Value` is a Nil atom. Returns false otherwise.
    ///
    /// For any Value on which `is_nil` returns true, `as_nil` is guaranteed
    /// to return `Some(())`.
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let v = sexp!(((a #nil) (b #f)));
    ///
    /// assert!(v["a"][1].is_nil());
    ///
    /// // The boolean `false` is not nil.
    /// assert!(!v["b"][1].is_nil());
    /// ```
    pub fn is_nil(&self) -> bool {
        self.as_nil().is_some()
    }

    /// If the `Value` is a Nil atom, returns `()`. Returns `None` otherwise.
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let v = sexp!(((a #nil) (b #f)));
    ///
    /// assert_eq!(v["a"][1].as_nil(), Some(()));
    ///
    /// // The boolean `false` is not nil.
    /// assert_eq!(v["b"][1].as_nil(), None);
    /// ```
    pub fn as_nil(&self) -> Option<()> {
        self.as_atom().and_then(Atom::as_nil)
    }

    /// Index into a S-expression list. A string or `Value` value can
    /// be used to access a value in an association list, and a usize
    /// index can be used to access the n-th element of a list.
    ///
    /// For indexing into association lists, the given string will
    /// match strings, symbols and keywords.
    ///
    /// Returns `None` if the type of `self` does not match the type
    /// of the index, for example if the index is a string and `self`
    /// is not an association list. Also returns `None` if the given
    /// key does not exist in the map or the given index is not within
    /// the bounds of the list; note that the tail of an improper list
    /// is also considered out-of-bounds.
    ///
    /// In Scheme terms, this method can be thought of a combination
    /// of `assoc` and `list-ref`, depending on the argument type. If
    /// you want to look up a number in an association list, use an
    /// `Value` value containing that number.
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let alist = sexp!((("A" . 65) (B . 66) (#:C 67) (42 . "The answer")));
    /// assert_eq!(*alist.get("A").unwrap(), sexp!(("A" . 65)));
    /// assert_eq!(*alist.get("B").unwrap(), sexp!((B . 66)));
    /// assert_eq!(*alist.get("C").unwrap(), sexp!((#:C 67)));
    /// assert_eq!(*alist.get(sexp!(42)).unwrap(), sexp!((42 . "The answer")));
    ///
    /// let list = sexp!(("A" "B" "C"));
    /// assert_eq!(*list.get(2).unwrap(), sexp!("C"));
    ///
    /// assert_eq!(list.get("A"), None);
    /// ```
    ///
    /// Square brackets can also be used to index into a value in a
    /// more concise way. This returns the nil value in cases where
    /// `get` would have returned `None`. See [`Index`] for details.
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let alist = sexp!((
    ///     ("A" . ("a" "á" "à"))
    ///     ("B" . ((b . 42) (c . 23)))
    ///     ("C" . ("c" "ć" "ć̣" "ḉ"))
    /// ));
    /// assert_eq!(alist["B"][1], sexp!((b . 42)));
    /// assert_eq!(alist["C"][2], sexp!("ć"));
    ///
    /// assert_eq!(alist["D"], sexp!(#nil));
    /// assert_eq!(alist[0]["x"]["y"]["z"], sexp!(#nil));
    /// ```
    ///
    /// [`Index`]: value/trait.Index.html
    pub fn get<I: Index>(&self, index: I) -> Option<&Value> {
        index.index_into(self)
    }
}

macro_rules! impl_from_atom {
    (
        $($ty:ty),*
    ) => {
        $(
            impl From<$ty> for Value {
                #[inline]
                fn from(n: $ty) -> Self {
                    Value::Atom(Atom::from(n))
                }
            }
        )*
    };
}

impl_from_atom!(u8, u16, u32, u64, i8, i16, i32, f32, f64, bool, &str, String, Number);

impl<'a> From<Cow<'a, str>> for Value {
    #[inline]
    fn from(s: Cow<'a, str>) -> Self {
        Value::from(s.to_string())
    }
}

impl From<Vec<Value>> for Value {
    fn from(elements: Vec<Value>) -> Self {
        Value::List(elements)
    }
}

mod index;
mod partial_eq;
