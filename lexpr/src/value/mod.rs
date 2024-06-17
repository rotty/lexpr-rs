//! The Value enum, a dynamically typed way of representing any valid S-expression value.
//!
//! # Constructing S-Expressions
//!
//! Lexpr provides a [`sexp!` macro][macro] to build `lexpr::Value`
//! objects with very natural S-expression syntax.
//!
//! ```
//! # use lexpr_macros::sexp;
//!
//! // The type of `john` is `lexpr::Value`
//! let john = sexp!((
//!     (name . "John Doe")
//!     (age . 43)
//!     (phones "+44 1234567" "+44 2345678")
//! ));
//!
//! println!("first phone number: {}", john["phones"][0]);
//!
//! // Convert to a string of S-expression data and print it out
//! println!("{}", john.to_string());
//! ```
//!
//! The `Value::to_string()` function converts a `lexpr::Value` into a
//! `String` of S-expression text.
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
//! # use lexpr_macros::sexp;
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
//! [`from_reader`][from_reader] for parsing from any `io::Read` like a file or
//! a TCP stream. For all these functions there also is a `_custom` variant
//! which allows for specifying parser options, in case the input deviates from
//! the `lexpr` default behavior.
//!
//! ```
//! # use lexpr::{parse::Error, Value};
//!
//! # fn main() -> Result<(), Error> {
//! // Some S-expression input data as a &str. Maybe this comes from the user.
//! let data = r#"(
//!         (name . "John Doe")
//!         (age . 43)
//!         (phones . (
//!             "+44 1234567"
//!             "+44 2345678"
//!         ))
//!     )"#;
//!
//! // Parse the string of data into lexpr::Value.
//! let v: Value = lexpr::from_str(data)?;
//!
//! // Access parts of the data by indexing with square brackets.
//! println!("Please call {} at the number {}", v["name"], v["phones"][0]);
//! # Ok(())
//! # }
//! ```
//!
//! [macro]: ../macro.sexp.html
//! [from_str]: ../parse/fn.from_str.html
//! [from_slice]: ../parse/fn.from_slice.html
//! [from_reader]: ../parse/fn.from_reader.html

use std::fmt;
use std::io;
use std::str;

use crate::cons::{self, Cons};
use crate::number::Number;

pub use self::index::Index;

/// Represents an S-expression value.
///
/// See the [`lexpr::value`] module documentation for usage examples.
///
/// [`lexpr::value`]: index.html
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    /// The special "nil" value.
    ///
    /// This is kind of an oddball value. In traditional Lisps (e.g., Common
    /// Lisp or Emacs Lisp) the empty list can be written as the symbol `nil`,
    /// while in Scheme, `nil` is just a regular symbol. Furthermore,
    /// traditional Lisps don't have a separate boolean data type, and represent
    /// true and false by the symbols `t` and `nil` instead. The `lexpr` parser
    /// can be instructed to parse the `nil` symbol as the `Nil` value (see
    /// [`NilSymbol::Special`]), allowing to choose its representation when
    /// converting to text again (see [`NilSyntax`]). Note that the empty list,
    /// when written as `()` or implicitly constructed as a list terminator, is
    /// always parsed as [`Value::Null`], not `Value::Nil`.
    ///
    /// In addition to being useful for conversions between S-expression
    /// variants, this value is also potentially returned when using the square
    /// bracket indexing operator on `Value`.
    ///
    /// [`NilSymbol::Special`]: crate::parse::NilSymbol::Special
    /// [`NilSyntax`]: ../print/enum.NilSyntax.html
    Nil,

    /// The empty list.
    ///
    /// This value terminates a chain of cons cells forming a proper list.
    Null,

    /// A boolean value.
    Bool(bool),

    /// A number.
    Number(Number),

    /// A character.
    Char(char),

    /// A string.
    String(Box<str>),

    /// A symbol.
    Symbol(Box<str>),

    /// A keyword.
    Keyword(Box<str>),

    /// A byte vector.
    Bytes(Box<[u8]>),

    /// Represents a Lisp "cons cell".
    ///
    /// Cons cells are often used to form singly-linked lists.
    /// ```
    /// # use lexpr_macros::sexp;
    /// let v = sexp!((a list 1 2 3));
    /// assert!(v.is_cons());
    /// assert_eq!(v[4], sexp!(3));
    /// ```
    Cons(Cons),

    /// A Lisp vector.
    Vector(Vec<Value>),
}

impl Value {
    /// Construct a symbol, given its name.
    pub fn symbol(name: impl Into<Box<str>>) -> Self {
        Value::Symbol(name.into())
    }

    /// Construct a keyword, given its name.
    ///
    /// ```
    /// # use lexpr::Value;
    /// let value = Value::keyword("foo");
    /// assert!(value.is_keyword());
    /// assert_eq!(value.as_keyword().unwrap(), "foo");
    /// ```
    pub fn keyword(name: impl Into<Box<str>>) -> Self {
        Value::Keyword(name.into())
    }

    /// Construct a string.
    ///
    /// ```
    /// # use lexpr::Value;
    /// let value = Value::string("foo");
    /// assert!(value.is_string());
    /// assert_eq!(value.as_str().unwrap(), "foo");
    /// ```
    pub fn string(s: impl Into<Box<str>>) -> Self {
        Value::String(s.into())
    }

    /// Construct a byte vector.
    ///
    /// ```
    /// # use lexpr::Value;
    /// let value = Value::bytes(b"foo" as &[u8]);
    /// assert!(value.is_bytes());
    /// assert_eq!(value.as_bytes().unwrap(), b"foo");
    /// ```
    pub fn bytes(bv: impl Into<Box<[u8]>>) -> Self {
        Value::Bytes(bv.into())
    }

    /// Create a cons cell given its `car` and `cdr` fields.
    ///
    /// ```
    /// # use lexpr::Value;
    /// let value = Value::cons(1, Value::Null);
    /// assert!(value.is_cons());
    /// assert_eq!(value.as_pair().unwrap(), (&Value::from(1), &Value::Null));
    /// ```
    ///
    /// Note that you can also construct a cons cell from a Rust pair via the
    /// `From` trait:
    ///
    /// ```
    /// # use lexpr::Value;
    /// let value = Value::from((42, "answer"));
    /// assert!(value.is_cons());
    /// assert_eq!(value.as_pair().unwrap(), (&Value::from(42), &Value::string("answer")));
    /// ```
    pub fn cons<T, U>(car: T, cdr: U) -> Self
    where
        T: Into<Value>,
        U: Into<Value>,
    {
        Value::Cons(Cons::new(car, cdr))
    }

    /// Create a list value from elements convertible into `Value`.
    ///
    /// ```
    /// # use lexpr::Value;
    /// # use lexpr_macros::sexp;
    /// assert_eq!(Value::list(vec![1, 2, 3]), sexp!((1 2 3)));
    /// ```
    pub fn list<I>(elements: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Value>,
    {
        Self::append(elements, Value::Null)
    }

    /// Returns true if the value is a (proper) list.
    pub fn is_list(&self) -> bool {
        match self {
            Value::Null => true,
            Value::Cons(pair) => pair
                .iter()
                .all(|p| matches!(p.cdr(), Value::Null | Value::Cons(_))),
            _ => false,
        }
    }

    /// Returns true if the value is a dotted (improper) list.
    ///
    /// Note that all values that are not pairs are considered dotted lists.
    ///
    /// ```
    /// # use lexpr::Value;
    /// # use lexpr_macros::sexp;
    /// let list = sexp!((1 2 3));
    /// assert!(!list.is_dotted_list());
    /// let dotted = sexp!((1 2 . 3));
    /// assert!(dotted.is_dotted_list());
    /// ```
    pub fn is_dotted_list(&self) -> bool {
        match self {
            Value::Null => false,
            Value::Cons(pair) => pair.iter().all(|p| !matches!(p.cdr(), Value::Null)),
            _ => true,
        }
    }

    /// Create a list value from elements convertible into `Value`, using a
    /// given value as a tail.
    ///
    /// ```
    /// # use lexpr::Value;
    /// # use lexpr_macros::sexp;
    /// assert_eq!(Value::append(vec![1u32, 2], 3), sexp!((1 2 . 3)));
    /// assert_eq!(Value::append(vec![1u32, 2, 3], sexp!((4 5))), sexp!((1 2 3 4 5)));
    /// ```
    pub fn append<I, T>(elements: I, tail: T) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Value>,
        T: Into<Value>,
    {
        let mut list = Cons::new(Value::Nil, Value::Null);
        let mut pair = &mut list;
        let mut have_value = false;
        for item in elements {
            if have_value {
                pair.set_cdr(Value::from((Value::Nil, Value::Null)));
                pair = pair.cdr_mut().as_cons_mut().unwrap();
            }
            pair.set_car(item.into());
            have_value = true;
        }
        if have_value {
            pair.set_cdr(tail.into());
            Value::Cons(list)
        } else {
            tail.into()
        }
    }

    /// Create a vector value from elements convertible into `Value`.
    ///
    /// ```
    /// # use lexpr::Value;
    /// # use lexpr_macros::sexp;
    /// assert_eq!(Value::vector(vec![1u32, 2, 3]), sexp!(#(1 2 3)));
    /// ```
    pub fn vector<I>(elements: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Value>,
    {
        let mut v: Vec<_> = elements.into_iter().map(Into::into).collect();
        v.shrink_to_fit();
        Value::Vector(v)
    }

    /// Returns true if the value is a String. Returns false otherwise.
    ///
    /// For any Value on which `is_string` returns true, `as_str` is guaranteed
    /// to return the string slice.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let v = sexp!(((a . "some string") (b . #f)));
    ///
    /// assert!(v["a"].is_string());
    ///
    /// // The boolean `false` is not a string.
    /// assert!(!v["b"].is_string());
    /// ```
    pub fn is_string(&self) -> bool {
        self.as_str().is_some()
    }

    /// If the value is a String, returns the associated str. Returns `None`
    /// otherwise.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let v = sexp!(((a . "some string") (b . #f)));
    ///
    /// assert_eq!(v["a"].as_str(), Some("some string"));
    ///
    /// // The boolean `false` is not a string.
    /// assert_eq!(v["b"].as_str(), None);
    ///
    /// // S-expression values are printed in S-expression
    /// // representation, so strings are in quotes.
    /// //    The value is: "some string"
    /// println!("The value is: {}", v["a"]);
    ///
    /// // Rust strings are printed without quotes.
    /// //
    /// //    The value is: some string
    /// println!("The value is: {}", v["a"].as_str().unwrap());
    /// ```
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    /// Returns true if the value is a symbol. Returns false otherwise.
    ///
    /// For any Value on which `is_symbol` returns true, `as_symbol` is guaranteed
    /// to return the string slice.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
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

    /// If the value is a symbol, returns the associated str. Returns `None`
    /// otherwise.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let v = sexp!(foo);
    ///
    /// assert_eq!(v.as_symbol(), Some("foo"));
    /// ```
    pub fn as_symbol(&self) -> Option<&str> {
        match self {
            Value::Symbol(s) => Some(s),
            _ => None,
        }
    }

    /// Returns true if the value is a keyword. Returns false otherwise.
    ///
    /// For any Value on which `is_keyword` returns true, `as_keyword` is guaranteed
    /// to return the string slice.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
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

    /// If the value is a keyword, returns the associated str. Returns `None`
    /// otherwise.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let v = sexp!(#:foo);
    ///
    /// assert_eq!(v.as_keyword(), Some("foo"));
    /// ```
    pub fn as_keyword(&self) -> Option<&str> {
        match self {
            Value::Keyword(s) => Some(s),
            _ => None,
        }
    }

    /// Get the name of a symbol or keyword, or the value of a string.
    ///
    /// This is useful if symbols, keywords and strings need to be treated
    /// equivalently in some context.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let kw = sexp!(#:foo);
    /// assert_eq!(kw.as_name(), Some("foo"));
    ///
    /// let sym = sexp!(bar);
    /// assert_eq!(sym.as_name(), Some("bar"));
    ///
    /// let s = sexp!("baz");
    /// assert_eq!(s.as_name(), Some("baz"));
    /// ```
    pub fn as_name(&self) -> Option<&str> {
        match self {
            Value::Symbol(s) => Some(s),
            Value::Keyword(s) => Some(s),
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    /// Returns true if the value is a byte vector. Returns false otherwise.
    ///
    /// For any Value on which `is_bytes` returns true, `as_bytes` is guaranteed
    /// to return the byte slice.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let v = sexp!(((a . ,(b"some bytes" as &[u8])) (b . "string")));
    ///
    /// assert!(v["a"].is_bytes());
    ///
    /// // A string is not a byte vector.
    /// assert!(!v["b"].is_bytes());
    /// ```
    pub fn is_bytes(&self) -> bool {
        self.as_bytes().is_some()
    }

    /// If the value is a byte vector, returns the associated byte
    /// slice. Returns `None` otherwise.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let v = sexp!(((a . ,(b"some bytes" as &[u8])) (b . "string")));
    ///
    /// assert_eq!(v["a"].as_bytes(), Some(b"some bytes" as &[u8]));
    ///
    /// // A string is not a byte vector.
    /// assert_eq!(v["b"].as_bytes(), None);
    /// ```
    pub fn as_bytes(&self) -> Option<&[u8]> {
        match self {
            Value::Bytes(s) => Some(s),
            _ => None,
        }
    }

    /// Return `true` if the value is a number.
    pub fn is_number(&self) -> bool {
        self.as_number().is_some()
    }

    /// For numbers, return a reference to them. For other values, return
    /// `None`.
    pub fn as_number(&self) -> Option<&Number> {
        match self {
            Value::Number(n) => Some(n),
            _ => None,
        }
    }

    /// Returns true if the value is an integer between `i64::MIN` and
    /// `i64::MAX`.
    ///
    /// For any Value on which `is_i64` returns true, `as_i64` is guaranteed to
    /// return the integer value.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let big = i64::max_value() as u64 + 10;
    /// let v = sexp!(((a . 64) (b . ,big) (c . 256.0)));
    ///
    /// assert!(v["a"].is_i64());
    ///
    /// // Greater than i64::MAX.
    /// assert!(!v["b"].is_i64());
    ///
    /// // Numbers with a decimal point are not considered integers.
    /// assert!(!v["c"].is_i64());
    /// ```
    pub fn is_i64(&self) -> bool {
        match self.as_number() {
            Some(n) => n.is_i64(),
            _ => false,
        }
    }

    /// Returns true if the value is an integer between zero and `u64::MAX`.
    ///
    /// For any Value on which `is_u64` returns true, `as_u64` is guaranteed to
    /// return the integer value.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let v = sexp!(((a . 64) (b . -64) (c . 256.0)));
    ///
    /// assert!(v["a"].is_u64());
    ///
    /// // Negative integer.
    /// assert!(!v["b"].is_u64());
    ///
    /// // Numbers with a decimal point are not considered integers.
    /// assert!(!v["c"].is_u64());
    /// ```
    pub fn is_u64(&self) -> bool {
        match self.as_number() {
            Some(n) => n.is_u64(),
            _ => false,
        }
    }

    /// Returns true if the value is a number that can be represented by f64.
    ///
    /// For any Value on which `is_f64` returns true, `as_f64` is guaranteed to
    /// return the floating point value.
    ///
    /// Currently this function returns true if and only if both `is_i64` and
    /// `is_u64` return false but this is not a guarantee in the future.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let v = sexp!(((a . 256.0) (b . 64) (c . -64)));
    ///
    /// assert!(v["a"].is_f64());
    ///
    /// // Integers.
    /// assert!(!v["b"].is_f64());
    /// assert!(!v["c"].is_f64());
    /// ```
    #[inline]
    pub fn is_f64(&self) -> bool {
        match self.as_number() {
            Some(n) => n.is_f64(),
            _ => false,
        }
    }

    /// If the value is an integer, represent it as i64 if possible. Returns
    /// None otherwise.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let big = i64::max_value() as u64 + 10;
    /// let v = sexp!(((a . 64) (b . ,big) (c . 256.0)));
    ///
    /// assert_eq!(v["a"].as_i64(), Some(64));
    /// assert_eq!(v["b"].as_i64(), None);
    /// assert_eq!(v["c"].as_i64(), None);
    /// ```
    #[inline]
    pub fn as_i64(&self) -> Option<i64> {
        self.as_number().and_then(Number::as_i64)
    }

    /// If the value is an integer, represent it as u64 if possible. Returns
    /// None otherwise.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let v = sexp!(((a . 64) (b . -64) (c . 256.0)));
    ///
    /// assert_eq!(v["a"].as_u64(), Some(64));
    /// assert_eq!(v["b"].as_u64(), None);
    /// assert_eq!(v["c"].as_u64(), None);
    /// ```
    pub fn as_u64(&self) -> Option<u64> {
        self.as_number().and_then(Number::as_u64)
    }

    /// If the value is a number, represent it as f64 if possible. Returns
    /// None otherwise.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let v = sexp!(((a . 256.0) (b . 64) (c . -64)));
    ///
    /// assert_eq!(v["a"].as_f64(), Some(256.0));
    /// assert_eq!(v["b"].as_f64(), Some(64.0));
    /// assert_eq!(v["c"].as_f64(), Some(-64.0));
    /// ```
    pub fn as_f64(&self) -> Option<f64> {
        self.as_number().and_then(Number::as_f64)
    }

    /// Returns true if the value is a Boolean. Returns false otherwise.
    ///
    /// For any Value on which `is_boolean` returns true, `as_bool` is
    /// guaranteed to return the boolean value.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let v = sexp!(((a . #f) (b . #nil)));
    ///
    /// assert!(v["a"].is_boolean());
    ///
    /// // The nil value is special, and not a boolean.
    /// assert!(!v["b"].is_boolean());
    /// ```
    pub fn is_boolean(&self) -> bool {
        self.as_bool().is_some()
    }

    /// If the value is a `Boolean`, returns the associated bool. Returns None
    /// otherwise.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let v = sexp!(((a . #f) (b . "false")));
    ///
    /// assert_eq!(v["a"].as_bool(), Some(false));
    ///
    /// // The string `"false"` is a string, not a boolean.
    /// assert_eq!(v["b"].as_bool(), None);
    /// ```
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    /// Returns true if the value is a character. Returns false otherwise.
    pub fn is_char(&self) -> bool {
        self.as_char().is_some()
    }

    /// If the value is a character, returns the associated `char`. Returns None
    /// otherwise.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let v = sexp!(((a . 'c') (b . "c")));
    ///
    /// assert_eq!(v["a"].as_char(), Some('c'));
    ///
    /// // The string `"c"` is a single-character string, not a character.
    /// assert_eq!(v["b"].as_char(), None);
    /// ```
    pub fn as_char(&self) -> Option<char> {
        match self {
            Value::Char(c) => Some(*c),
            _ => None,
        }
    }

    /// Returns true if the value is `Nil`. Returns false otherwise.
    ///
    /// For any Value on which `is_nil` returns true, `as_nil` is guaranteed
    /// to return `Some(())`.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let v = sexp!(((a . #nil) (b . #f)));
    ///
    /// assert!(v["a"].is_nil());
    ///
    /// // The boolean `false` is not nil.
    /// assert!(!v["b"].is_nil());
    /// ```
    pub fn is_nil(&self) -> bool {
        self.as_nil().is_some()
    }

    /// If the value is `Nil`, returns `()`. Returns `None` otherwise.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let v = sexp!(((a . #nil) (b . #f) (c . ())));
    ///
    /// assert_eq!(v["a"].as_nil(), Some(()));
    ///
    /// // The boolean `false` is not nil.
    /// assert_eq!(v["b"].as_nil(), None);
    /// // Neither is the empty list.
    /// assert_eq!(v["c"].as_nil(), None);
    /// ```
    pub fn as_nil(&self) -> Option<()> {
        match self {
            Value::Nil => Some(()),
            _ => None,
        }
    }

    /// Returns true if the value is `Null`. Returns false otherwise.
    pub fn is_null(&self) -> bool {
        self.as_null().is_some()
    }

    /// If the value is `Null`, returns `()`. Returns `None` otherwise.
    pub fn as_null(&self) -> Option<()> {
        match self {
            Value::Null => Some(()),
            _ => None,
        }
    }

    /// Returns true if the value is a cons cell. Returns `False` otherwise.
    pub fn is_cons(&self) -> bool {
        matches!(self, Value::Cons(_))
    }

    /// If the value is a cons cell, returns a reference to it. Returns `None`
    /// otherwise.
    pub fn as_cons(&self) -> Option<&Cons> {
        match self {
            Value::Cons(pair) => Some(pair),
            _ => None,
        }
    }

    /// If the value is a cons cell, returns a mutable reference to it. Returns
    /// `None` otherwise.
    pub fn as_cons_mut(&mut self) -> Option<&mut Cons> {
        match self {
            Value::Cons(pair) => Some(pair),
            _ => None,
        }
    }

    /// If the value is a cons cell, return references to its `car` and `cdr`
    /// fields.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// let cell = sexp!((foo . bar));
    /// assert_eq!(cell.as_pair(), Some((&sexp!(foo), &sexp!(bar))));
    /// assert_eq!(sexp!("not-a-pair").as_pair(), None);
    /// ```
    pub fn as_pair(&self) -> Option<(&Value, &Value)> {
        self.as_cons().map(Cons::as_pair)
    }

    /// Returns true if the value is a vector.
    pub fn is_vector(&self) -> bool {
        matches!(self, Value::Vector(_))
    }

    /// If the value is a vector, return a reference to its elements.
    ///
    /// ```
    /// # use lexpr::Value;
    /// # use lexpr_macros::sexp;
    /// let v = sexp!(#(1 2 "three"));
    /// let slice: &[Value] = &[sexp!(1), sexp!(2), sexp!("three")];
    /// assert_eq!(v.as_slice(), Some(slice));
    /// ```
    pub fn as_slice(&self) -> Option<&[Value]> {
        match self {
            Value::Vector(elements) => Some(elements),
            _ => None,
        }
    }

    /// If the value is a vector, return a mutable reference to its elements.
    ///
    /// ```
    /// # use lexpr::Value;
    /// # use lexpr_macros::sexp;
    /// let mut v = sexp!(#(1 2 "three"));
    /// v.as_slice_mut().unwrap()[2] = sexp!(3);
    /// let slice: &[Value] = &[sexp!(1), sexp!(2), sexp!(3)];
    /// assert_eq!(v.as_slice(), Some(slice));
    /// ```
    pub fn as_slice_mut(&mut self) -> Option<&mut [Value]> {
        match self {
            Value::Vector(elements) => Some(elements),
            _ => None,
        }
    }

    /// If the value is a list, return an iterator over the list elements.
    ///
    /// If the value is not either a cons cell or `Null`, `None` is returned.
    ////
    /// Note that the returned iterator has special behavior for improper lists, yielding the
    /// element after the dot after returning `None` the first time.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// let value = lexpr::from_str("(1 2 . 3)").unwrap();
    /// let mut iter = value.list_iter().unwrap();
    /// assert_eq!(iter.next(), Some(&sexp!(1)));
    /// assert_eq!(iter.next(), Some(&sexp!(2)));
    /// assert_eq!(iter.next(), None);
    /// assert_eq!(iter.next(), Some(&sexp!(3)));
    /// assert_eq!(iter.next(), None);
    /// ```
    pub fn list_iter(&self) -> Option<cons::ListIter<'_>> {
        match self {
            Value::Cons(cell) => Some(cons::ListIter::cons(cell)),
            Value::Null => Some(cons::ListIter::empty()),
            _ => None,
        }
    }

    /// Attempts conversion to a vector, cloning the values.
    ///
    /// For proper lists (including `Value::Null`), this returns a vector of
    /// values. If you want to handle improper list in a similar way, combine
    /// [`as_cons`] and the [`Cons::to_vec`] method.
    ///
    /// ```
    /// # use lexpr::Value;
    /// # use lexpr_macros::sexp;
    /// assert_eq!(sexp!((1 2 3)).to_vec(), Some(vec![sexp!(1), sexp!(2), sexp!(3)]));
    /// assert_eq!(sexp!(()).to_vec(), Some(vec![]));
    /// assert_eq!(sexp!((1 2 . 3)).to_vec(), None);
    /// ```
    /// [`as_cons`]: Value::as_cons
    pub fn to_vec(&self) -> Option<Vec<Value>> {
        match self {
            Value::Null => Some(Vec::new()),
            Value::Cons(pair) => {
                let (vec, rest) = pair.to_ref_vec();
                if rest.is_null() {
                    Some(vec.into_iter().cloned().collect())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Attempts conversion to a vector, taking references to the values.
    ///
    /// For proper lists (including `Value::Null`), this returns a vector of
    /// value references. If you want to handle improper list in a similar way,
    /// combine [`as_cons`] and the [`Cons::to_ref_vec`] method.
    ///
    /// ```
    /// # use lexpr::Value;
    /// # use lexpr_macros::sexp;
    /// assert_eq!(sexp!((1 2 3)).to_ref_vec(), Some(vec![&sexp!(1), &sexp!(2), &sexp!(3)]));
    /// assert_eq!(sexp!(()).to_ref_vec(), Some(vec![]));
    /// assert_eq!(sexp!((1 2 . 3)).to_ref_vec(), None);
    /// ```
    ///
    /// [`as_cons`]: Value::as_cons
    pub fn to_ref_vec(&self) -> Option<Vec<&Value>> {
        match self {
            Value::Null => Some(Vec::new()),
            Value::Cons(pair) => {
                let (vec, rest) = pair.to_ref_vec();
                if rest.is_null() {
                    Some(vec)
                } else {
                    None
                }
            }
            _ => None,
        }
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
    /// of `assoc-ref` and `list-ref`, depending on the argument type. If
    /// you want to look up a number in an association list, use an
    /// `Value` value containing that number.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let alist = sexp!((("A" . 65) (B . 66) (#:C . 67) (42 . "The answer")));
    /// assert_eq!(alist.get("A").unwrap(), &sexp!(65));
    /// assert_eq!(alist.get("B").unwrap(), &sexp!(66));
    /// assert_eq!(alist.get("C").unwrap(), &sexp!(67));
    /// assert_eq!(alist.get(sexp!(42)).unwrap(), &sexp!("The answer"));
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
    /// # use lexpr_macros::sexp;
    /// #
    /// let alist = sexp!((
    ///     ("A" . ("a" "á" "à"))
    ///     ("B" . ((b . 42) (c . 23)))
    ///     ("C" . ("c" "ć" "ć̣" "ḉ"))
    /// ));
    /// assert_eq!(alist["B"][0], sexp!((b . 42)));
    /// assert_eq!(alist["C"][1], sexp!("ć"));
    ///
    /// assert_eq!(alist["D"], sexp!(#nil));
    /// assert_eq!(alist[0]["x"]["y"]["z"], sexp!(#nil));
    /// ```
    ///
    /// [`Index`]: trait.Index.html
    pub fn get<I: Index>(&self, index: I) -> Option<&Value> {
        index.index_into(self)
    }
}

struct WriterFormatter<'a, 'b> {
    inner: &'a mut fmt::Formatter<'b>,
}

impl<'a, 'b> io::Write for WriterFormatter<'a, 'b> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        fn io_error<E>(_: E) -> io::Error {
            // Sexp does not matter because fmt::Debug and fmt::Display impls
            // below just map it to fmt::Error
            io::Error::new(io::ErrorKind::Other, "fmt error")
        }
        let s = str::from_utf8(buf).map_err(io_error)?;
        self.inner.write_str(s).map_err(io_error)?;
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl fmt::Display for Value {
    /// Display an S-expression value as a string.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// #
    /// let value = sexp!(((city "London") (street "10 Downing Street")));
    ///
    /// // Compact format:
    /// //
    /// // ((city "London") (street "10 Downing Street"))
    /// let compact = format!("{}", value);
    /// assert_eq!(compact,
    ///     r#"((city "London") (street "10 Downing Street"))"#);
    /// ```
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut wr = WriterFormatter { inner: f };
        crate::print::to_writer(&mut wr, self).map_err(|_| fmt::Error)
    }
}

impl str::FromStr for Value {
    /// Parse an S-expression value from a string.
    type Err = crate::parse::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        crate::parse::from_str(s)
    }
}

mod from;
mod index;
mod partial_eq;

#[cfg(test)]
mod tests;
