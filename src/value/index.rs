use std::ops;

use crate::Atom;
use crate::Value;

/// A type that can be used to index into a `lexpr::Value`.
///
/// The [`get`] and [`get_mut`] methods of `Value` accept any type
/// that implements `Index`, as does the [square-bracket indexing
/// operator]. This trait is implemented for strings and `Value`, both
/// of which can be used to index into the index into association
/// lists, and for `usize` which is used to index into to lists by
/// element index.
///
/// Note that improper lists are only indexable by `usize`, not by
/// strings.
///
/// [`get`]: ../enum.Value.html#method.get
/// [`get_mut`]: ../enum.Value.html#method.get_mut
/// [square-bracket indexing operator]: ../enum.Value.html#impl-Index%3CI%3E
///
/// This trait is sealed and cannot be implemented for types outside
/// of `lexpr`.
///
/// # Examples
///
/// ```
/// # use lexpr::sexp;
/// #
/// let data = sexp!(((foo 42) (bar . (1 2 3))));
///
/// // Data is an association list so it can be indexed with a string.
/// let bar = &data["bar"];
///
/// // Bar is a list so it can be indexed with an integer.
/// let first = &bar[1];
///
/// assert_eq!(first, 1);
/// ```
pub trait Index: private::Sealed {
    /// Return None if the key is not already in the array or object.
    #[doc(hidden)]
    fn index_into<'v>(&self, v: &'v Value) -> Option<&'v Value>;

    /// Return None if the key is not already in the array or object.
    #[doc(hidden)]
    fn index_into_mut<'v>(&self, v: &'v mut Value) -> Option<&'v mut Value>;
}

// Prevent users from implementing the Index trait.
mod private {
    pub trait Sealed {}
    impl Sealed for usize {}
    impl Sealed for str {}
    impl Sealed for String {}
    impl<'a, T: ?Sized> Sealed for &'a T where T: Sealed {}
    impl Sealed for super::Value {}
}

impl Index for usize {
    fn index_into<'v>(&self, v: &'v Value) -> Option<&'v Value> {
        match *v {
            Value::List(ref vec) => vec.get(*self),
            Value::ImproperList(ref vec, _) => vec.get(*self),
            _ => None,
        }
    }
    fn index_into_mut<'v>(&self, v: &'v mut Value) -> Option<&'v mut Value> {
        match *v {
            Value::List(ref mut vec) => vec.get_mut(*self),
            Value::ImproperList(ref mut vec, _) => vec.get_mut(*self),
            _ => None,
        }
    }
}

fn match_pair_name(name: &str, pair: &Value) -> bool {
    match pair {
        Value::List(elements) => !elements.is_empty() && elements[0].as_name() == Some(name),
        Value::ImproperList(elements, _) => {
            !elements.is_empty() && elements[0].as_name() == Some(name)
        }
        _ => false,
    }
}

impl Index for str {
    fn index_into<'v>(&self, v: &'v Value) -> Option<&'v Value> {
        match v {
            Value::List(elements) => elements.iter().find(|e| match_pair_name(self, e)),
            _ => None,
        }
    }
    fn index_into_mut<'v>(&self, v: &'v mut Value) -> Option<&'v mut Value> {
        match v {
            Value::List(elements) => elements.iter_mut().find(|e| match_pair_name(self, e)),
            _ => None,
        }
    }
}

impl Index for String {
    fn index_into<'v>(&self, v: &'v Value) -> Option<&'v Value> {
        self[..].index_into(v)
    }
    fn index_into_mut<'v>(&self, v: &'v mut Value) -> Option<&'v mut Value> {
        self[..].index_into_mut(v)
    }
}

impl<'a, T: ?Sized> Index for &'a T
where
    T: Index,
{
    fn index_into<'v>(&self, v: &'v Value) -> Option<&'v Value> {
        (**self).index_into(v)
    }
    fn index_into_mut<'v>(&self, v: &'v mut Value) -> Option<&'v mut Value> {
        (**self).index_into_mut(v)
    }
}

fn match_pair_key(value: &Value, pair: &Value) -> bool {
    match pair {
        Value::List(elements) => !elements.is_empty() && &pair[0] == value,
        Value::ImproperList(elements, _) => !elements.is_empty() && &pair[0] == value,
        _ => false,
    }
}

impl Index for Value {
    fn index_into<'v>(&self, v: &'v Value) -> Option<&'v Value> {
        match v {
            Value::List(elements) => elements.iter().find(|e| match_pair_key(self, e)),
            _ => None,
        }
    }
    fn index_into_mut<'v>(&self, v: &'v mut Value) -> Option<&'v mut Value> {
        match v {
            Value::List(elements) => elements.iter_mut().find(|e| match_pair_key(self, e)),
            _ => None,
        }
    }
}

// The usual semantics of Index is to panic on invalid indexing.
//
// That said, the usual semantics are for things like Vec and BTreeMap which
// have different use cases than Value. If you are working with a Vec, you know
// that you are working with a Vec and you can get the len of the Vec and make
// sure your indices are within bounds. The Value use cases are more
// loosey-goosey. You got some JSON from an endpoint and you want to pull values
// out of it. Outside of this Index impl, you already have the option of using
// value.as_array() and working with the Vec directly, or matching on
// Value::Array and getting the Vec directly. The Index impl means you can skip
// that and index directly into the thing using a concise syntax. You don't have
// to check the type, you don't have to check the len, it is all about what you
// expect the Value to look like.
//
// Basically the use cases that would be well served by panicking here are
// better served by using one of the other approaches: get and get_mut,
// as_array, or match. The value of this impl is that it adds a way of working
// with Value that is not well served by the existing approaches: concise and
// careless and sometimes that is exactly what you want.
impl<I> ops::Index<I> for Value
where
    I: Index,
{
    type Output = Value;

    /// Index into a `lexpr::Value` using the syntax `value[0]` or
    /// `value["k"]`.
    ///
    /// Returns the nil value if the type of `self` does not match the
    /// type of the index, for example if the index is a string and
    /// `self` is not an association list. Also returns the nil value
    /// if the given key does not exist in the assication list or the
    /// given index is not within the bounds of the list.
    ///
    /// Note that repeatedly indexing with a string is not possible,
    /// as the indexing operation returns the found association list
    /// entry, which is not an association list itself. This behavior,
    /// i.e. returning the whole entry including the key is due to the
    /// design decison of representing lists as Rust vectors.
    ///
    /// # Examples
    ///
    /// ```
    /// # use lexpr::sexp;
    /// #
    /// let data = sexp!(((a . 42) (x . (y (z zz)))));
    ///
    /// assert_eq!(data["x"], sexp!((x . (y (z zz)))));
    ///
    /// assert_eq!(data["a"], sexp!((a . 42))); // returns nil for undefined values
    /// assert_eq!(data["b"], sexp!(#nil)); // does not panic
    /// ```
    fn index(&self, index: I) -> &Value {
        static NIL: Value = Value::Atom(Atom::Nil);
        index.index_into(self).unwrap_or(&NIL)
    }
}
