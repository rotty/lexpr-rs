//! List "cons cell" data type and accompanying iterator types.
use std::fmt;

use crate::Value;

/// A Lisp "cons cell".
///
/// A cons cell is similiar to a two-element tuple in Rust. Its fields
/// are traditionally called `car` and `cdr`, for obscure historical
/// reasons. Both the `car` and the `cdr` field can hold any `Value`,
/// including other cons cells.
///
/// This data type is used to represent singly-linked lists, by
/// forming a chain of cons cells where the list element is kept in
/// the `car` field, and the `cdr` field either points to the next
/// cons cell, or terminates the list with any other value. Usually,
/// that terminator value is `Value::Null`, also referred to as the
/// empty list. If any other terminating value is used, the resulting
/// linked list is referred to as "dotted", or "improper" list.
///
/// The `Cons` data type provides some utility function for the
/// singly-linked list use case, such as iterating through the list or
/// converting the list to a vector. To account for the possibility of
/// dotted lists, the iterators and vector conversion functions have
/// slightly unusual types.
///
/// The most natural way to traverse a singly linked list is probably by using
/// the `list_iter` method.
#[derive(PartialEq, Clone)]
pub struct Cons {
    inner: Box<(Value, Value)>,
}

impl fmt::Debug for Cons {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "({:?} . {:?})", self.car(), self.cdr())
    }
}

impl Cons {
    /// Constructs a new cons cell from two values.
    pub fn new<T, U>(car: T, cdr: U) -> Self
    where
        T: Into<Value>,
        U: Into<Value>,
    {
        Cons {
            inner: Box::new((car.into(), cdr.into())),
        }
    }

    /// Returns a reference to the value in the `car` field.
    pub fn car(&self) -> &Value {
        &self.inner.0
    }

    /// Returns a mutable reference to the value in the `car` field.
    pub fn car_mut(&mut self) -> &mut Value {
        &mut self.inner.0
    }

    /// Sets the `car` field.
    pub fn set_car(&mut self, car: impl Into<Value>) {
        self.inner.0 = car.into()
    }

    /// Returns a reference to the value in the `cdr` field.
    pub fn cdr(&self) -> &Value {
        &self.inner.1
    }

    /// Returns a mutable reference to the value in the `cdr` field.
    pub fn cdr_mut(&mut self) -> &mut Value {
        &mut self.inner.1
    }

    /// Sets the `cdr` field.
    pub fn set_cdr(&mut self, cdr: impl Into<Value>) {
        self.inner.1 = cdr.into()
    }

    /// Returns references to the values in the `car` and `cdr` fields.
    ///
    /// ```
    /// # use lexpr::{Cons, Value};
    /// let cell = Cons::new(1, 2);
    /// assert_eq!(cell.as_pair(), (&Value::from(1), &Value::from(2)));
    /// ```
    pub fn as_pair(&self) -> (&Value, &Value) {
        (&self.inner.0, &self.inner.1)
    }

    /// Converts `self` into a pair of values without cloning.
    ///
    /// ```
    /// # use lexpr::Cons;
    /// let cell = Cons::new("a", 42);
    /// assert_eq!(cell.car(), "a");
    /// assert_eq!(cell.cdr(), 42);
    /// let (car, cdr) = cell.into_pair();
    /// assert_eq!(car, "a");
    /// assert_eq!(cdr, 42);
    /// ```
    pub fn into_pair(self) -> (Value, Value) {
        (self.inner.0, self.inner.1)
    }

    /// Obtains an iterator yielding references to all the cons cells in this
    /// linked list.
    ///
    /// ```
    /// # use lexpr::{Cons, Value};
    /// for cell in Cons::new(1, Cons::new(2, Value::Null)).iter() {
    ///    println!("list element: {}", cell.car());
    /// }
    /// ```
    pub fn iter(&self) -> Iter<'_> {
        Iter { cursor: Some(self) }
    }

    /// Converts `self` into a vector without cloning the elements.
    ///
    /// Returns the accumulated items of the list and the `cdr` of the last list
    /// element. For proper lists, this will always be `Value::Null`.
    ///
    /// ```
    /// # use lexpr::{Cons, Value};
    /// let list = Cons::new(1, Cons::new(2, Cons::new(3, Value::Null)));
    /// assert_eq!(list.into_vec(), (vec![Value::from(1), Value::from(2), Value::from(3)], Value::Null));
    /// ```
    pub fn into_vec(self) -> (Vec<Value>, Value) {
        let mut vec = Vec::new();
        for (item, rest) in self.into_iter() {
            vec.push(item);
            if let Some(rest) = rest {
                return (vec, rest);
            }
        }
        unreachable!()
    }

    /// Retrieves a vector, cloning the values.
    ///
    /// Returns the accumulated items of the list and the `cdr` of the last list
    /// element. For proper lists, this will always be `Value::Null`.
    ///
    /// ```
    /// # use lexpr::{Cons, Value};
    /// let list = Cons::new(1, Cons::new(2, Cons::new(3, Value::Null)));
    /// assert_eq!(list.to_vec(), (vec![Value::from(1), Value::from(2), Value::from(3)], Value::Null));
    /// ```
    pub fn to_vec(&self) -> (Vec<Value>, Value) {
        let mut vec = Vec::new();
        for pair in self.iter() {
            vec.push(pair.car().clone());
            if !pair.cdr().is_cons() {
                return (vec, pair.cdr().clone());
            }
        }
        unreachable!()
    }

    /// Retrieves a vector, taking references to the values.
    ///
    /// Returns the accumulated items of the list and the `cdr` of the last list
    /// element. For proper lists, this will always be `Value::Null`.
    ///
    /// ```
    /// # use lexpr::{Cons, Value};
    /// let list = Cons::new(1, Cons::new(2, Cons::new(3, Value::Null)));
    /// assert_eq!(list.to_ref_vec(), (vec![&Value::from(1), &Value::from(2), &Value::from(3)], &Value::Null));
    /// ```
    pub fn to_ref_vec(&self) -> (Vec<&Value>, &Value) {
        let mut vec = Vec::new();
        for pair in self.iter() {
            vec.push(pair.car());
            if !pair.cdr().is_cons() {
                return (vec, pair.cdr());
            }
        }
        unreachable!()
    }

    /// Returns an iterator that returns each element (`car` field) of a singly-linked list.
    ///
    /// The iterator returns `None` if a terminating value is encountered. For a
    /// dotted list, the iterator is not yet exhausted at that point, and
    /// produces the non-`Null` terminating value next.
    pub fn list_iter(&self) -> ListIter<'_> {
        ListIter::cons(self)
    }
}

impl IntoIterator for Cons {
    type Item = (Value, Option<Value>);
    type IntoIter = IntoIter;

    /// Obtains an iterator yielding the contents of the elements of this linked
    /// list.
    ///
    /// The returned iterator transfers ownership of the values contained in the
    /// list to the consumer of the iterator. For each cons cell but the last,
    /// the iterator yields a pair containing the value in the cell's `car`
    /// field and `None`. For the last cell, the yielded pair will contain the
    /// value of `car` and `Some(cdr)`.
    //
    /// ```
    /// # use lexpr::{Cons, Value};
    /// let vec: Vec<_> = Cons::new(1, Cons::new(2, 3)).into_iter().collect();
    /// assert_eq!(vec, vec![(Value::from(1), None), (Value::from(2), Some(Value::from(3)))]);
    /// ```
    fn into_iter(self) -> IntoIter {
        IntoIter { cursor: Some(self) }
    }
}

impl<'a> IntoIterator for &'a Cons {
    type Item = &'a Cons;
    type IntoIter = Iter<'a>;

    fn into_iter(self) -> Iter<'a> {
        self.iter()
    }
}

/// An iterator over a chain of cons cells.
///
/// This is returned by the [`Cons::iter`] method.
pub struct Iter<'a> {
    cursor: Option<&'a Cons>,
}

impl<'a> Iter<'a> {
    /// Returns the current cons cell, without advancing the iterator.
    pub fn peek(&self) -> Option<&Cons> {
        self.cursor
    }
}

impl<'a> Iterator for Iter<'a> {
    type Item = &'a Cons;

    fn next(&mut self) -> Option<Self::Item> {
        match self.cursor {
            Some(pair) => {
                match pair.cdr() {
                    Value::Cons(pair) => self.cursor = Some(pair),
                    _ => self.cursor = None,
                }
                Some(pair)
            }
            None => None,
        }
    }
}

/// An iterator consuming a chain of cons cells.
///
/// This is returned by the [`Cons::into_iter`] method.
///
/// [`Cons::into_iter`]: struct.Cons.html#method.into_iter
pub struct IntoIter {
    cursor: Option<Cons>,
}

impl IntoIter {
    /// Returns the current cons cell, without advancing the iterator.
    pub fn peek(&self) -> Option<&Cons> {
        self.cursor.as_ref()
    }

    /// Returns a mutable reference to the current cons cell, without advancing
    /// the iterator.
    pub fn peek_mut(&mut self) -> Option<&mut Cons> {
        self.cursor.as_mut()
    }
}

impl Iterator for IntoIter {
    type Item = (Value, Option<Value>);

    fn next(&mut self) -> Option<Self::Item> {
        match self.cursor.take() {
            Some(cell) => {
                let (car, cdr) = cell.into_pair();
                match cdr {
                    Value::Cons(cell) => {
                        self.cursor = Some(cell);
                        Some((car, None))
                    }
                    _ => {
                        self.cursor = None;
                        Some((car, Some(cdr)))
                    }
                }
            }
            None => None,
        }
    }
}

/// An iterator yielding the `car` field of a chain of cons cells.
///
/// # Improper lists
///
/// Since in Lisp, lists can be "improper", i.e., terminated by a value other than `Null`, this
/// iterator type takes advantage of the fact that Rust's iterators can produce multiple sequences
/// of values, each terminated by `None`. For an improper list, the terminating value is produced
/// after the sequence of elements, as a singleton element, again followed by `None`.
///
/// For example, while the list `(1 2 3)` will produce the three expected `Some` values, followed by
/// `None`, the list `(1 2 . 3)` will produce `Some` values for `1` and `2`, then a `None`, followed
/// by a some value for `3`, and then the final `None`.
#[derive(Debug, Clone)]
pub struct ListIter<'a>(ListCursor<'a>);

#[derive(Debug, Clone)]
enum ListCursor<'a> {
    Cons(&'a Cons),
    Dot(&'a Value),
    Rest(&'a Value),
    Exhausted,
}

impl<'a> ListIter<'a> {
    /// Returns true when the iterator is completely exhausted.
    ///
    /// For an improper list, true will only be returned after the terminating value has been
    /// consumed.
    pub fn is_empty(&self) -> bool {
        match &self.0 {
            ListCursor::Exhausted => true,
            _ => false,
        }
    }

    /// Returns a peek at the value that would be returned by a call to `next`.
    ///
    /// For improper lists, this implies that after the last regular element, `None` will be
    /// returned, while `is_empty` still returns false at that point.
    pub fn peek(&self) -> Option<&Value> {
        match &self.0 {
            ListCursor::Cons(cell) => Some(cell.car()),
            ListCursor::Dot(_) => None,
            ListCursor::Rest(value) => Some(value),
            ListCursor::Exhausted => None,
        }
    }

    pub(crate) fn empty() -> Self {
        ListIter(ListCursor::Exhausted)
    }

    pub(crate) fn cons(cell: &'a Cons) -> Self {
        ListIter(ListCursor::Cons(cell))
    }
}

impl<'a> Iterator for ListIter<'a> {
    type Item = &'a Value;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0 {
            ListCursor::Cons(cell) => {
                let car = cell.car();
                match cell.cdr() {
                    Value::Cons(next) => {
                        self.0 = ListCursor::Cons(next);
                    }
                    Value::Null => {
                        self.0 = ListCursor::Exhausted;
                    }
                    cdr => {
                        self.0 = ListCursor::Dot(cdr);
                    }
                }
                Some(car)
            }
            ListCursor::Dot(value) => {
                self.0 = ListCursor::Rest(value);
                None
            }
            ListCursor::Rest(value) => {
                self.0 = ListCursor::Exhausted;
                Some(value)
            }
            ListCursor::Exhausted => None,
        }
    }
}
