//! Dynamically typed number type.

use std::fmt::{self, Debug, Display};

/// Represents an S-expression number, whether integer or floating point.
#[derive(PartialEq, Clone)]
pub struct Number {
    n: N,
}

#[derive(Debug, PartialEq, Clone)]
enum N {
    PosInt(u64),
    NegInt(i64),
    Float(f64),
}

impl Number {
    /// Returns true if the `Number` is an integer between `i64::MIN` and
    /// `i64::MAX`.
    ///
    /// For any `Number` on which `is_i64` returns true, `as_i64` is
    /// guaranteed to return the integer value.
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
    #[inline]
    pub fn is_i64(&self) -> bool {
        match self.n {
            N::PosInt(v) => v <= i64::max_value() as u64,
            N::NegInt(_) => true,
            N::Float(_) => false,
        }
    }

    /// Returns true if the `Number` is an integer between zero and `u64::MAX`.
    ///
    /// For any Number on which `is_u64` returns true, `as_u64` is guaranteed to
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
    #[inline]
    pub fn is_u64(&self) -> bool {
        match self.n {
            N::PosInt(_) => true,
            N::NegInt(_) | N::Float(_) => false,
        }
    }

    /// Returns true if the `Number` can be represented by f64.
    ///
    /// For any Number on which `is_f64` returns true, `as_f64` is guaranteed to
    /// return the floating point value.
    ///
    /// Currently this function returns true if and only if both `is_i64` and
    /// `is_u64` return false but this is not a guarantee in the future.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// let v = sexp!(((a . 256.0) (b . 64) (c . -64)));
    /// assert!(v["a"].is_f64());
    ///
    /// // Integers.
    /// assert!(!v["b"].is_f64());
    /// assert!(!v["c"].is_f64());
    /// ```
    #[inline]
    pub fn is_f64(&self) -> bool {
        match self.n {
            N::Float(_) => true,
            N::PosInt(_) | N::NegInt(_) => false,
        }
    }

    /// If the `Number` is an integer, represent it as i64 if possible. Returns
    /// None otherwise.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// let big = i64::max_value() as u64 + 10;
    /// let v = sexp!(((a . 64) (b . ,big) (c . 256.0)));
    ///
    /// assert_eq!(v["a"].as_i64(), Some(64));
    /// assert_eq!(v["b"].as_i64(), None);
    /// assert_eq!(v["c"].as_i64(), None);
    /// ```
    #[inline]
    pub fn as_i64(&self) -> Option<i64> {
        match self.n {
            N::PosInt(n) => {
                if n <= i64::max_value() as u64 {
                    Some(n as i64)
                } else {
                    None
                }
            }
            N::NegInt(n) => Some(n),
            N::Float(_) => None,
        }
    }

    /// If the `Number` is an integer, represent it as u64 if possible. Returns
    /// None otherwise.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    /// let v = sexp!(((a . 64) (b . -64) (c . 256.0)));
    ///
    /// assert_eq!(v["a"].as_u64(), Some(64));
    /// assert_eq!(v["b"].as_u64(), None);
    /// assert_eq!(v["c"].as_u64(), None);
    /// ```
    #[inline]
    pub fn as_u64(&self) -> Option<u64> {
        match self.n {
            N::PosInt(n) => Some(n),
            N::NegInt(_) | N::Float(_) => None,
        }
    }

    /// Represents the number as f64 if possible. Returns None otherwise.
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
    #[inline]
    pub fn as_f64(&self) -> Option<f64> {
        match self.n {
            N::PosInt(n) => Some(n as f64),
            N::NegInt(n) => Some(n as f64),
            N::Float(n) => Some(n),
        }
    }

    /// Converts a finite `f64` to a `Number`. Infinite or NaN values
    /// are not S-expression numbers.
    ///
    /// ```
    /// # use std::f64;
    /// #
    /// # use lexpr::Number;
    /// #
    /// assert!(Number::from_f64(256.0).is_some());
    ///
    /// assert!(Number::from_f64(f64::NAN).is_none());
    /// ```
    #[inline]
    pub fn from_f64(f: f64) -> Option<Number> {
        if f.is_finite() {
            Some(Number { n: N::Float(f) })
        } else {
            None
        }
    }

    /// Dispatch based on the type of the contained value.
    ///
    /// Depending on the stored value, one of the functions of the
    /// supplied visitor will be called.
    pub fn visit<V>(&self, visitor: V) -> Result<V::Value, V::Error>
    where
        V: Visitor,
    {
        match self.n {
            N::PosInt(n) => visitor.visit_u64(n),
            N::NegInt(n) => visitor.visit_i64(n),
            N::Float(n) => visitor.visit_f64(n),
        }
    }
}

/// Trait to access the value stored in `Number`.
///
/// The `Number` type does not directly expose its internal
/// structure to allow future changes without breaking the API.
///
/// Instead, you can implement this trait and pass your implementation
/// to `Number::visit`.
///
/// [`Number::visit`]: struct.Number.html#method.visit
pub trait Visitor {
    /// The return type of the visitor methods.
    type Value;
    /// The error type of the visitor methods.
    type Error;

    /// Construct an error given a message.
    ///
    /// This method is used by trait default implementations.
    fn error<T: Into<String>>(msg: T) -> Self::Error;

    /// The stored value is a `u64`.
    fn visit_u64(self, n: u64) -> Result<Self::Value, Self::Error>;
    /// The stored value is an `i64`.
    fn visit_i64(self, n: i64) -> Result<Self::Value, Self::Error>;
    /// The stored value is `f64`.
    fn visit_f64(self, n: f64) -> Result<Self::Value, Self::Error>;
}

macro_rules! impl_from_unsigned {
    (
        $($ty:ty),*
    ) => {
        $(
            impl From<$ty> for Number {
                #[inline]
                fn from(u: $ty) -> Self {
                    Number { n: N::PosInt(u64::from(u)) }
                }
            }
        )*
    };
}

macro_rules! impl_from_signed {
    (
        $($ty:ty),*
    ) => {
        $(
            impl From<$ty> for Number {
                #[inline]
                fn from(n: $ty) -> Self {
                    let n = if n >= 0 {
                        N::PosInt(n as u64)
                    } else {
                        N::NegInt(i64::from(n))
                    };
                    Number { n }
                }
            }
        )*
    };
}

impl_from_unsigned!(u8, u16, u32, u64);
impl_from_signed!(i8, i16, i32, i64);

impl From<f32> for Number {
    #[inline]
    fn from(n: f32) -> Self {
        Number {
            n: N::Float(f64::from(n)),
        }
    }
}

impl From<f64> for Number {
    #[inline]
    fn from(n: f64) -> Self {
        Number { n: N::Float(n) }
    }
}

impl Display for Number {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.n {
            N::PosInt(i) => Display::fmt(&i, formatter),
            N::NegInt(i) => Display::fmt(&i, formatter),
            N::Float(f) => Display::fmt(&f, formatter),
        }
    }
}

impl Debug for Number {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.n, formatter)
    }
}
