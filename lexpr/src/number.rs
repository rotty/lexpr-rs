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
    // The contained value is guaranteed not to fit in a `u64`.
    PosInt128(u128),
    // The contained value is guaranteed to be negative.
    NegInt(i64),
    // The contained value is guaranteed to be negative and not to fit in a
    // `i64`.
    NegInt128(i128),
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
    /// # use lexpr::Number;
    /// #
    /// assert!(Number::from(64).is_i64());
    /// // Greater than i64::MAX.
    /// assert!(!Number::from(i64::MAX as u64 + 10).is_i64());
    /// // Floats are not considered integers.
    /// assert!(!Number::from(256.0).is_i64());
    /// ```
    #[inline]
    pub fn is_i64(&self) -> bool {
        match self.n {
            N::PosInt(v) => v <= i64::MAX as u64,
            N::NegInt(_) => true,
            N::NegInt128(_) | N::PosInt128(_) | N::Float(_) => false,
        }
    }

    /// Returns true if the `Number` is an integer between zero and `u64::MAX`.
    ///
    /// For any `Number` on which `is_u64` returns true, `as_u64` is guaranteed
    /// to return the integer value.
    ///
    /// ```
    /// # use lexpr::Number;
    ///
    /// assert!(Number::from(64).is_u64());
    ///
    /// // Negative integer.
    /// assert!(!Number::from(-64).is_u64());
    ///
    /// // Floats are not considered integers.
    /// assert!(!Number::from(256.0).is_u64());
    /// ```
    #[inline]
    pub fn is_u64(&self) -> bool {
        match self.n {
            N::PosInt(_) => true,
            N::NegInt(_) | N::NegInt128(_) | N::PosInt128(_) | N::Float(_) => false,
        }
    }

    /// Returns true if the `Number` is an integer between zero and `u128::MAX`.
    ///
    /// For any Number on which `is_u128` returns true, `as_u128` is guaranteed to
    /// return the integer value.
    ///
    /// ```
    /// # use lexpr::Number;
    /// #
    /// assert!(Number::from(64).is_u128());
    ///
    /// // Negative integer.
    /// assert!(!Number::from(-64).is_u128());
    ///
    /// // Floats are not considered integers.
    /// assert!(!Number::from(265.0).is_u128());
    ///
    /// assert!(Number::from(u128::MAX).is_u128());
    /// ```
    #[inline]
    pub fn is_u128(&self) -> bool {
        match self.n {
            N::PosInt(_) | N::PosInt128(_) => true,
            N::NegInt(_) | N::NegInt128(_) | N::Float(_) => false,
        }
    }

    /// Returns true if the `Number` can be represented by f64.
    ///
    /// For any Number on which `is_f64` returns true, `as_f64` is guaranteed to
    /// return the floating point value.
    ///
    /// This function returns true if and only if the stored value is a floating
    /// point number, i.e., all of the `is_i64`, `is_u64`, `is_i128` and
    /// `is_u128` return false.
    ///
    /// ```
    /// # use lexpr::Number;
    /// #
    /// assert!(Number::from(256.0).is_f64());
    ///
    /// // Integers.
    /// assert!(!Number::from(64).is_f64());
    /// assert!(!Number::from(-64).is_f64());
    /// ```
    #[inline]
    pub fn is_f64(&self) -> bool {
        match self.n {
            N::Float(_) => true,
            N::PosInt(_) | N::NegInt(_) | N::PosInt128(_) | N::NegInt128(_) => false,
        }
    }

    /// If the `Number` is an integer, represent it as i64 if possible. Returns
    /// None otherwise.
    ///
    /// ```
    /// # use lexpr::Number;
    /// #
    /// assert_eq!(Number::from(64).as_i64(), Some(64));
    /// assert_eq!(Number::from(i64::MAX as u64 + 10).as_i64(), None);
    /// assert_eq!(Number::from(256.0).as_i64(), None);
    /// ```
    #[inline]
    pub fn as_i64(&self) -> Option<i64> {
        match self.n {
            N::PosInt(n) => {
                if n <= i64::MAX as u64 {
                    Some(n as i64)
                } else {
                    None
                }
            }
            N::NegInt(n) => Some(n),
            N::Float(_) | N::NegInt128(_) | N::PosInt128(_) => None,
        }
    }

    /// If the `Number` is an integer, represent it as i128 if possible. Returns
    /// None otherwise.
    ///
    /// ```
    /// # use lexpr::Number;
    /// #
    /// assert_eq!(Number::from(64).as_i128(), Some(64));
    /// assert_eq!(Number::from(i128::MAX).as_i128(), Some(i128::MAX));
    /// assert_eq!(Number::from(256.0).as_i128(), None);
    /// assert_eq!(Number::from(i128::MIN).as_i128(), Some(i128::MIN));
    /// ```
    #[inline]
    pub fn as_i128(&self) -> Option<i128> {
        match self.n {
            N::PosInt(n) => Some(i128::from(n)),
            N::PosInt128(n) => {
                if n <= i128::MAX as u128 {
                    Some(n as i128)
                } else {
                    None
                }
            }
            N::NegInt(n) => Some(i128::from(n)),
            N::NegInt128(n) => Some(n),
            N::Float(_) => None,
        }
    }
    /// If the `Number` is an integer, represent it as u64 if possible. Returns
    /// None otherwise.
    ///
    /// ```
    /// # use lexpr::Number;
    /// #
    /// assert_eq!(Number::from(64).as_u64(), Some(64));
    /// assert_eq!(Number::from(-64).as_u64(), None);
    /// assert_eq!(Number::from(256.0).as_u64(), None);
    /// ```
    #[inline]
    pub fn as_u64(&self) -> Option<u64> {
        match self.n {
            N::PosInt(n) => Some(n),
            N::NegInt(_) | N::NegInt128(_) | N::PosInt128(_) | N::Float(_) => None,
        }
    }

    /// If the `Number` is an integer, represent it as u128 if possible. Returns
    /// None otherwise.
    ///
    /// ```
    /// # use lexpr::Number;
    /// #
    /// assert_eq!(Number::from(64).as_u128(), Some(64));
    /// assert_eq!(Number::from(-64).as_u128(), None);
    /// assert_eq!(Number::from(256.0).as_u128(), None);
    /// assert_eq!(Number::from(u128::MAX).as_u128(), Some(u128::MAX));
    /// ```
    #[inline]
    pub fn as_u128(&self) -> Option<u128> {
        match self.n {
            N::PosInt(n) => Some(u128::from(n)),
            N::PosInt128(n) => Some(n),
            N::NegInt(_) | N::NegInt128(_) | N::Float(_) => None,
        }
    }

    /// Represents the number as f64 if possible. Returns None otherwise.
    ///
    /// ```
    /// # use lexpr::Number;
    /// #
    /// assert_eq!(Number::from(256.0).as_f64(), Some(256.0));
    /// assert_eq!(Number::from(64).as_f64(), Some(64.0));
    /// assert_eq!(Number::from(-64).as_f64(), Some(-64.0));
    /// ```
    #[inline]
    pub fn as_f64(&self) -> Option<f64> {
        match self.n {
            N::PosInt(n) => Some(n as f64),
            N::NegInt(n) => Some(n as f64),
            N::PosInt128(n) => Some(n as f64),
            N::NegInt128(n) => Some(n as f64),
            N::Float(n) => Some(n),
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
            N::PosInt128(n) => visitor.visit_u128(n),
            N::NegInt(n) => visitor.visit_i64(n),
            N::NegInt128(n) => visitor.visit_i128(n),
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
    /// The stored value is `u128`.
    fn visit_u128(self, n: u128) -> Result<Self::Value, Self::Error>
    where
        Self: Sized,
    {
        let _ = n;
        Err(Self::error("u128 not supported"))
    }
    /// The stored value is `i128`.
    fn visit_i128(self, n: i128) -> Result<Self::Value, Self::Error>
    where
        Self: Sized,
    {
        let _ = n;
        Err(Self::error("i128 not supported"))
    }
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

impl From<u128> for Number {
    #[inline]
    fn from(n: u128) -> Self {
        let n = if n <= u128::from(u64::MAX) {
            N::PosInt(n as u64)
        } else {
            N::PosInt128(n)
        };
        Number { n }
    }
}

impl From<i128> for Number {
    #[inline]
    fn from(n: i128) -> Self {
        let n = if n >= 0 {
            if n <= i128::from(u64::MAX) {
                N::PosInt(n as u64)
            } else {
                N::PosInt128(n as u128)
            }
        } else {
            if n >= i128::from(i64::MIN) {
                N::NegInt(n as i64)
            } else {
                N::NegInt128(n)
            }
        };
        Number { n }
    }
}

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
            N::PosInt128(i) => Display::fmt(&i, formatter),
            N::NegInt128(i) => Display::fmt(&i, formatter),
            N::Float(f) => Display::fmt(&f, formatter),
        }
    }
}

impl Debug for Number {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.n, formatter)
    }
}
