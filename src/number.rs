//! Dynamically typed number type.

/// Represents an S-expression number, whether integer or floating point.
#[derive(Debug, PartialEq, Clone)]
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
    /// # use lexpr::sexp;
    /// #
    /// let big = i64::max_value() as u64 + 10;
    /// let v = sexp!(((a 64) (b ,big) (c 256.0)));
    ///
    /// assert_eq!(v["a"][1].as_i64(), Some(64));
    /// assert_eq!(v["b"][1].as_i64(), None);
    /// assert_eq!(v["c"][1].as_i64(), None);
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
    /// # use lexpr::sexp;
    /// #
    /// let v = sexp!(((a 64) (b -64) (c 256.0)));
    ///
    /// assert_eq!(v["a"][1].as_u64(), Some(64));
    /// assert_eq!(v["b"][1].as_u64(), None);
    /// assert_eq!(v["c"][1].as_u64(), None);
    /// ```
    #[inline]
    pub fn as_u64(&self) -> Option<u64> {
        #[cfg(not(feature = "arbitrary_precision"))]
        match self.n {
            N::PosInt(n) => Some(n),
            N::NegInt(_) | N::Float(_) => None,
        }
        #[cfg(feature = "arbitrary_precision")]
        self.n.parse().ok()
    }

    /// Represents the number as f64 if possible. Returns None otherwise.
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
    #[inline]
    pub fn as_f64(&self) -> Option<f64> {
        match self.n {
            N::PosInt(n) => Some(n as f64),
            N::NegInt(n) => Some(n as f64),
            N::Float(n) => Some(n),
        }
    }

    /// Converts a finite `f64` to a `Number`. Infinite or NaN values are not JSON
    /// numbers.
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
        Number { n: N::Float(f64::from(n)) }
    }
}

impl From<f64> for Number {
    #[inline]
    fn from(n: f64) -> Self {
        Number { n: N::Float(n) }
    }
}
