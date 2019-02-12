//! Non-compound S-expression value type.

use std::borrow::Cow;

pub use crate::number::Number;

/// A Lisp atom.
///
/// Atoms represent non-compound data types.
#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    /// The special "nil" value.
    ///
    /// This value is used in many Lisp dialects to represent the
    /// empty list. It is different from the empty list in `lexpr`;
    /// the latter is represented as a [`Value::List`] with zero
    /// elements.
    ///
    /// [`Value::List`]: enum.Value.html#variant.List
    Nil,

    /// A boolean value.
    Bool(bool),

    /// A number.
    Number(Number),

    /// A string.
    String(String),

    /// A symbol.
    Symbol(String),

    /// A keyword.
    Keyword(String),
}

impl Atom {
    /// Create an atom representing a symbol.
    pub fn symbol(name: impl Into<String>) -> Self {
        Atom::Symbol(name.into())
    }

    /// Create an atom representing a keyword.
    pub fn keyword(name: impl Into<String>) -> Self {
        Atom::Keyword(name.into())
    }

    /// Check whether the atom is the special nil value.
    #[inline]
    pub fn is_nil(&self) -> bool {
        match self {
            Atom::Nil => true,
            _ => false,
        }
    }

    /// If the atom is nil, returns `()`. Returns `None` otherwise.
    pub fn as_nil(&self) -> Option<()> {
        match self {
            Atom::Nil => Some(()),
            _ => None,
        }
    }

    /// Check wether the atom represents a string.
    #[inline]
    pub fn is_string(&self) -> bool {
        match self {
            Atom::String(_) => true,
            _ => false,
        }
    }

    /// If the atom represents a string, return it as `str`
    /// reference. Returns `None` otherwise.
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Atom::String(s) => Some(s),
            _ => None,
        }
    }

    /// Check wether the atom represents a symbol.
    #[inline]
    pub fn is_symbol(&self) -> bool {
        match self {
            Atom::Symbol(_) => true,
            _ => false,
        }
    }

    /// If the atom represents a symbol, return it as `str`
    /// reference. Returns `None` otherwise.
    pub fn as_symbol(&self) -> Option<&str> {
        match self {
            Atom::Symbol(s) => Some(s),
            _ => None,
        }
    }

    /// Check wether the atom represents a keyword.
    #[inline]
    pub fn is_keyword(&self) -> bool {
        match self {
            Atom::Keyword(_) => true,
            _ => false,
        }
    }

    /// If the atom represents a keyword, return it as `str`
    /// reference. Returns `None` otherwise.
    pub fn as_keyword(&self) -> Option<&str> {
        match self {
            Atom::Keyword(s) => Some(s),
            _ => None,
        }
    }

    /// Check wether the atom represents a boolean value.
    pub fn is_boolean(&self) -> bool {
        match self {
            Atom::Bool(_) => true,
            _ => false,
        }
    }

    /// If the atom is a boolean, return its value.
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Atom::Bool(b) => Some(*b),
            _ => None,
        }
    }

    /// Get the name of a symbol or keyword, or the value of a string.
    pub fn as_name(&self) -> Option<&str> {
        match self {
            Atom::Symbol(s) => Some(s),
            Atom::Keyword(s) => Some(s),
            Atom::String(s) => Some(s),
            _ => None,
        }
    }

    /// Check whether the atom is an integer and can be represented as an `i64`.
    #[inline]
    pub fn is_i64(&self) -> bool {
        match self {
            Atom::Number(number) => number.is_i64(),
            _ => false,
        }
    }

    /// If the atom is an integer, represent it as an `i64`, if possible.
    pub fn as_i64(&self) -> Option<i64> {
        self.as_number().and_then(Number::as_i64)
    }

    /// Check whether the atom is an integer and can be represented as an `u64`.
    #[inline]
    pub fn is_u64(&self) -> bool {
        match self {
            Atom::Number(number) => number.is_u64(),
            _ => false,
        }
    }

    /// If the atom is an integer, represent it as an `u64`, if possible.
    pub fn as_u64(&self) -> Option<u64> {
        self.as_number().and_then(Number::as_u64)
    }

    /// Check whether the atom is a floating point number and can be
    /// represented as an `f64`.
    ///
    /// Currently this function returns true if and only if both
    /// `is_i64` and `is_u64` return false but this is not a guarantee
    /// in the future.
    #[inline]
    pub fn is_f64(&self) -> bool {
        match self {
            Atom::Number(number) => number.is_f64(),
            _ => false,
        }
    }

    /// If the atom is a number, represent it as an `f64`, if possible.
    pub fn as_f64(&self) -> Option<f64> {
        self.as_number().and_then(Number::as_f64)
    }

    /// If the atom is a number, return a reference to the underlying
    /// `Number` instance.
    pub fn as_number(&self) -> Option<&Number> {
        match self {
            Atom::Number(n) => Some(n),
            _ => None,
        }
    }
}

macro_rules! impl_from_number {
    (
        $($ty:ty),*
    ) => {
        $(
            impl From<$ty> for Atom {
                #[inline]
                fn from(n: $ty) -> Self {
                    Atom::Number(Number::from(n))
                }
            }
        )*
    };
}

impl_from_number!(u8, u16, u32, u64, i8, i16, i32, i64, f32, f64);

impl From<&str> for Atom {
    #[inline]
    fn from(s: &str) -> Self {
        Atom::String(s.into())
    }
}

impl<'a> From<Cow<'a, str>> for Atom {
    #[inline]
    fn from(s: Cow<'a, str>) -> Self {
        Atom::from(s.to_string())
    }
}

impl From<String> for Atom {
    #[inline]
    fn from(s: String) -> Self {
        Atom::String(s)
    }
}

impl From<bool> for Atom {
    #[inline]
    fn from(v: bool) -> Self {
        Atom::Bool(v)
    }
}

impl From<Number> for Atom {
    fn from(n: Number) -> Self {
        Atom::Number(n)
    }
}
