use std::borrow::Cow;

use crate::{Cons, Number, Value};

macro_rules! impl_from_number {
    (
        $($ty:ty),*
    ) => {
        $(
            impl From<$ty> for Value {
                #[inline]
                fn from(n: $ty) -> Self {
                    Value::Number(Number::from(n))
                }
            }
        )*
    };
}

impl_from_number!(u8, u16, u32, u64, i8, i16, i32, i64, f32, f64);

impl From<char> for Value {
    #[inline]
    fn from(c: char) -> Self {
        Value::Char(c)
    }
}

impl From<&str> for Value {
    #[inline]
    fn from(s: &str) -> Self {
        Value::String(s.into())
    }
}

impl<'a> From<Cow<'a, str>> for Value {
    #[inline]
    fn from(s: Cow<'a, str>) -> Self {
        Value::from(s.as_ref())
    }
}

impl From<Box<str>> for Value {
    #[inline]
    fn from(s: Box<str>) -> Self {
        Value::String(s)
    }
}

impl From<String> for Value {
    #[inline]
    fn from(s: String) -> Self {
        Value::String(s.into_boxed_str())
    }
}

impl From<bool> for Value {
    #[inline]
    fn from(v: bool) -> Self {
        Value::Bool(v)
    }
}

impl From<&[u8]> for Value {
    #[inline]
    fn from(bytes: &[u8]) -> Self {
        Value::Bytes(bytes.into())
    }
}

impl From<Box<[u8]>> for Value {
    #[inline]
    fn from(bytes: Box<[u8]>) -> Self {
        Value::Bytes(bytes)
    }
}

impl From<Vec<u8>> for Value {
    #[inline]
    fn from(bytes: Vec<u8>) -> Self {
        Value::Bytes(bytes.into_boxed_slice())
    }
}

impl From<Number> for Value {
    fn from(n: Number) -> Self {
        Value::Number(n)
    }
}

impl<T, U> From<(T, U)> for Value
where
    T: Into<Value>,
    U: Into<Value>,
{
    fn from((car, cdr): (T, U)) -> Self {
        Value::Cons(Cons::new(car, cdr))
    }
}

impl From<Cons> for Value {
    fn from(pair: Cons) -> Self {
        Value::Cons(pair)
    }
}

impl From<Vec<Value>> for Value {
    fn from(elts: Vec<Value>) -> Self {
        Value::Vector(elts)
    }
}

impl From<Box<[Value]>> for Value {
    fn from(elts: Box<[Value]>) -> Self {
        Value::Vector(elts.into_vec())
    }
}
