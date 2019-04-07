use std::marker::PhantomData;

use serde::de::{self, Error as SerdeError, Visitor};
use serde::Deserialize;

use lexpr::{number, Cons, Number};

use crate::error::{Error, Result};
use crate::Value;

pub struct Deserializer<'de> {
    input: &'de Value,
}

impl<'de> Deserializer<'de> {
    pub fn from_value(input: &'de Value) -> Self {
        Deserializer { input }
    }
}

/// Interpret a `lexpr::Value` as an instance of type `T`.
///
/// This conversion can fail if the structure of the `Value` does not match the
/// structure expected by `T`, for example if `T` is a struct type but the
/// `Value` contains something other than a S-expression list. It can also fail
/// if the structure is correct but `T`'s implementation of `Deserialize`
/// decides that something is wrong with the data, for example required struct
/// fields are missing from the S-expression map or some number is too big to
/// fit in the expected primitive type.
///
/// ```
/// # use serde_lexpr::Value;
/// let val = Value::string("foo");
/// let s: String = serde_lexpr::from_value(&val).unwrap();
/// assert_eq!("foo", s);
/// ```
pub fn from_value<'a, T>(value: &'a Value) -> Result<T>
where
    T: Deserialize<'a>,
{
    T::deserialize(&mut Deserializer::from_value(value))
}

macro_rules! deserialize_prim_number {
    ($method:ident) => {
        fn $method<V>(self, visitor: V) -> Result<V::Value>
        where
            V: de::Visitor<'de>,
        {
            self.input.as_number().ok_or_else(|| invalid_value(self.input, "a number")).and_then(|n| visit_number(n, visitor))
        }
    }
}

impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match self.input {
            Value::Nil => visitor.visit_unit(),
            Value::Null => visitor.visit_seq(ListAccess::empty()),
            Value::Bool(b) => visitor.visit_bool(*b),
            Value::Cons(cell) => visitor.visit_seq(ConsAccess::new(cell)),
            Value::Keyword(_) => Err(invalid_value(self.input, "rust-compatible value")),
            Value::Symbol(_) => Err(invalid_value(self.input, "rust-compatible value")),
            Value::Number(n) => visit_number(n, visitor),
            Value::Char(c) => visitor.visit_char(*c),
            Value::String(s) => visitor.visit_borrowed_str(s),
            Value::Vector(elts) => visitor.visit_seq(VecAccess::new(elts)),
            Value::Bytes(bytes) => visitor.visit_borrowed_bytes(bytes),
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match self.input {
            Value::Bool(b) => visitor.visit_bool(*b),
            _ => Err(invalid_value(self.input, "boolean")),
        }
    }

    deserialize_prim_number!(deserialize_i8);
    deserialize_prim_number!(deserialize_i16);
    deserialize_prim_number!(deserialize_i32);
    deserialize_prim_number!(deserialize_i64);
    deserialize_prim_number!(deserialize_u8);
    deserialize_prim_number!(deserialize_u16);
    deserialize_prim_number!(deserialize_u32);
    deserialize_prim_number!(deserialize_u64);
    deserialize_prim_number!(deserialize_f32);
    deserialize_prim_number!(deserialize_f64);

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match self.input {
            Value::Char(c) => visitor.visit_char(*c),
            _ => Err(invalid_value(self.input, "char")),
        }
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.input
            .as_str()
            .ok_or_else(|| invalid_value(self.input, "a string"))
            .and_then(|s| visitor.visit_borrowed_str(s))
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match self.input {
            Value::Bytes(bytes) => visitor.visit_borrowed_bytes(bytes),
            _ => Err(invalid_value(self.input, "byte vector")),
        }
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_bytes(visitor)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match self.input {
            Value::Null => visitor.visit_none(),
            Value::Cons(cons) if cons.cdr().is_null() => {
                visitor.visit_some(&mut Deserializer::from_value(cons.car()))
            }
            _ => Err(invalid_value(self.input, "empty or one-element list")),
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.input
            .as_nil()
            .ok_or_else(|| invalid_value(self.input, "nil"))
            .and_then(|_| visitor.visit_unit())
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    fn deserialize_newtype_struct<V>(self, _name: &str, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match self.input {
            Value::Null => visitor.visit_seq(ListAccess::empty()),
            Value::Vector(elements) => visitor.visit_seq(VecAccess::new(elements)),
            Value::Cons(cell) => visitor.visit_seq(ListAccess::new(cell)),
            _ => Err(invalid_value(self.input, "list")),
        }
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match self.input {
            Value::Vector(elements) => visitor.visit_seq(VecAccess::new(elements)),
            Value::Cons(cell) => visitor.visit_seq(ListAccess::new(cell)),
            _ => Err(invalid_value(self.input, "list")),
        }
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_tuple(len, visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.input
            .as_cons()
            .ok_or_else(|| invalid_value(self.input, "a list"))
            .and_then(|cell| visitor.visit_map(MapAccess::new(cell)))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        match self.input {
            Value::Symbol(_) => visitor.visit_enum(UnitVariantAccess::new(self)),
            Value::Cons(cell) => visitor.visit_enum(VariantAccess::new(cell)),
            _ => Err(invalid_value(self.input, "symbol or cons cell")),
        }
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.input
            .as_symbol()
            .ok_or_else(|| invalid_value(self.input, "symbol"))
            .and_then(|s| visitor.visit_borrowed_str(s))
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_unit()
    }
}

fn visit_number<'de, V>(n: &'de Number, visitor: V) -> Result<V::Value>
where
    V: de::Visitor<'de>,
{
    struct Proxy<'de, V> {
        visitor: V,
        _lifetime: &'de PhantomData<()>,
    };
    impl<'de, V> number::Visitor for Proxy<'de, V>
    where
        V: de::Visitor<'de>,
    {
        type Value = V::Value;
        type Error = Error;

        fn error<T: Into<String>>(msg: T) -> Error {
            Error::invalid_value(de::Unexpected::Other(&msg.into()), &"i64, u64 or f64")
        }
        fn visit_i64(self, n: i64) -> Result<V::Value> {
            self.visitor.visit_i64(n)
        }
        fn visit_u64(self, n: u64) -> Result<V::Value> {
            self.visitor.visit_u64(n)
        }
        fn visit_f64(self, n: f64) -> Result<V::Value> {
            self.visitor.visit_f64(n)
        }
    }
    n.visit(Proxy {
        visitor,
        _lifetime: &PhantomData,
    })
}

fn invalid_value(value: &Value, expected: &'static str) -> Error {
    let unexpected = match value {
        Value::Null => de::Unexpected::Other("null value"),
        Value::Nil => de::Unexpected::Other("nil value"),
        Value::String(s) => de::Unexpected::Str(s),
        Value::Symbol(_) => de::Unexpected::Other("symbol"),
        Value::Keyword(_) => de::Unexpected::Other("keyword"),
        Value::Bool(b) => de::Unexpected::Bool(*b),
        Value::Char(c) => de::Unexpected::Char(*c),
        Value::Bytes(_) => de::Unexpected::Other("byte string"),
        Value::Cons(_) => de::Unexpected::Other("cons cell"),
        Value::Number(_) => de::Unexpected::Other("number"), // FIXME: implement properly
        Value::Vector(_) => de::Unexpected::Other("vector"),
    };
    Error::invalid_type(unexpected, &expected)
}

struct ConsAccess<'a> {
    idx: u8,
    cell: &'a Cons,
}

impl<'a> ConsAccess<'a> {
    fn new(cell: &'a Cons) -> Self {
        ConsAccess { idx: 0, cell }
    }
}

impl<'de> de::SeqAccess<'de> for ConsAccess<'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: de::DeserializeSeed<'de>,
    {
        let value = match self.idx {
            0 => self.cell.car(),
            1 => self.cell.cdr(),
            _ => return Ok(None),
        };
        Ok(Some(
            seed.deserialize(&mut Deserializer::from_value(value))?,
        ))
    }
}

struct ListAccess<'a> {
    cursor: Option<&'a Cons>,
}

impl<'a> ListAccess<'a> {
    fn new(cell: &'a Cons) -> Self {
        ListAccess { cursor: Some(cell) }
    }
    fn empty() -> Self {
        ListAccess { cursor: None }
    }
}

impl<'de> de::SeqAccess<'de> for ListAccess<'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: de::DeserializeSeed<'de>,
    {
        match self.cursor {
            Some(cell) => {
                let element = seed.deserialize(&mut Deserializer::from_value(cell.car()))?;
                match cell.cdr() {
                    Value::Cons(next) => self.cursor = Some(next),
                    Value::Null => self.cursor = None,
                    _ => return Err(invalid_value(cell.cdr(), "cons cell or end of list")),
                }
                Ok(Some(element))
            }
            None => Ok(None),
        }
    }
}

struct VecAccess<'a> {
    vec: &'a [Value],
    index: usize,
}

impl<'a> VecAccess<'a> {
    fn new(vec: &'a [Value]) -> Self {
        VecAccess { vec, index: 0 }
    }
}

impl<'de> de::SeqAccess<'de> for VecAccess<'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: de::DeserializeSeed<'de>,
    {
        if self.index >= self.vec.len() {
            Ok(None)
        } else {
            let deserialized =
                seed.deserialize(&mut Deserializer::from_value(&self.vec[self.index]))?;
            self.index += 1;
            Ok(Some(deserialized))
        }
    }
}

struct MapAccess<'a> {
    cursor: Option<&'a Cons>,
}

impl<'a> MapAccess<'a> {
    fn new(cell: &'a Cons) -> Self {
        MapAccess { cursor: Some(cell) }
    }
}

impl<'de> de::MapAccess<'de> for MapAccess<'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: de::DeserializeSeed<'de>,
    {
        match self.cursor {
            None => Ok(None),
            Some(cell) => cell
                .car()
                .as_cons()
                .ok_or_else(|| invalid_value(cell.car(), "cons cell"))
                .and_then(|cell| {
                    Ok(Some(
                        seed.deserialize(&mut Deserializer::from_value(cell.car()))?,
                    ))
                }),
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: de::DeserializeSeed<'de>,
    {
        let cell = self
            .cursor
            .expect("next value requested after end of sequence");
        let value = cell
            .car()
            .as_cons()
            .ok_or_else(|| invalid_value(cell.car(), "cons cell"))
            .and_then(|cell| seed.deserialize(&mut Deserializer::from_value(cell.cdr())))?;
        self.cursor = match cell.cdr() {
            Value::Cons(cell) => Some(cell),
            Value::Null => None,
            _ => return Err(invalid_value(cell.cdr(), "end of list or cons cell")),
        };
        Ok(value)
    }
}

struct VariantAccess<'a> {
    cell: &'a Cons,
}

impl<'a> VariantAccess<'a> {
    fn new(cell: &'a Cons) -> Self {
        VariantAccess { cell }
    }
}

impl<'de> de::EnumAccess<'de> for VariantAccess<'de> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self)>
    where
        V: de::DeserializeSeed<'de>,
    {
        let val = seed.deserialize(&mut Deserializer::from_value(self.cell.car()))?;
        Ok((val, self))
    }
}

impl<'de> de::VariantAccess<'de> for VariantAccess<'de> {
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
    where
        T: de::DeserializeSeed<'de>,
    {
        seed.deserialize(&mut Deserializer::from_value(self.cell.cdr()))
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_seq(&mut Deserializer::from_value(self.cell.cdr()), visitor)
    }

    fn struct_variant<V>(self, fields: &'static [&'static str], visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_struct(
            &mut Deserializer::from_value(self.cell.cdr()),
            "",
            fields,
            visitor,
        )
    }
}

struct UnitVariantAccess<'a, 'de> {
    de: &'a mut Deserializer<'de>,
}

impl<'a, 'de> UnitVariantAccess<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        UnitVariantAccess { de }
    }
}

impl<'a, 'de> de::EnumAccess<'de> for UnitVariantAccess<'a, 'de> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self)>
    where
        V: de::DeserializeSeed<'de>,
    {
        let variant = seed.deserialize(&mut *self.de)?;
        Ok((variant, self))
    }
}

impl<'a, 'de> de::VariantAccess<'de> for UnitVariantAccess<'a, 'de> {
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, _seed: T) -> Result<T::Value>
    where
        T: de::DeserializeSeed<'de>,
    {
        Err(de::Error::invalid_type(
            de::Unexpected::UnitVariant,
            &"newtype variant",
        ))
    }

    fn tuple_variant<V>(self, _len: usize, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        Err(de::Error::invalid_type(
            de::Unexpected::UnitVariant,
            &"tuple variant",
        ))
    }

    fn struct_variant<V>(self, _fields: &'static [&'static str], _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        Err(de::Error::invalid_type(
            de::Unexpected::UnitVariant,
            &"struct variant",
        ))
    }
}
