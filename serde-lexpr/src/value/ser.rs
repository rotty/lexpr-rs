//! Serializer implementation constructing `lexpr::Value`.

use serde::ser;

use crate::error::{Error, Result};
use crate::Value;

pub struct Serializer;

impl ser::Serializer for Serializer {
    type Ok = Value;
    type Error = Error;

    type SerializeSeq = SerializeList;
    type SerializeTuple = SerializeVector;
    type SerializeTupleStruct = SerializeVector;
    type SerializeTupleVariant = SerializeTupleVariant;
    type SerializeMap = SerializeMap;
    type SerializeStruct = SerializeStruct;
    type SerializeStructVariant = SerializeStructVariant;

    fn serialize_bool(self, v: bool) -> Result<Value> {
        Ok(Value::Bool(v))
    }

    fn serialize_i8(self, v: i8) -> Result<Value> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_i16(self, v: i16) -> Result<Value> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_i32(self, v: i32) -> Result<Value> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_i64(self, v: i64) -> Result<Value> {
        Ok(Value::from(v))
    }

    fn serialize_u8(self, v: u8) -> Result<Value> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_u16(self, v: u16) -> Result<Value> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_u32(self, v: u32) -> Result<Value> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_u64(self, v: u64) -> Result<Value> {
        Ok(Value::from(v))
    }

    fn serialize_f32(self, v: f32) -> Result<Value> {
        self.serialize_f64(f64::from(v))
    }

    fn serialize_f64(self, v: f64) -> Result<Value> {
        Ok(Value::from(v))
    }

    fn serialize_char(self, value: char) -> Result<Value> {
        Ok(Value::Char(value))
    }

    fn serialize_str(self, value: &str) -> Result<Value> {
        Ok(Value::String(value.into()))
    }

    fn serialize_bytes(self, value: &[u8]) -> Result<Value> {
        Ok(Value::Bytes(value.into()))
    }

    fn serialize_unit(self) -> Result<Value> {
        Ok(Value::Null)
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Value> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        self,
        _name: &str,
        _variant_index: u32,
        variant: &str,
    ) -> Result<Value> {
        Ok(Value::symbol(variant))
    }

    fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T) -> Result<Value>
    where
        T: ser::Serialize + ?Sized,
    {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &str,
        _variant_index: u32,
        variant: &str,
        value: &T,
    ) -> Result<Value>
    where
        T: ser::Serialize + ?Sized,
    {
        Ok(Value::cons(Value::symbol(variant), to_value(value)?))
    }

    /// Serializes `None` as the empty list.
    fn serialize_none(self) -> Result<Value> {
        Ok(Value::Null)
    }

    /// Serializes `Some` as a one-element list.
    fn serialize_some<V>(self, value: &V) -> Result<Value>
    where
        V: ser::Serialize + ?Sized,
    {
        Ok(Value::cons(value.serialize(self)?, Value::Null))
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<SerializeList> {
        Ok(SerializeList {
            items: len.map_or_else(Vec::new, Vec::with_capacity),
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<SerializeVector> {
        Ok(SerializeVector {
            items: Vec::with_capacity(len),
        })
    }

    fn serialize_tuple_struct(self, _name: &'static str, len: usize) -> Result<SerializeVector> {
        self.serialize_tuple(len)
    }

    fn serialize_tuple_variant(
        self,
        _enum: &'static str,
        _idx: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<SerializeTupleVariant> {
        Ok(SerializeTupleVariant {
            name: variant,
            items: Vec::with_capacity(len),
        })
    }

    fn serialize_map(self, len: Option<usize>) -> Result<SerializeMap> {
        Ok(SerializeMap {
            entries: len.map_or_else(Vec::new, Vec::with_capacity),
            next_key: None,
        })
    }

    fn serialize_struct(self, _name: &'static str, len: usize) -> Result<SerializeStruct> {
        Ok(SerializeStruct {
            fields: Vec::with_capacity(len),
        })
    }

    fn serialize_struct_variant(
        self,
        _enum: &'static str,
        _idx: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<SerializeStructVariant> {
        Ok(SerializeStructVariant {
            name: variant,
            fields: Vec::with_capacity(len),
        })
    }
}

#[doc(hidden)]
pub struct SerializeList {
    items: Vec<Value>,
}

#[doc(hidden)]
pub struct SerializeVector {
    items: Vec<Value>,
}

#[doc(hidden)]
pub struct SerializeTupleVariant {
    name: &'static str,
    items: Vec<Value>,
}

#[doc(hidden)]
pub struct SerializeMap {
    entries: Vec<Value>,
    next_key: Option<Value>,
}

#[doc(hidden)]
pub struct SerializeStruct {
    fields: Vec<Value>,
}

#[doc(hidden)]
pub struct SerializeStructVariant {
    name: &'static str,
    fields: Vec<Value>,
}

impl ser::SerializeSeq for SerializeList {
    type Ok = Value;
    type Error = Error;

    fn serialize_element<T>(&mut self, elem: &T) -> Result<()>
    where
        T: ser::Serialize + ?Sized,
    {
        self.items.push(to_value(elem)?);
        Ok(())
    }

    fn end(self) -> Result<Value> {
        Ok(Value::list(self.items))
    }
}

impl ser::SerializeTuple for SerializeVector {
    type Ok = Value;
    type Error = Error;

    fn serialize_element<V>(&mut self, value: &V) -> Result<()>
    where
        V: ser::Serialize + ?Sized,
    {
        self.items.push(to_value(value)?);
        Ok(())
    }

    fn end(self) -> Result<Value> {
        Ok(Value::Vector(self.items))
    }
}

impl ser::SerializeTupleStruct for SerializeVector {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<V>(&mut self, value: &V) -> Result<()>
    where
        V: ser::Serialize + ?Sized,
    {
        ser::SerializeTuple::serialize_element(self, value)
    }

    fn end(self) -> Result<Value> {
        ser::SerializeTuple::end(self)
    }
}

impl ser::SerializeTupleVariant for SerializeTupleVariant {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<V>(&mut self, v: &V) -> Result<()>
    where
        V: ser::Serialize + ?Sized,
    {
        self.items.push(to_value(v)?);
        Ok(())
    }

    fn end(self) -> Result<Value> {
        Ok(Value::cons(
            Value::symbol(self.name),
            Value::list(self.items),
        ))
    }
}

impl ser::SerializeMap for SerializeMap {
    type Ok = Value;
    type Error = Error;

    fn serialize_key<T>(&mut self, key: &T) -> Result<()>
    where
        T: ser::Serialize + ?Sized,
    {
        self.next_key = Some(to_value(key)?);
        Ok(())
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<()>
    where
        T: ser::Serialize + ?Sized,
    {
        match self.next_key.take() {
            Some(key) => self.entries.push(Value::cons(key, to_value(value)?)),
            None => panic!("serialize_value called before serialize_key"),
        }
        Ok(())
    }

    fn serialize_entry<K, V>(&mut self, key: &K, value: &V) -> Result<()>
    where
        K: ser::Serialize + ?Sized,
        V: ser::Serialize + ?Sized,
    {
        self.entries
            .push(Value::cons(to_value(key)?, to_value(value)?));
        Ok(())
    }

    fn end(self) -> Result<Value> {
        Ok(Value::list(self.entries))
    }
}

impl ser::SerializeStruct for SerializeStruct {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<V>(&mut self, field: &'static str, value: &V) -> Result<()>
    where
        V: ser::Serialize + ?Sized,
    {
        self.fields
            .push(Value::cons(Value::symbol(field), to_value(value)?));
        Ok(())
    }

    fn end(self) -> Result<Value> {
        Ok(Value::list(self.fields))
    }
}

impl ser::SerializeStructVariant for SerializeStructVariant {
    type Ok = Value;
    type Error = Error;

    fn serialize_field<V>(&mut self, field: &'static str, v: &V) -> Result<()>
    where
        V: ser::Serialize + ?Sized,
    {
        self.fields
            .push(Value::cons(Value::symbol(field), to_value(v)?));
        Ok(())
    }

    fn end(self) -> Result<Value> {
        Ok(Value::cons(
            Value::symbol(self.name),
            Value::list(self.fields),
        ))
    }
}

/// Convert a `T` into `lexpr::Value` which is an enum that can represent
/// any valid S-expression data.
///
/// This conversion can fail if `T`'s implementation of `Serialize` decides to
/// return an error.
///
/// ```rust
/// # use lexpr::Value;
/// let val = serde_lexpr::to_value("s").unwrap();
/// assert_eq!(val, Value::string("s"));
/// ```
// Taking by value is more friendly to iterator adapters, option and result
// consumers, etc. See <https://github.com/serde-rs/json/pull/149>.
pub fn to_value<T>(value: T) -> Result<Value>
where
    T: ser::Serialize,
{
    value.serialize(Serializer)
}
