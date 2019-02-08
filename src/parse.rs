use std::io;

use crate::{Value, Result};

#[doc(hidden)]
pub fn from_reader<R>(_rdr: impl io::Read) -> Result<Value>
{
    unimplemented!()
}

#[doc(hidden)]
pub fn from_slice(_v: &[u8]) -> Result<Value>
{
    unimplemented!()
}

#[doc(hidden)]
pub fn from_str(_s: &str) -> Result<Value>
{
    unimplemented!()
}
