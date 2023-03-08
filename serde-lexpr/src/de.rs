use std::io;

use serde::de::DeserializeOwned;

use lexpr::parse;

use crate::error::Result;
use crate::value::from_value;

/// Deserialize an instance of type `T` from an S-expression string, using the
/// default parser options.
///
/// ```
/// use serde_lexpr::from_str;
///
/// let v: Vec<u32> = from_str("(1 2 3)").unwrap();
/// assert_eq!(v, vec![1, 2, 3]);
/// ```
pub fn from_str<T>(s: &str) -> Result<T>
where
    T: DeserializeOwned,
{
    from_value(&lexpr::from_str(s)?)
}

/// Deserialize an instance of type `T` from an S-expression string.
///
/// ```
/// use serde_lexpr::{from_str_custom, parse};
///
/// let v: Vec<u32> = from_str_custom("[1 2 3]", parse::Options::elisp()).unwrap();
/// assert_eq!(v, vec![1, 2, 3]);
/// ```
pub fn from_str_custom<T>(s: &str, options: parse::Options) -> Result<T>
where
    T: DeserializeOwned,
{
    from_value(&lexpr::from_str_custom(s, options)?)
}

/// Deserialize an instance of type `T` from an S-expression byte slice, using the
/// default parser options.
///
/// ```
/// use serde_lexpr::from_slice;
///
/// let v: Vec<u32> = from_slice(b"(1 2 3)").unwrap();
/// assert_eq!(v, vec![1, 2, 3]);
/// ```
pub fn from_slice<T>(s: &[u8]) -> Result<T>
where
    T: DeserializeOwned,
{
    from_value(&lexpr::from_slice(s)?)
}

/// Deserialize an instance of type `T` from an S-expression byte slice, using the
/// default parser options.
///
/// ```
/// use serde_lexpr::{from_slice_custom, parse};
///
/// let v: Vec<u32> = from_slice_custom(b"[1 2 3]", parse::Options::elisp()).unwrap();
/// assert_eq!(v, vec![1, 2, 3]);
/// ```
pub fn from_slice_custom<T>(s: &[u8], options: parse::Options) -> Result<T>
where
    T: DeserializeOwned,
{
    from_value(&lexpr::from_slice_custom(s, options)?)
}

/// Parse a value from an input stream of S-expressions, using the
/// default parser options.
///
/// ```
/// use serde_lexpr::from_reader;
///
/// let cursor = std::io::Cursor::new(b"(1 2 3)");
/// let v: Vec<u32> = from_reader(cursor).unwrap();
/// assert_eq!(v, vec![1, 2, 3]);
/// ```
pub fn from_reader<T>(rdr: impl io::Read) -> Result<T>
where
    T: DeserializeOwned,
{
    from_value(&lexpr::from_reader(rdr)?)
}

/// Parse a value from an input stream of S-expressions, using the
/// default parser options.
///
/// ```
/// use serde_lexpr::{from_reader_custom, parse};
///
/// let cursor = std::io::Cursor::new(b"(1 2 3)");
/// let v: Vec<u32> = from_reader_custom(cursor, parse::Options::elisp()).unwrap();
/// assert_eq!(v, vec![1, 2, 3]);
/// ```
pub fn from_reader_custom<T>(rdr: impl io::Read, options: parse::Options) -> Result<T>
where
    T: DeserializeOwned,
{
    from_value(&lexpr::from_reader_custom(rdr, options)?)
}
