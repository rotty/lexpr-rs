use serde::Serialize;

use lexpr::print;

use crate::error::Result;
use crate::value::to_value;

use std::io;

/// Serialize an instance of type `T` into an S-expression string, using the
/// default printer options.
///
/// ```
/// use serde_lexpr::to_string;
///
/// assert_eq!(to_string(&("foo", 1)).unwrap(), r#"#("foo" 1)"#.to_string())
/// ```
pub fn to_string<T>(value: &T) -> Result<String>
where
    T: Serialize,
{
    Ok(lexpr::to_string(&to_value(value)?)?)
}

/// Serialize an instance of type `T` into an S-expression string.
///
/// ```
/// use serde_lexpr::{print, to_string_custom};
///
/// assert_eq!(to_string_custom(&("foo", 1), print::Options::elisp()).unwrap(), r#"["foo" 1]"#.to_string())
/// ```
pub fn to_string_custom<T>(value: &T, options: print::Options) -> Result<String>
where
    T: Serialize,
{
    Ok(lexpr::to_string_custom(&to_value(value)?, options)?)
}

/// Serialize an instance of type `T` into an S-expression byte vector, using the
/// default printer options.
///
/// ```
/// use serde_lexpr::to_vec;
///
/// let expected: Vec<u8> = r#"#("foo" 1)"#.into();
/// assert_eq!(to_vec(&("foo", 1)).unwrap(), expected);
/// ```
pub fn to_vec<T>(value: &T) -> Result<Vec<u8>>
where
    T: Serialize,
{
    Ok(lexpr::to_vec(&to_value(value)?)?)
}

/// Serialize an instance of type `T` into an S-expression byte vector.
///
/// ```
/// use serde_lexpr::{print, to_vec_custom};
///
/// let expected: Vec<u8> = r#"["foo" 1]"#.into();
/// assert_eq!(to_vec_custom(&("foo", 1), print::Options::elisp()).unwrap(), expected);
/// ```
pub fn to_vec_custom<T>(value: &T, options: print::Options) -> Result<Vec<u8>>
where
    T: Serialize,
{
    Ok(lexpr::to_vec_custom(&to_value(value)?, options)?)
}

/// Serialize an instance of type `T` into an S-expression byte vector, using the
/// default printer options.
///
/// ```
/// use serde_lexpr::to_writer;
///
/// let mut output = Vec::new();
/// to_writer(&mut output, &("foo", 1)).unwrap();
/// assert_eq!(output, r#"#("foo" 1)"#.as_bytes());
/// ```
pub fn to_writer<T, W>(writer: W, value: &T) -> Result<()>
where
    T: Serialize,
    W: io::Write,
{
    Ok(lexpr::to_writer(writer, &to_value(value)?)?)
}

/// Serialize an instance of type `T` into an S-expression byte vector.
///
/// ```
/// use serde_lexpr::{print, to_writer_custom};
///
/// let mut output = Vec::new();
/// to_writer_custom(&mut output, &("foo", 1), print::Options::elisp()).unwrap();
/// assert_eq!(output, r#"["foo" 1]"#.as_bytes());
/// ```
pub fn to_writer_custom<T, W>(writer: W, value: &T, options: print::Options) -> Result<()>
where
    T: Serialize,
    W: io::Write,
{
    Ok(lexpr::to_writer_custom(writer, &to_value(value)?, options)?)
}
