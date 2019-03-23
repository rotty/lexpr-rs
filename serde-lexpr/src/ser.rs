use serde::Serialize;

use crate::error::Result;
use crate::value::to_value;

/// Serialize an instance of type `T` into an for S-expression string.
///
/// ```
/// # use serde_lexpr::to_string;
/// assert_eq!(to_string(&("foo", 1)).unwrap(), r#"#("foo" 1)"#.to_string())
/// ```
pub fn to_string<T>(value: &T) -> Result<String>
where
    T: Serialize,
{
    Ok(lexpr::to_string(&to_value(value)?)?)
}
