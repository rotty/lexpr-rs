use serde::de::DeserializeOwned;

use crate::error::Result;
use crate::value::from_value;

/// Deserialize an instance of type `T` from an S-expression string.
///
/// ```
/// # use serde_lexpr::from_str;
/// let v: Vec<u32> = from_str("(1 2 3)").unwrap();
/// assert_eq!(v, vec![1, 2, 3]);
/// ```
pub fn from_str<T>(s: &str) -> Result<T>
where
    T: DeserializeOwned,
{
    Ok(from_value(&lexpr::from_str(s)?)?)
}
