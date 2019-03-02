//! Serialization to and from `lexpr::Value`.

pub use lexpr::Value;

pub use de::from_value;
pub use ser::to_value;

mod de;
mod ser;
