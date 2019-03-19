#![deny(missing_docs)]

//! This crate provides [Serde]-based serialization and
//! deserialization from statically-typed Rust data structures to the
//! dynamically typed S-expression values, using the [`lexpr::Value`]
//! type and to their text representation.
//!
//! [Serde]: https://crates.io/crates/serde
//! [`lexpr::Value`]: https://docs.rs/lexpr/*/lexpr/enum.Value.html

mod de;
mod error;
mod ser;
mod value;

pub use de::from_str;
pub use error::{Error, Result};
pub use ser::to_string;
pub use value::{from_value, to_value, Cons, Value};

// This is exposed for convenience (allowing importing via `serde_lexpr`) and so
// that links from the `Value` documentation work.
pub use lexpr::parse;
