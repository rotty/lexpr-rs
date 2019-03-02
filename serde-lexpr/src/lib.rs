#![deny(missing_docs)]

//! This crate provides [Serde]-based serialization and
//! deserialization from statically-typed Rust data structures to the
//! dynamically typed S-expression values, using the [`lexpr::Value`]
//! type and to their text representation.
//!
//! [Serde]: https://crates.io/crates/serde
//! [`lexpr::Value`]: https://docs.rs/lexpr/*/lexpr/enum.Value.html

pub use error::{Error, Result};
pub use value::{from_value, to_value, Value};

pub mod error;
pub mod value;
