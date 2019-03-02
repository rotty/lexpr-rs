//! When serializing or deserializing S-expressions goes wrong.

use std::fmt::{self, Debug, Display};
use std::{error, result};

use serde::{de, ser};

/// This type represents all possible errors that can occur when
/// serializing or deserializing S-expression data.
pub struct Error(Box<ErrorImpl>);

/// Alias for a `Result` with the error type `serde_lexpr::Error`.
pub type Result<T> = result::Result<T, Error>;

enum ErrorImpl {
    Message(String),
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self.0 {
            ErrorImpl::Message(ref msg) => msg,
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self.0 {
            _ => None,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.0 {
            ErrorImpl::Message(ref msg) => Display::fmt(msg, f),
        }
    }
}

impl Debug for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match *self.0 {
            ErrorImpl::Message(ref msg) => formatter.debug_tuple("Message").field(msg).finish(),
        }
    }
}

impl de::Error for Error {
    fn custom<T: Display>(msg: T) -> Error {
        Error(Box::new(ErrorImpl::Message(msg.to_string())))
    }

    fn invalid_type(unexp: de::Unexpected, exp: &de::Expected) -> Self {
        if let de::Unexpected::Unit = unexp {
            Error::custom(format_args!("invalid type: null, expected {}", exp))
        } else {
            Error::custom(format_args!("invalid type: {}, expected {}", unexp, exp))
        }
    }
}

impl ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Error {
        Error(Box::new(ErrorImpl::Message(msg.to_string())))
    }
}
