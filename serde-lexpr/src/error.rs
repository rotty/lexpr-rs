//! When serializing or deserializing S-expressions goes wrong.

use std::fmt::{self, Debug, Display};
use std::{error, io, result};

use serde::{de, ser};

use lexpr::parse;

/// This type represents all possible errors that can occur when
/// serializing or deserializing S-expression data.
pub struct Error(Box<ErrorImpl>);

/// Alias for a `Result` with the error type `serde_lexpr::Error`.
pub type Result<T> = result::Result<T, Error>;

enum ErrorImpl {
    Message(String),
    Io(io::Error),
    Parse(parse::Error),
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match &*self.0 {
            ErrorImpl::Message(msg) => msg,
            ErrorImpl::Io(e) => e.description(),
            ErrorImpl::Parse(e) => e.description(),
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match &*self.0 {
            ErrorImpl::Io(e) => Some(e),
            ErrorImpl::Parse(e) => Some(e),
            _ => None,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &*self.0 {
            ErrorImpl::Message(ref msg) => Display::fmt(msg, f),
            ErrorImpl::Io(e) => Display::fmt(e, f),
            ErrorImpl::Parse(e) => Display::fmt(e, f),
        }
    }
}

impl Debug for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match &*self.0 {
            ErrorImpl::Message(msg) => formatter.debug_tuple("Message").field(msg).finish(),
            ErrorImpl::Io(e) => formatter.debug_tuple("Io").field(e).finish(),
            ErrorImpl::Parse(e) => formatter.debug_tuple("Parse").field(e).finish(),
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

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error(Box::new(ErrorImpl::Io(e)))
    }
}

impl From<parse::Error> for Error {
    fn from(e: parse::Error) -> Self {
        Error(Box::new(ErrorImpl::Parse(e)))
    }
}
