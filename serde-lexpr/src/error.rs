//! When serializing or deserializing S-expressions goes wrong.

use std::fmt::{self, Debug, Display};
use std::{error, io, result};

use serde::{de, ser};

use lexpr::parse;
pub use lexpr::parse::error::Location;

/// This type represents all possible errors that can occur when
/// serializing or deserializing S-expression data.
pub struct Error(Box<ErrorImpl>);

/// Alias for a `Result` with the error type `serde_lexpr::Error`.
pub type Result<T> = result::Result<T, Error>;

enum ErrorImpl {
    Message(String, Option<Location>),
    Io(io::Error),
    Parse(parse::Error),
}

impl Error {
    /// Location of the error in the input stream.
    pub fn location(&self) -> Option<Location> {
        match &*self.0 {
            ErrorImpl::Message(_, loc) => *loc,
            ErrorImpl::Parse(e) => e.location(),
            ErrorImpl::Io(_) => None,
        }
    }
    /// Categorizes the cause of this error.
    ///
    /// - `Category::Io` - failure to read or write bytes on an IO stream
    /// - `Category::Syntax` - input that is not a syntactically valid S-expression
    /// - `Category::Data` - input data that is semantically incorrect
    /// - `Category::Eof` - unexpected end of the input data
    pub fn classify(&self) -> Category {
        match &*self.0 {
            ErrorImpl::Message(_, _) => Category::Data,
            ErrorImpl::Io(_) => Category::Io,
            ErrorImpl::Parse(e) => match e.classify() {
                parse::error::Category::Syntax => Category::Syntax,
                parse::error::Category::Io => Category::Io,
                parse::error::Category::Eof => Category::Eof,
            },
        }
    }
}

/// Categorizes the cause of a `serde_lexpr::Error`.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Category {
    /// The error was caused by a failure to read or bytes from an input source.
    Io,

    /// The error was caused by input that was not a syntactically valid
    /// S-expression.
    Syntax,

    /// The error was caused by input data that was semantically incorrect.
    ///
    /// For example, an S-expression text representing a number is semantically
    /// incorrect when the type being deserialized into holds a String.
    Data,

    /// The error was caused by prematurely reaching the end of the input data.
    ///
    /// Callers that process streaming input may be interested in retrying the
    /// deserialization once more data is available.
    Eof,
}

impl From<Error> for io::Error {
    /// Convert a `serde_lexpr::Error` into an `io::Error`.
    ///
    /// S-expression syntax errors are turned into `InvalidData` IO errors.  EOF
    /// errors are turned into `UnexpectedEof` IO errors.
    ///
    /// ```
    /// use std::io;
    ///
    /// enum MyError {
    ///     Io(io::Error),
    ///     Lexpr(serde_lexpr::Error),
    /// }
    ///
    /// impl From<serde_lexpr::Error> for MyError {
    ///     fn from(err: serde_lexpr::Error) -> MyError {
    ///         use serde_lexpr::error::Category;
    ///         match err.classify() {
    ///             Category::Io => {
    ///                 MyError::Io(err.into())
    ///             }
    ///             Category::Syntax | Category::Eof | Category::Data => {
    ///                 MyError::Lexpr(err)
    ///             }
    ///         }
    ///     }
    /// }
    /// ```
    fn from(l: Error) -> Self {
        if let ErrorImpl::Io(err) = *l.0 {
            err
        } else {
            match l.classify() {
                Category::Io => unreachable!(),
                Category::Syntax | Category::Data => io::Error::new(io::ErrorKind::InvalidData, l),
                Category::Eof => io::Error::new(io::ErrorKind::UnexpectedEof, l),
            }
        }
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
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
            ErrorImpl::Message(msg, _) => Display::fmt(msg, f),
            ErrorImpl::Io(e) => Display::fmt(e, f),
            ErrorImpl::Parse(e) => Display::fmt(e, f),
        }
    }
}

impl Debug for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match &*self.0 {
            ErrorImpl::Message(msg, loc) => formatter
                .debug_tuple("Message")
                .field(msg)
                .field(loc)
                .finish(),
            ErrorImpl::Io(e) => formatter.debug_tuple("Io").field(e).finish(),
            ErrorImpl::Parse(e) => formatter.debug_tuple("Parse").field(e).finish(),
        }
    }
}

impl de::Error for Error {
    fn custom<T: Display>(msg: T) -> Error {
        Error(Box::new(ErrorImpl::Message(msg.to_string(), None)))
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
        Error(Box::new(ErrorImpl::Message(msg.to_string(), None)))
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
