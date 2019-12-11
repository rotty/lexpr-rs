//! When parsing S-expressions goes wrong.

use std::error;
use std::fmt::{self, Debug, Display};
use std::io;
use std::result;

/// This type represents the possible errors when parsing S-expression
/// data.
pub struct Error {
    /// This `Box` allows us to keep the size of `Error` as small as possible. A
    /// larger `Error` type was substantially slower due to all the functions
    /// that pass around `Result<T, Error>`.
    err: Box<ErrorImpl>,
}

/// Alias for a `Result` with the error type `lexpr::Error`.
pub type Result<T> = result::Result<T, Error>;

/// Location of a parse error.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    line: usize,
    column: usize,
}

impl Location {
    /// One-based line number at which the error was detected.
    ///
    /// Characters in the first line of the input (before the first newline
    /// character) are in line 1.
    pub fn line(&self) -> usize {
        self.line
    }

    /// One-based column number at which the error was detected.
    ///
    /// The first character in the input and any characters immediately
    /// following a newline character are in column 1.
    ///
    /// Note that errors may occur in column 0, for example if a read from an IO
    /// stream fails immediately following a previously read newline character.
    pub fn column(&self) -> usize {
        self.column
    }
}

impl Error {
    /// Location of the error in the input stream.
    pub fn location(&self) -> Option<Location> {
        self.err.location
    }

    /// Categorizes the cause of this error.
    ///
    /// - `Category::Io` - failure to read or write bytes on an IO stream
    /// - `Category::Syntax` - input that is not a syntactically valid S-experssion
    /// - `Category::Eof` - unexpected end of the input data
    pub fn classify(&self) -> Category {
        match self.err.code {
            ErrorCode::Io(_) => Category::Io,
            ErrorCode::EofWhileParsingList
            | ErrorCode::EofWhileParsingString
            | ErrorCode::EofWhileParsingVector
            | ErrorCode::EofWhileParsingValue
            | ErrorCode::EofWhileParsingCharacterConstant => Category::Eof,
            ErrorCode::ExpectedSomeIdent
            | ErrorCode::ExpectedSomeValue
            | ErrorCode::ExpectedVector
            | ErrorCode::ExpectedOctet
            | ErrorCode::MismatchedParenthesis
            | ErrorCode::InvalidEscape
            | ErrorCode::InvalidNumber
            | ErrorCode::InvalidCharacterConstant
            | ErrorCode::InvalidSymbol
            | ErrorCode::NumberOutOfRange
            | ErrorCode::InvalidUnicodeCodePoint
            | ErrorCode::TrailingCharacters
            | ErrorCode::RecursionLimitExceeded => Category::Syntax,
        }
    }

    /// Returns true if this error was caused by a failure to read or write
    /// bytes on an IO stream.
    pub fn is_io(&self) -> bool {
        self.classify() == Category::Io
    }

    /// Returns true if this error was caused by input that was not
    /// a syntactically valid S-expression.
    pub fn is_syntax(&self) -> bool {
        self.classify() == Category::Syntax
    }

    /// Returns true if this error was caused by prematurely reaching the end of
    /// the input data.
    ///
    /// Callers that process streaming input may be interested in retrying the
    /// deserialization once more data is available.
    pub fn is_eof(&self) -> bool {
        self.classify() == Category::Eof
    }
}

/// Categorizes the cause of a `lexpr::parse::Error`.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Category {
    /// The error was caused by a failure to read or bytes from an input source.
    Io,

    /// The error was caused by input that was not a syntactically valid
    /// S-expression.
    Syntax,

    /// The error was caused by prematurely reaching the end of the input data.
    ///
    /// Callers that process streaming input may be interested in retrying the
    /// deserialization once more data is available.
    Eof,
}

impl From<Error> for io::Error {
    /// Convert a `lexpr::parse::Error` into an `io::Error`.
    ///
    /// S-expression syntax errors are turned into `InvalidData` IO errors.  EOF
    /// errors are turned into `UnexpectedEof` IO errors.
    ///
    /// ```
    /// use std::io;
    ///
    /// enum MyError {
    ///     Io(io::Error),
    ///     Parse(lexpr::parse::Error),
    /// }
    ///
    /// impl From<lexpr::parse::Error> for MyError {
    ///     fn from(err: lexpr::parse::Error) -> MyError {
    ///         use lexpr::parse::error::Category;
    ///         match err.classify() {
    ///             Category::Io => {
    ///                 MyError::Io(err.into())
    ///             }
    ///             Category::Syntax | Category::Eof => {
    ///                 MyError::Parse(err)
    ///             }
    ///         }
    ///     }
    /// }
    /// ```
    fn from(l: Error) -> Self {
        if let ErrorCode::Io(err) = l.err.code {
            err
        } else {
            match l.classify() {
                Category::Io => unreachable!(),
                Category::Syntax => io::Error::new(io::ErrorKind::InvalidData, l),
                Category::Eof => io::Error::new(io::ErrorKind::UnexpectedEof, l),
            }
        }
    }
}

impl Error {
    pub(crate) fn syntax(code: ErrorCode, line: usize, column: usize) -> Self {
        Error {
            err: Box::new(ErrorImpl {
                code,
                location: Some(Location { line, column }),
            }),
        }
    }

    pub(crate) fn io(error: io::Error) -> Self {
        Error {
            err: Box::new(ErrorImpl {
                code: ErrorCode::Io(error),
                location: None,
            }),
        }
    }
}

struct ErrorImpl {
    code: ErrorCode,
    location: Option<Location>,
}

pub(crate) enum ErrorCode {
    /// Some IO error occurred while serializing or deserializing.
    Io(io::Error),

    /// EOF while parsing a list.
    EofWhileParsingList,

    /// EOF while parsing a vector.
    EofWhileParsingVector,

    /// EOF while parsing a string.
    EofWhileParsingString,

    /// EOF while parsing a S-expression value.
    EofWhileParsingValue,

    // EOF while parsing character constant.
    EofWhileParsingCharacterConstant,

    /// Expected to parse either a `#t`, `#f`, or a `#nil`.
    ExpectedSomeIdent,

    /// Used a mismatching parenthesis to close a list or vector.
    MismatchedParenthesis,

    /// Expected this character to start an S-expression value.
    ExpectedSomeValue,

    /// Expected a vector.
    ExpectedVector,

    /// Expected an octet (integer in range 0-255).
    ExpectedOctet,

    /// Invalid hex escape code.
    InvalidEscape,

    /// Invalid number.
    InvalidNumber,

    /// Invalid symbol.
    InvalidSymbol,

    /// Number is bigger than the maximum value of its type.
    NumberOutOfRange,

    /// Invalid unicode code point.
    InvalidUnicodeCodePoint,

    /// Invalid character constant.
    InvalidCharacterConstant,

    /// S-expression has non-whitespace trailing characters after the value.
    TrailingCharacters,

    /// Encountered nesting of S-expression maps and arrays more than 128 layers deep.
    RecursionLimitExceeded,
}

impl Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            ErrorCode::Io(ref err) => Display::fmt(err, f),
            ErrorCode::EofWhileParsingList => f.write_str("EOF while parsing a list"),
            ErrorCode::EofWhileParsingVector => f.write_str("EOF while parsing a vector"),
            ErrorCode::EofWhileParsingString => f.write_str("EOF while parsing a string"),
            ErrorCode::EofWhileParsingValue => f.write_str("EOF while parsing a value"),
            ErrorCode::EofWhileParsingCharacterConstant => {
                f.write_str("EOF while parsing a character constant")
            }
            ErrorCode::ExpectedSomeIdent => f.write_str("expected ident"),
            ErrorCode::ExpectedSomeValue => f.write_str("expected value"),
            ErrorCode::ExpectedVector => f.write_str("expected vector"),
            ErrorCode::ExpectedOctet => f.write_str("expected octet"),
            ErrorCode::InvalidEscape => f.write_str("invalid escape"),
            ErrorCode::InvalidNumber => f.write_str("invalid number"),
            ErrorCode::InvalidSymbol => f.write_str("invalid symbol"),
            ErrorCode::MismatchedParenthesis => f.write_str("mismatched parenthesis"),
            ErrorCode::NumberOutOfRange => f.write_str("number out of range"),
            ErrorCode::InvalidUnicodeCodePoint => f.write_str("invalid unicode code point"),
            ErrorCode::InvalidCharacterConstant => f.write_str("invalid character constant"),
            ErrorCode::TrailingCharacters => f.write_str("trailing characters"),
            ErrorCode::RecursionLimitExceeded => f.write_str("recursion limit exceeded"),
        }
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self.err.code {
            ErrorCode::Io(ref err) => Some(err),
            _ => None,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&*self.err, f)
    }
}

impl Display for ErrorImpl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(loc) = self.location {
            write!(
                f,
                "{} at line {} column {}",
                self.code, loc.line, loc.column
            )
        } else {
            Display::fmt(&self.code, f)
        }
    }
}

// Remove two layers of verbosity from the debug representation. Humans often
// end up seeing this representation because it is what unwrap() shows.
impl Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(loc) = self.err.location {
            write!(
                f,
                "Error({:?}, line: {}, column: {})",
                self.err.code.to_string(),
                loc.line,
                loc.column,
            )
        } else {
            write!(f, "Error({:?})", self.err.code.to_string())
        }
    }
}
