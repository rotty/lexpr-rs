use std::io;
use std::str;
use std::u64;

use crate::error::{Error, ErrorCode};
use crate::read::{self, Reference};
use crate::style::KeywordStyle;
use crate::{Number, Result, Value};

pub use read::{IoRead, Read, SliceRead, StrRead};

/// Parser for the S-expression text representation.
///
/// This type, given a input source, provides the [`parse`] method,
/// which can be used to read a single S-expression from the input
/// source.
///
/// [`parse`]: struct.Parser.html#method.parse
pub struct Parser<R> {
    read: R,
    scratch: Vec<u8>,
    remaining_depth: u8,
}

/// Various options to influence parser behavior.
pub struct Options {
    keyword_styles: Vec<KeywordStyle>,
    nil_handling: NilHandling,
}

pub enum NilHandling {
    /// Treat `nil` like as the empty list. This the behavior of Emacs
    /// Lisp and Common Lisp.
    ///
    /// In the parsed `Value`, the empty list, written as `()` and the
    /// empty list, written as `nil` are both represented by the
    /// `Value::List` variant with zero elements.
    EmptyList,

    /// Treat `nil` as a regular symbol. This is the behavior found in
    /// Scheme.
    ///
    /// The parsed `Value` will be equal to `Value::symbol("nil")`.
    Symbol,
}

impl Options {
    /// Construct an empty set of options. This corresponds most
    /// closely to the Scheme dialect of S-expressions:
    ///
    /// - The identifier `nil` is treated as regular symbol (i.e.,
    ///   `NilHandling::Symbol`).
    /// - No style of keywords is recognized.
    pub fn new() -> Self {
        Options {
            keyword_styles: vec![],
            nil_handling: NilHandling::Symbol,
        }
    }
    /// Add `style` to the recognized keyword styles.
    pub fn with_keyword_style(&mut self, style: KeywordStyle) -> &mut Self {
        self.keyword_styles.push(style);
        self
    }
    pub fn with_nil_handling(&mut self, handling: NilHandling) -> &mut Self {
        self.nil_handling = handling;
        self
    }
}

impl<'de, R> Parser<R>
where
    R: read::Read<'de>,
{
    /// Create a S-expression parser from one of the possible sexpr
    /// input sources.
    ///
    /// Typically it is more convenient to use one of these methods
    /// instead:
    ///
    ///   - `Parser::from_str`
    ///   - `Parser::from_bytes`
    ///   - `Parser::from_reader
    pub fn new(read: R) -> Self {
        Parser {
            read,
            scratch: Vec::with_capacity(128),
            remaining_depth: 128,
        }
    }
}

impl<R> Parser<read::IoRead<R>>
where
    R: io::Read,
{
    /// Creates a S-expression deserializer from an `io::Read`.
    pub fn from_reader(reader: R) -> Self {
        Parser::new(read::IoRead::new(reader))
    }
}

impl<'a> Parser<read::SliceRead<'a>> {
    /// Creates a S-expression deserializer from a `&[u8]`.
    pub fn from_slice(bytes: &'a [u8]) -> Self {
        Parser::new(read::SliceRead::new(bytes))
    }
}

impl<'a> Parser<read::StrRead<'a>> {
    /// Creates a S-expression deserializer from a `&str`.
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(s: &'a str) -> Self {
        Parser::new(read::StrRead::new(s))
    }
}

macro_rules! overflow {
    ($a:ident * 10 + $b:ident, $c:expr) => {
        $a >= $c / 10 && ($a > $c / 10 || $b > $c % 10)
    };
}

impl<'de, R: Read<'de>> Parser<R> {
    /// The `Parser::end` method should be called after a value has been fully deserialized.
    /// This allows the `Parser` to validate that the input stream is at the end or that it
    /// only has trailing whitespace.
    pub fn end(&mut self) -> Result<()> {
        match self.parse_whitespace()? {
            Some(_) => Err(self.peek_error(ErrorCode::TrailingCharacters)),
            None => Ok(()),
        }
    }

    /// Turn a Sexp deserializer into an iterator over values of type T.
    // TODO: Deserializer<R> cannot implement `IntoIterator`, as the
    // returned iterator is generic over `T`.
    // #[allow(clippy::should_implement_trait)]
    // pub fn into_iter<T>(self) -> StreamDeserializer<'de, R, T>
    // where
    //     T: de::Deserialize<'de>,
    // {
    //     // This cannot be an implementation of std::iter::IntoIterator because
    //     // we need the caller to choose what T is.
    //     let offset = self.read.byte_offset();
    //     StreamDeserializer {
    //         de: self,
    //         offset,
    //         output: PhantomData,
    //         lifetime: PhantomData,
    //     }
    // }

    fn peek(&mut self) -> Result<Option<u8>> {
        self.read.peek()
    }

    fn peek_or_null(&mut self) -> Result<u8> {
        Ok(self.peek()?.unwrap_or(b'\x00'))
    }

    fn eat_char(&mut self) {
        self.read.discard();
    }

    fn next_char(&mut self) -> Result<Option<u8>> {
        self.read.next()
    }

    fn next_char_or_null(&mut self) -> Result<u8> {
        Ok(self.next_char()?.unwrap_or(b'\x00'))
    }

    /// Error caused by a byte from next_char().
    fn error(&mut self, reason: ErrorCode) -> Error {
        let pos = self.read.position();
        Error::syntax(reason, pos.line, pos.column)
    }

    /// Error caused by a byte from peek().
    fn peek_error(&mut self, reason: ErrorCode) -> Error {
        let pos = self.read.peek_position();
        Error::syntax(reason, pos.line, pos.column)
    }

    /// Returns the first non-whitespace byte without consuming it, or `None` if
    /// EOF is encountered.
    fn parse_whitespace(&mut self) -> Result<Option<u8>> {
        loop {
            match self.peek()? {
                Some(b' ') | Some(b'\n') | Some(b'\t') | Some(b'\r') => {
                    self.eat_char();
                }
                other => {
                    return Ok(other);
                }
            }
        }
    }

    /// Parse a single S-expression from the input source.
    ///
    /// ```
    /// # use lexpr::{sexp, Parser};
    /// let mut parser = Parser::from_str(r#"foo ("bar" . 3.14) #:baz (1 2 3)"#);
    /// assert_eq!(parser.parse().unwrap(), sexp!(foo));
    /// assert_eq!(parser.parse().unwrap(), sexp!(("bar" . 3.14)));
    /// assert_eq!(parser.parse().unwrap(), sexp!(#:baz));
    /// assert_eq!(parser.parse().unwrap(), sexp!((1 2 3)));
    /// assert!(parser.end().is_ok());
    /// ```
    pub fn parse(&mut self) -> Result<Value> {
        let peek = match self.parse_whitespace()? {
            Some(b) => b,
            None => {
                return Err(self.peek_error(ErrorCode::EofWhileParsingValue));
            }
        };

        let value = match peek {
            b'#' => {
                self.eat_char();
                match self.next_char()? {
                    Some(b't') => Ok(Value::from(true)),
                    Some(b'f') => Ok(Value::from(false)),
                    Some(b'n') => {
                        self.expect_ident(b"il")?;
                        Ok(Value::nil())
                    }
                    Some(b':') => Ok(Value::keyword(self.parse_symbol()?)),
                    Some(_) => Err(self.peek_error(ErrorCode::ExpectedSomeIdent)),
                    None => Err(self.peek_error(ErrorCode::EofWhileParsingValue)),
                }
            }
            b'-' => {
                // FIXME: might be a symbol instead
                self.eat_char();
                Ok(Value::from(self.parse_integer(false)?))
            }
            b'0'...b'9' => Ok(Value::from(self.parse_integer(true)?)),
            b'"' => {
                self.eat_char();
                self.scratch.clear();
                match self.read.parse_str(&mut self.scratch)? {
                    Reference::Borrowed(s) => Ok(Value::from(s)),
                    Reference::Copied(s) => Ok(Value::from(s)),
                }
            }
            b'(' => {
                self.remaining_depth -= 1;
                if self.remaining_depth == 0 {
                    return Err(self.peek_error(ErrorCode::RecursionLimitExceeded));
                }

                self.eat_char();
                let ret = self.parse_list_elements();

                self.remaining_depth += 1;

                match (ret, self.end_seq()) {
                    (Ok(ret), Ok(())) => Ok(ret),
                    (Err(err), _) | (_, Err(err)) => Err(err),
                }
            }
            b'a'...b'z' | b'A'...b'Z' => Ok(Value::symbol(self.parse_symbol()?)),
            _ => {
                if SYMBOL_EXTENDED.contains(&peek) {
                    Ok(Value::symbol(self.parse_symbol()?))
                } else {
                    Err(self.peek_error(ErrorCode::ExpectedSomeValue))
                }
            }
        };

        match value {
            Ok(value) => Ok(value),
            // The de::Error and From<de::value::Error> impls both create errors
            // with unknown line and column. Fill in the position here by
            // looking at the current index in the input. There is no way to
            // tell whether this should call `error` or `peek_error` so pick the
            // one that seems correct more often. Worst case, the position is
            // off by one character.
            Err(err) => Err(err.fix_position(|code| self.error(code))),
        }
    }

    fn parse_symbol(&mut self) -> Result<String> {
        self.scratch.clear();
        match self.read.parse_symbol(&mut self.scratch)? {
            Reference::Borrowed(s) => Ok(s.into()),
            Reference::Copied(s) => Ok(s.into()),
        }
    }

    fn expect_ident(&mut self, ident: &[u8]) -> Result<()> {
        for c in ident {
            if Some(*c) != self.next_char()? {
                return Err(self.error(ErrorCode::ExpectedSomeIdent));
            }
        }

        Ok(())
    }

    fn parse_list_elements(&mut self) -> Result<Value> {
        fn form_list(mut list: Vec<Value>, tail: Value) -> Value {
            match tail {
                Value::Atom(atom) => Value::ImproperList(list, atom),
                Value::List(rest_list) => {
                    list.extend(rest_list);
                    Value::List(list)
                }
                Value::ImproperList(rest_list, rest) => {
                    list.extend(rest_list);
                    Value::ImproperList(list, rest)
                }
            }
        }
        let mut list = Vec::new();
        loop {
            match self.parse_whitespace() {
                Err(e) => return Err(e),
                Ok(Some(c)) => match c {
                    b')' => return Ok(Value::List(list)),
                    b'.' => {
                        self.eat_char();
                        let tail = self.parse()?;
                        match self.parse_whitespace()? {
                            Some(b')') => return Ok(form_list(list, tail)),
                            Some(_) => return Err(self.peek_error(ErrorCode::TrailingCharacters)),
                            None => return Err(self.peek_error(ErrorCode::EofWhileParsingList)),
                        }
                    }
                    _ => list.push(self.parse()?),
                },
                Ok(None) => return Err(self.peek_error(ErrorCode::EofWhileParsingList)),
            }
        }
    }

    fn parse_integer(&mut self, pos: bool) -> Result<Number> {
        match self.next_char_or_null()? {
            b'0' => {
                // There can be only one leading '0'.
                match self.peek_or_null()? {
                    b'0'...b'9' => Err(self.peek_error(ErrorCode::InvalidNumber)),
                    _ => self.parse_number(pos, 0),
                }
            }
            c @ b'1'...b'9' => {
                let mut res = u64::from(c - b'0');

                loop {
                    match self.peek_or_null()? {
                        c @ b'0'...b'9' => {
                            self.eat_char();
                            let digit = u64::from(c - b'0');

                            // We need to be careful with overflow. If we can, try to keep the
                            // number as a `u64` until we grow too large. At that point, switch to
                            // parsing the value as a `f64`.
                            if overflow!(res * 10 + digit, u64::MAX) {
                                return Ok(Number::from(self.parse_long_integer(
                                    pos, res, 1, // res * 10^1
                                )?));
                            }

                            res = res * 10 + digit;
                        }
                        _ => {
                            return self.parse_number(pos, res);
                        }
                    }
                }
            }
            _ => Err(self.error(ErrorCode::InvalidNumber)),
        }
    }

    fn parse_long_integer(
        &mut self,
        pos: bool,
        significand: u64,
        mut exponent: i32,
    ) -> Result<f64> {
        loop {
            match self.peek_or_null()? {
                b'0'...b'9' => {
                    self.eat_char();
                    // This could overflow... if your integer is gigabytes long.
                    // Ignore that possibility.
                    exponent += 1;
                }
                b'.' => {
                    return self.parse_decimal(pos, significand, exponent);
                }
                // b'e' | b'E' => {
                //     return self.parse_exponent(pos, significand, exponent);
                // }
                _ => {
                    return self.f64_from_parts(pos, significand, exponent);
                }
            }
        }
    }

    fn parse_number(&mut self, pos: bool, significand: u64) -> Result<Number> {
        Ok(match self.peek_or_null()? {
            b'.' => Number::from(self.parse_decimal(pos, significand, 0)?),
            // b'e' | b'E' => Number::F64(try!(self.parse_exponent(pos, significand, 0))),
            _ => {
                if pos {
                    Number::from(significand)
                } else {
                    let neg = (significand as i64).wrapping_neg();

                    // Convert into a float if we underflow.
                    if neg > 0 {
                        Number::from(-(significand as f64))
                    } else {
                        Number::from(neg)
                    }
                }
            }
        })
    }

    fn parse_decimal(&mut self, pos: bool, mut significand: u64, mut exponent: i32) -> Result<f64> {
        self.eat_char();

        let mut at_least_one_digit = false;
        while let c @ b'0'...b'9' = self.peek_or_null()? {
            self.eat_char();
            let digit = u64::from(c - b'0');
            at_least_one_digit = true;

            if overflow!(significand * 10 + digit, u64::MAX) {
                // The next multiply/add would overflow, so just ignore all
                // further digits.
                while let b'0'...b'9' = self.peek_or_null()? {
                    self.eat_char();
                }
                break;
            }

            significand = significand * 10 + digit;
            exponent -= 1;
        }

        if !at_least_one_digit {
            return Err(self.peek_error(ErrorCode::InvalidNumber));
        }

        match self.peek_or_null()? {
            // b'e' | b'E' => self.parse_exponent(pos, significand, exponent),
            _ => self.f64_from_parts(pos, significand, exponent),
        }
    }

    fn f64_from_parts(&mut self, pos: bool, significand: u64, mut exponent: i32) -> Result<f64> {
        if significand >= 1u64 << 52 {
            // Slow path -- we would potentially lose digits by
            // casting `significand` (which is bigger than 2^52) to
            // `f64`, so we delegate floating number parsing to the
            // standard library.
            self.scratch.clear();
            itoa::write(&mut self.scratch, significand).unwrap();
            self.scratch.push(b'e');
            itoa::write(&mut self.scratch, exponent).unwrap();
            // Unsafe should be OK here, as `itoa::write()` should
            // never produce non-ASCII output.
            let f: f64 = unsafe { str::from_utf8_unchecked(&self.scratch) }
                .parse()
                .map_err(|_| self.error(ErrorCode::NumberOutOfRange))?;
            if !pos {
                return Ok(f * -1.0);
            }
            return Ok(f);
        }
        let mut f = significand as f64;
        loop {
            match POW10.get(exponent.abs() as usize) {
                Some(&pow) => {
                    if exponent >= 0 {
                        f *= pow;
                        if f.is_infinite() {
                            return Err(self.error(ErrorCode::NumberOutOfRange));
                        }
                    } else {
                        f /= pow;
                    }
                    break;
                }
                None => {
                    if f == 0.0 {
                        break;
                    }
                    if exponent >= 0 {
                        return Err(self.error(ErrorCode::NumberOutOfRange));
                    }
                    f /= 1e308;
                    exponent += 308;
                }
            }
        }
        Ok(if pos { f } else { -f })
    }

    fn end_seq(&mut self) -> Result<()> {
        match self.parse_whitespace()? {
            Some(b')') => {
                self.eat_char();
                Ok(())
            }
            Some(_) => Err(self.peek_error(ErrorCode::TrailingCharacters)),
            None => Err(self.peek_error(ErrorCode::EofWhileParsingList)),
        }
    }
}

const SYMBOL_EXTENDED: [u8; 17] = [
    b'!', b'$', b'%', b'&', b'*', b'+', /* | b'-' FIXME, */ b'.', b'/', b':', b'<', b'=',
    b'>', b'?', b'@', b'^', b'_', b'~',
];

#[rustfmt::skip]
static POW10: [f64; 309] =
    [1e000, 1e001, 1e002, 1e003, 1e004, 1e005, 1e006, 1e007, 1e008, 1e009,
     1e010, 1e011, 1e012, 1e013, 1e014, 1e015, 1e016, 1e017, 1e018, 1e019,
     1e020, 1e021, 1e022, 1e023, 1e024, 1e025, 1e026, 1e027, 1e028, 1e029,
     1e030, 1e031, 1e032, 1e033, 1e034, 1e035, 1e036, 1e037, 1e038, 1e039,
     1e040, 1e041, 1e042, 1e043, 1e044, 1e045, 1e046, 1e047, 1e048, 1e049,
     1e050, 1e051, 1e052, 1e053, 1e054, 1e055, 1e056, 1e057, 1e058, 1e059,
     1e060, 1e061, 1e062, 1e063, 1e064, 1e065, 1e066, 1e067, 1e068, 1e069,
     1e070, 1e071, 1e072, 1e073, 1e074, 1e075, 1e076, 1e077, 1e078, 1e079,
     1e080, 1e081, 1e082, 1e083, 1e084, 1e085, 1e086, 1e087, 1e088, 1e089,
     1e090, 1e091, 1e092, 1e093, 1e094, 1e095, 1e096, 1e097, 1e098, 1e099,
     1e100, 1e101, 1e102, 1e103, 1e104, 1e105, 1e106, 1e107, 1e108, 1e109,
     1e110, 1e111, 1e112, 1e113, 1e114, 1e115, 1e116, 1e117, 1e118, 1e119,
     1e120, 1e121, 1e122, 1e123, 1e124, 1e125, 1e126, 1e127, 1e128, 1e129,
     1e130, 1e131, 1e132, 1e133, 1e134, 1e135, 1e136, 1e137, 1e138, 1e139,
     1e140, 1e141, 1e142, 1e143, 1e144, 1e145, 1e146, 1e147, 1e148, 1e149,
     1e150, 1e151, 1e152, 1e153, 1e154, 1e155, 1e156, 1e157, 1e158, 1e159,
     1e160, 1e161, 1e162, 1e163, 1e164, 1e165, 1e166, 1e167, 1e168, 1e169,
     1e170, 1e171, 1e172, 1e173, 1e174, 1e175, 1e176, 1e177, 1e178, 1e179,
     1e180, 1e181, 1e182, 1e183, 1e184, 1e185, 1e186, 1e187, 1e188, 1e189,
     1e190, 1e191, 1e192, 1e193, 1e194, 1e195, 1e196, 1e197, 1e198, 1e199,
     1e200, 1e201, 1e202, 1e203, 1e204, 1e205, 1e206, 1e207, 1e208, 1e209,
     1e210, 1e211, 1e212, 1e213, 1e214, 1e215, 1e216, 1e217, 1e218, 1e219,
     1e220, 1e221, 1e222, 1e223, 1e224, 1e225, 1e226, 1e227, 1e228, 1e229,
     1e230, 1e231, 1e232, 1e233, 1e234, 1e235, 1e236, 1e237, 1e238, 1e239,
     1e240, 1e241, 1e242, 1e243, 1e244, 1e245, 1e246, 1e247, 1e248, 1e249,
     1e250, 1e251, 1e252, 1e253, 1e254, 1e255, 1e256, 1e257, 1e258, 1e259,
     1e260, 1e261, 1e262, 1e263, 1e264, 1e265, 1e266, 1e267, 1e268, 1e269,
     1e270, 1e271, 1e272, 1e273, 1e274, 1e275, 1e276, 1e277, 1e278, 1e279,
     1e280, 1e281, 1e282, 1e283, 1e284, 1e285, 1e286, 1e287, 1e288, 1e289,
     1e290, 1e291, 1e292, 1e293, 1e294, 1e295, 1e296, 1e297, 1e298, 1e299,
     1e300, 1e301, 1e302, 1e303, 1e304, 1e305, 1e306, 1e307, 1e308];

fn from_trait<'de, R>(read: R) -> Result<Value>
where
    R: Read<'de>,
{
    let mut parser = Parser::new(read);
    let value = parser.parse()?;
    parser.end()?;

    Ok(value)
}

#[doc(hidden)]
pub fn from_reader(rdr: impl io::Read) -> Result<Value> {
    from_trait(read::IoRead::new(rdr))
}

#[doc(hidden)]
pub fn from_slice(bytes: &[u8]) -> Result<Value> {
    from_trait(read::SliceRead::new(bytes))
}

#[doc(hidden)]
pub fn from_str(s: &str) -> Result<Value> {
    from_trait(read::StrRead::new(s))
}

#[cfg(test)]
mod tests {
    use super::{from_str, Parser};
    use crate::{Atom, Value};

    #[test]
    fn test_atoms_default() {
        let mut parser = Parser::from_str("foo-symbol #:bar-keyword #t #f #nil 100 -42 4.5");
        for value in vec![Value::symbol("foo-symbol"),
                          Value::keyword("bar-keyword"),
                          Value::from(true),
                          Value::from(false),
                          Value::nil(),
                          Value::from(100),
                          Value::from(-42),
                          Value::from(4.5)] {
            assert_eq!(parser.parse().unwrap(), value);
        }
        assert_eq!(parser.end().unwrap(), ());
    }

    #[test]
    fn test_lists_default() {
        assert_eq!(from_str("()").unwrap(), Value::from(vec![]));
        assert_eq!(
            from_str("(1 . (2 . (3 . ())))").unwrap(),
            Value::list(vec![1u32, 2, 3])
        );
        assert_eq!(
            from_str("(1 . 2)").unwrap(),
            Value::improper_list(vec![Value::from(1)], Atom::from(2))
        );
        assert_eq!(
            from_str("(1 hello . 2)").unwrap(),
            Value::improper_list(vec![Value::from(1), Value::symbol("hello")], Atom::from(2))
        );
    }

    #[test]
    fn test_broken_lists_default() {
        assert!(from_str("(.)").is_err());
        assert!(from_str("(1 2 .)").is_err());
        assert!(from_str("(1 2 . 3 4)").is_err());
        assert!(from_str("(1 2 . 3 . 4)").is_err());
    }

    #[test]
    fn test_list_nil_default() {
        assert_eq!(
            from_str("(1 . #nil)").unwrap(),
            Value::improper_list(vec![1], Atom::Nil)
        );
        assert_eq!(from_str("(#nil)").unwrap(), Value::list(vec![Value::nil()]));
    }

    // #[test]
    // fn test_list_elisp() {
    //     let parser = Parser::new()
    // }
}
