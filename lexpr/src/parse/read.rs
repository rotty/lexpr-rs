use std::ops::Deref;
use std::{char, cmp, io, str, u32};

use super::error::{Error, ErrorCode, Result};
use super::iter::LineColIterator;

/// Trait used by the parser for iterating over input.
///
/// This trait is manually "specialized" for iterating over
/// `&[u8]`. Once feature(specialization) is stable we can use actual
/// specialization.
///
/// This trait is sealed and cannot be implemented for types outside of
/// `lexpr`.
pub trait Read<'de>: private::Sealed {
    #[doc(hidden)]
    fn next(&mut self) -> Result<Option<u8>>;
    #[doc(hidden)]
    fn peek(&mut self) -> Result<Option<u8>>;

    /// Only valid after a call to peek(). Discards the peeked byte.
    #[doc(hidden)]
    fn discard(&mut self);

    /// Position of the most recent call to next().
    ///
    /// The most recent call was probably next() and not peek(), but this method
    /// should try to return a sensible result if the most recent call was
    /// actually peek() because we don't always know.
    ///
    /// Only called in case of an error, so performance is not important.
    #[doc(hidden)]
    fn position(&self) -> Position;

    /// Position of the most recent call to peek().
    ///
    /// The most recent call was probably peek() and not next(), but this method
    /// should try to return a sensible result if the most recent call was
    /// actually next() because we don't always know.
    ///
    /// Only called in case of an error, so performance is not important.
    #[doc(hidden)]
    fn peek_position(&self) -> Position;

    /// Offset from the beginning of the input to the next byte that would be
    /// returned by next() or peek().
    #[doc(hidden)]
    fn byte_offset(&self) -> usize;

    /// Assumes the previous byte was a quotation mark. Parses a string with
    /// R6RS escapes until the next quotation mark using the given scratch space
    /// if necessary. The scratch space is initially empty.
    #[doc(hidden)]
    fn parse_str<'s>(&'s mut self, scratch: &'s mut Vec<u8>) -> Result<Reference<'de, 's, str>>;

    /// Parses an unescaped string until the next whitespace or list close..
    #[doc(hidden)]
    fn parse_symbol<'s>(&'s mut self, scratch: &'s mut Vec<u8>) -> Result<Reference<'de, 's, str>>;

    /// Assumes the previous byte was a quotation mark. Parses a JSON-escaped
    /// string until the next quotation mark using the given scratch space if
    /// necessary. The scratch space is initially empty.
    ///
    /// This function returns the raw bytes in the string with escape sequences
    /// expanded but without performing unicode validation.
    #[doc(hidden)]
    fn parse_str_raw<'s>(
        &'s mut self,
        scratch: &'s mut Vec<u8>,
    ) -> Result<Reference<'de, 's, [u8]>>;

    /// Assumes the previous byte was a hex escape sequnce ('\x') in a string.
    /// Parses the hexadecimal sequence, including the terminating semicolon.
    #[doc(hidden)]
    fn decode_hex_escape(&mut self) -> Result<u32>;
}

pub struct Position {
    pub line: usize,
    pub column: usize,
}

pub enum Reference<'b, 'c, T: ?Sized + 'static> {
    Borrowed(&'b T),
    Copied(&'c T),
}

impl<'b, 'c, T: ?Sized + 'static> Deref for Reference<'b, 'c, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match *self {
            Reference::Borrowed(b) => b,
            Reference::Copied(c) => c,
        }
    }
}

/// S-expression input source that reads from a std::io input stream.
pub struct IoRead<R>
where
    R: io::Read,
{
    iter: LineColIterator<io::Bytes<R>>,
    /// Temporary storage of peeked byte.
    ch: Option<u8>,
}

/// S-expression input source that reads from a slice of bytes.
//
// This is more efficient than other iterators because peek() can be read-only
// and we can compute line/col position only if an error happens.
pub struct SliceRead<'a> {
    slice: &'a [u8],
    /// Index of the *next* byte that will be returned by next() or peek().
    index: usize,
}

/// S-expression input source that reads from a UTF-8 string.
//
// Able to elide UTF-8 checks by assuming that the input is valid UTF-8.
pub struct StrRead<'a> {
    delegate: SliceRead<'a>,
}

// Prevent users from implementing the Read trait.
mod private {
    pub trait Sealed {}
}

//////////////////////////////////////////////////////////////////////////////

impl<R> IoRead<R>
where
    R: io::Read,
{
    /// Create a S-expression input source to read from a std::io input stream.
    pub fn new(reader: R) -> Self {
        IoRead {
            iter: LineColIterator::new(reader.bytes()),
            ch: None,
        }
    }
}

impl<R> private::Sealed for IoRead<R> where R: io::Read {}

impl<R> IoRead<R>
where
    R: io::Read,
{
    fn parse_str_bytes<'s, T, F>(
        &'s mut self,
        scratch: &'s mut Vec<u8>,
        validate: bool,
        result: F,
    ) -> Result<T>
    where
        T: 's,
        F: FnOnce(&'s Self, &'s [u8]) -> Result<T>,
    {
        loop {
            let ch = next_or_eof(self)?;
            if !ESCAPE[ch as usize] {
                scratch.push(ch);
                continue;
            }
            match ch {
                b'"' => {
                    return result(self, scratch);
                }
                b'\\' => {
                    parse_r6rs_escape(self, scratch)?;
                }
                _ => {
                    if validate {
                        return error(self, ErrorCode::ControlCharacterWhileParsingString);
                    }
                    scratch.push(ch);
                }
            }
        }
    }

    fn parse_symbol_bytes<'s, T, F>(&'s mut self, scratch: &'s mut Vec<u8>, result: F) -> Result<T>
    where
        T: 's,
        F: FnOnce(&'s Self, &'s [u8]) -> Result<T>,
    {
        // FIXME: this should fail on being given a lone '.'
        loop {
            match self.peek()? {
                Some(b' ') | Some(b'\n') | Some(b'\t') | Some(b'\r') | Some(b')') | None => {
                    return result(self, scratch);
                }
                Some(ch) => {
                    self.discard();
                    scratch.push(ch);
                }
            }
        }
    }
}

impl<'de, R> Read<'de> for IoRead<R>
where
    R: io::Read,
{
    #[inline]
    fn next(&mut self) -> Result<Option<u8>> {
        match self.ch.take() {
            Some(ch) => Ok(Some(ch)),
            None => match self.iter.next() {
                Some(Err(err)) => Err(Error::io(err)),
                Some(Ok(ch)) => Ok(Some(ch)),
                None => Ok(None),
            },
        }
    }

    #[inline]
    fn peek(&mut self) -> Result<Option<u8>> {
        match self.ch {
            Some(ch) => Ok(Some(ch)),
            None => match self.iter.next() {
                Some(Err(err)) => Err(Error::io(err)),
                Some(Ok(ch)) => {
                    self.ch = Some(ch);
                    Ok(self.ch)
                }
                None => Ok(None),
            },
        }
    }

    #[inline]
    fn discard(&mut self) {
        self.ch = None;
    }

    fn position(&self) -> Position {
        Position {
            line: self.iter.line(),
            column: self.iter.col(),
        }
    }

    fn peek_position(&self) -> Position {
        // The LineColIterator updates its position during peek() so it has the
        // right one here.
        self.position()
    }

    fn byte_offset(&self) -> usize {
        match self.ch {
            Some(_) => self.iter.byte_offset() - 1,
            None => self.iter.byte_offset(),
        }
    }

    fn parse_str<'s>(&'s mut self, scratch: &'s mut Vec<u8>) -> Result<Reference<'de, 's, str>> {
        self.parse_str_bytes(scratch, true, as_str)
            .map(Reference::Copied)
    }

    fn parse_str_raw<'s>(
        &'s mut self,
        scratch: &'s mut Vec<u8>,
    ) -> Result<Reference<'de, 's, [u8]>> {
        self.parse_str_bytes(scratch, false, |_, bytes| Ok(bytes))
            .map(Reference::Copied)
    }

    fn parse_symbol<'s>(&'s mut self, scratch: &'s mut Vec<u8>) -> Result<Reference<'de, 's, str>> {
        self.parse_symbol_bytes(scratch, as_str)
            .map(Reference::Copied)
    }

    fn decode_hex_escape(&mut self) -> Result<u32> {
        let mut n = 0;
        loop {
            let next = next_or_eof(self)?;
            if next == b';' {
                return Ok(n);
            }
            match decode_hex_val(next) {
                None => return error(self, ErrorCode::InvalidEscape),
                Some(val) => {
                    if val >= (1 << 24) {
                        // A codepoint never has more than 24 bits
                        return error(self, ErrorCode::InvalidUnicodeCodePoint);
                    }
                    n = (n << 4) + u32::from(val);
                }
            }
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

impl<'a> SliceRead<'a> {
    /// Create a S-expression input source to read from a slice of bytes.
    pub fn new(slice: &'a [u8]) -> Self {
        SliceRead { slice, index: 0 }
    }

    fn position_of_index(&self, i: usize) -> Position {
        let mut position = Position { line: 1, column: 0 };
        for ch in &self.slice[..i] {
            match *ch {
                b'\n' => {
                    position.line += 1;
                    position.column = 0;
                }
                _ => {
                    position.column += 1;
                }
            }
        }
        position
    }

    fn parse_symbol_bytes<'s, T: ?Sized, F>(
        &'s mut self,
        scratch: &'s mut Vec<u8>,
        result: F,
    ) -> Result<Reference<'a, 's, T>>
    where
        T: 's,
        F: for<'f> FnOnce(&'s Self, &'f [u8]) -> Result<&'f T>,
    {
        // Index of the first byte not yet copied into the scratch space.
        let start = self.index;

        loop {
            match self.peek_byte() {
                None | Some(b' ') | Some(b'\n') | Some(b'\t') | Some(b'\r') | Some(b')') => {
                    if scratch.is_empty() {
                        // Fast path: return a slice of the raw S-expression without any
                        // copying.
                        let borrowed = &self.slice[start..self.index];
                        return result(self, borrowed).map(Reference::Borrowed);
                    } else {
                        scratch.extend_from_slice(&self.slice[start..self.index]);
                        // "as &[u8]" is required for rustc 1.8.0
                        let copied = scratch as &[u8];
                        return result(self, copied).map(Reference::Copied);
                    }
                }
                _ => self.index += 1,
            }
        }
    }

    fn peek_byte(&self) -> Option<u8> {
        if self.index < self.slice.len() {
            Some(self.slice[self.index])
        } else {
            None
        }
    }

    /// The big optimization here over IoRead is that if the string contains no
    /// backslash escape sequences, the returned &str is a slice of the raw
    /// S-expression data so we avoid copying into the scratch space.
    fn parse_str_bytes<'s, T: ?Sized, F>(
        &'s mut self,
        scratch: &'s mut Vec<u8>,
        validate: bool,
        result: F,
    ) -> Result<Reference<'a, 's, T>>
    where
        T: 's,
        F: for<'f> FnOnce(&'s Self, &'f [u8]) -> Result<&'f T>,
    {
        // Index of the first byte not yet copied into the scratch space.
        let mut start = self.index;

        loop {
            while self.index < self.slice.len() && !ESCAPE[self.slice[self.index] as usize] {
                self.index += 1;
            }
            if self.index == self.slice.len() {
                return error(self, ErrorCode::EofWhileParsingString);
            }
            match self.slice[self.index] {
                b'"' => {
                    if scratch.is_empty() {
                        // Fast path: return a slice of the raw S-expression without any
                        // copying.
                        let borrowed = &self.slice[start..self.index];
                        self.index += 1;
                        return result(self, borrowed).map(Reference::Borrowed);
                    } else {
                        scratch.extend_from_slice(&self.slice[start..self.index]);
                        self.index += 1;
                        return result(self, scratch).map(Reference::Copied);
                    }
                }
                b'\\' => {
                    scratch.extend_from_slice(&self.slice[start..self.index]);
                    self.index += 1;
                    parse_r6rs_escape(self, scratch)?;
                    start = self.index;
                }
                _ => {
                    self.index += 1;
                    if validate {
                        return error(self, ErrorCode::ControlCharacterWhileParsingString);
                    }
                }
            }
        }
    }
}

impl<'a> private::Sealed for SliceRead<'a> {}

impl<'a> Read<'a> for SliceRead<'a> {
    #[inline]
    fn next(&mut self) -> Result<Option<u8>> {
        // `Ok(self.slice.get(self.index).map(|ch| { self.index += 1; *ch }))`
        // is about 10% slower.
        Ok(if self.index < self.slice.len() {
            let ch = self.slice[self.index];
            self.index += 1;
            Some(ch)
        } else {
            None
        })
    }

    #[inline]
    fn peek(&mut self) -> Result<Option<u8>> {
        // `Ok(self.slice.get(self.index).map(|ch| *ch))` is about 10% slower
        // for some reason.
        Ok(if self.index < self.slice.len() {
            Some(self.slice[self.index])
        } else {
            None
        })
    }

    #[inline]
    fn discard(&mut self) {
        self.index += 1;
    }

    fn position(&self) -> Position {
        self.position_of_index(self.index)
    }

    fn peek_position(&self) -> Position {
        // Cap it at slice.len() just in case the most recent call was next()
        // and it returned the last byte.
        self.position_of_index(cmp::min(self.slice.len(), self.index + 1))
    }

    fn byte_offset(&self) -> usize {
        self.index
    }

    fn parse_str<'s>(&'s mut self, scratch: &'s mut Vec<u8>) -> Result<Reference<'a, 's, str>> {
        self.parse_str_bytes(scratch, true, as_str)
    }

    fn parse_str_raw<'s>(
        &'s mut self,
        scratch: &'s mut Vec<u8>,
    ) -> Result<Reference<'a, 's, [u8]>> {
        self.parse_str_bytes(scratch, false, |_, bytes| Ok(bytes))
    }

    fn parse_symbol<'s>(&'s mut self, scratch: &'s mut Vec<u8>) -> Result<Reference<'a, 's, str>> {
        self.parse_symbol_bytes(scratch, as_str)
    }

    fn decode_hex_escape(&mut self) -> Result<u32> {
        let mut n = 0;
        loop {
            let next = self.slice[self.index];
            if next == b';' {
                self.index += 1;
                break;
            }
            let ch = decode_hex_val(next);
            self.index += 1;
            match ch {
                None => return error(self, ErrorCode::InvalidEscape),
                Some(val) => {
                    if n >= (1 << 24) {
                        // A codepoint never has more than 24 bits
                        return error(self, ErrorCode::InvalidUnicodeCodePoint);
                    }
                    n = (n << 4) + u32::from(val);
                }
            }
        }
        Ok(n)
    }
}

//////////////////////////////////////////////////////////////////////////////

impl<'a> StrRead<'a> {
    /// Create a S-expression input source to read from a UTF-8 string.
    pub fn new(s: &'a str) -> Self {
        StrRead {
            delegate: SliceRead::new(s.as_bytes()),
        }
    }
}

impl<'a> private::Sealed for StrRead<'a> {}

impl<'a> Read<'a> for StrRead<'a> {
    #[inline]
    fn next(&mut self) -> Result<Option<u8>> {
        self.delegate.next()
    }

    #[inline]
    fn peek(&mut self) -> Result<Option<u8>> {
        self.delegate.peek()
    }

    #[inline]
    fn discard(&mut self) {
        self.delegate.discard();
    }

    fn position(&self) -> Position {
        self.delegate.position()
    }

    fn peek_position(&self) -> Position {
        self.delegate.peek_position()
    }

    fn byte_offset(&self) -> usize {
        self.delegate.byte_offset()
    }

    fn parse_str<'s>(&'s mut self, scratch: &'s mut Vec<u8>) -> Result<Reference<'a, 's, str>> {
        self.delegate.parse_str_bytes(scratch, true, |_, bytes| {
            // The input is assumed to be valid UTF-8 and the \u-escapes are
            // checked along the way, so don't need to check here.
            Ok(unsafe { str::from_utf8_unchecked(bytes) })
        })
    }

    fn parse_str_raw<'s>(
        &'s mut self,
        scratch: &'s mut Vec<u8>,
    ) -> Result<Reference<'a, 's, [u8]>> {
        self.delegate.parse_str_raw(scratch)
    }

    fn parse_symbol<'s>(&'s mut self, scratch: &'s mut Vec<u8>) -> Result<Reference<'a, 's, str>> {
        self.delegate.parse_symbol_bytes(scratch, |_, bytes| {
            // The input is assumed to be valid UTF-8 and the \u-escapes are
            // checked along the way, so don't need to check here.
            Ok(unsafe { str::from_utf8_unchecked(bytes) })
        })
    }

    fn decode_hex_escape(&mut self) -> Result<u32> {
        self.delegate.decode_hex_escape()
    }
}

//////////////////////////////////////////////////////////////////////////////

// Lookup table of bytes that must be escaped. A value of true at index i means
// that byte i requires an escape sequence in the input.
static ESCAPE: [bool; 256] = {
    const CT: bool = true; // control character \x00...\x1F
    const QU: bool = true; // quote \x22
    const BS: bool = true; // backslash \x5C
    const __: bool = false; // allow unescaped
    [
        //   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
        CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, // 0
        CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, CT, // 1
        __, __, QU, __, __, __, __, __, __, __, __, __, __, __, __, __, // 2
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 3
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 4
        __, __, __, __, __, __, __, __, __, __, __, __, BS, __, __, __, // 5
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 6
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 7
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 8
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 9
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // A
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // B
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // C
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // D
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // E
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // F
    ]
};

fn next_or_eof<'de, R: ?Sized + Read<'de>>(read: &mut R) -> Result<u8> {
    match read.next()? {
        Some(b) => Ok(b),
        None => error(read, ErrorCode::EofWhileParsingString),
    }
}

fn error<'de, R: ?Sized + Read<'de>, T>(read: &R, reason: ErrorCode) -> Result<T> {
    let position = read.position();
    Err(Error::syntax(reason, position.line, position.column))
}

fn as_str<'de, 's, R: Read<'de>>(read: &R, slice: &'s [u8]) -> Result<&'s str> {
    str::from_utf8(slice).or_else(|_| error(read, ErrorCode::InvalidUnicodeCodePoint))
}

/// Parses an R6RS escape sequence and appends it into the scratch
/// space. Assumes the previous byte read was a backslash.
fn parse_r6rs_escape<'de, R: Read<'de>>(read: &mut R, scratch: &mut Vec<u8>) -> Result<()> {
    let ch = next_or_eof(read)?;

    match ch {
        b'"' => scratch.push(b'"'),
        b'\\' => scratch.push(b'\\'),
        b'/' => scratch.push(b'/'),
        b'a' => scratch.push(0x07),
        b'b' => scratch.push(b'\x08'),
        b'f' => scratch.push(b'\x0c'),
        b'n' => scratch.push(b'\n'),
        b'r' => scratch.push(b'\r'),
        b't' => scratch.push(b'\t'),
        b'v' => scratch.push(0x0B),
        // TODO: trailing backspace (i.e., a continuation line)
        b'x' => {
            let c = {
                let n = read.decode_hex_escape()?;
                match char::from_u32(n) {
                    Some(c) => c,
                    None => {
                        return error(read, ErrorCode::InvalidUnicodeCodePoint);
                    }
                }
            };
            scratch.extend_from_slice(c.encode_utf8(&mut [0_u8; 4]).as_bytes());
        }
        _ => {
            return error(read, ErrorCode::InvalidEscape);
        }
    }

    Ok(())
}

#[allow(clippy::zero_prefixed_literal)]
static HEX: [u8; 256] = {
    const __: u8 = 255; // not a hex digit
    [
        //   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 0
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 1
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 2
        00, 01, 02, 03, 04, 05, 06, 07, 08, 09, __, __, __, __, __, __, // 3
        __, 10, 11, 12, 13, 14, 15, __, __, __, __, __, __, __, __, __, // 4
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 5
        __, 10, 11, 12, 13, 14, 15, __, __, __, __, __, __, __, __, __, // 6
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 7
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 8
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 9
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // A
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // B
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // C
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // D
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // E
        __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // F
    ]
};

fn decode_hex_val(val: u8) -> Option<u8> {
    let n = HEX[val as usize];
    if n == 255 {
        None
    } else {
        Some(n)
    }
}
