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
    fn parse_r6rs_str<'s>(
        &'s mut self,
        scratch: &'s mut Vec<u8>,
    ) -> Result<Reference<'de, 's, str>>;

    /// Assumes the previous byte was a quotation mark. Parses a string with
    /// Emacs Lisp escapes until the next quotation mark using the given scratch
    /// space if necessary. The scratch space is initially empty.
    #[doc(hidden)]
    fn parse_elisp_str<'s>(
        &'s mut self,
        scratch: &'s mut Vec<u8>,
    ) -> Result<ElispStrReference<'de, 's>>;

    /// Parses an unescaped string until the next whitespace or non-symbol character.
    #[doc(hidden)]
    fn parse_symbol<'s>(&'s mut self, scratch: &'s mut Vec<u8>) -> Result<Reference<'de, 's, str>>;

    /// Parses a R6RS character constant.
    #[doc(hidden)]
    fn parse_r6rs_char(&mut self, scratch: &mut Vec<u8>) -> Result<char> {
        // TODO: This could maybe benefit, performance-wise, from a seperate
        // implementation on `SliceRead`.
        parse_r6rs_char(self, scratch)
    }

    /// Parses an Emacs Lisp character constant.
    fn parse_elisp_char(&mut self, scratch: &mut Vec<u8>) -> Result<char> {
        // TODO: This could maybe benefit, performance-wise, from a seperate
        // implementation on `SliceRead`.
        parse_elisp_char(self, scratch)
    }
}

/// A location in the parsed text.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Position {
    line: usize,
    column: usize,
}

impl Position {
    pub(crate) fn new(line: usize, column: usize) -> Self {
        Position { line, column }
    }

    /// Returns the 1-based line number.
    pub fn line(&self) -> usize {
        self.line
    }
    /// Returns the column.
    ///
    /// Column numbers are 0-based byte offsets from the last line terminator
    /// (`\n`).
    pub fn column(&self) -> usize {
        self.column
    }
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

#[derive(Debug)]
pub enum ElispStr<U, M> {
    Unibyte(U),
    Multibyte(M),
}

pub type ElispStrReference<'b, 'c> = ElispStr<Reference<'b, 'c, [u8]>, Reference<'b, 'c, str>>;

#[derive(Debug)]
enum ElispEscape {
    Unibyte,
    Multibyte,
    Indeterminate,
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
    fn parse_r6rs_str_bytes<'s, T, F>(
        &'s mut self,
        scratch: &'s mut Vec<u8>,
        result: F,
    ) -> Result<T>
    where
        T: 's,
        F: FnOnce(&'s Self, &'s [u8]) -> Result<T>,
    {
        loop {
            let ch = next_or_eof(self)?;
            match ch {
                b'"' => {
                    return result(self, scratch);
                }
                b'\\' => {
                    parse_r6rs_escape(self, scratch)?;
                }
                _ => {
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
        loop {
            match self.peek()? {
                Some(b' ') | Some(b'\n') | Some(b'\t') | Some(b'\r') | Some(b')') | Some(b']')
                | Some(b'(') | Some(b'[') | Some(b';') | None => {
                    if scratch == b"." {
                        return error(self, ErrorCode::InvalidSymbol);
                    }
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

    fn parse_r6rs_str<'s>(
        &'s mut self,
        scratch: &'s mut Vec<u8>,
    ) -> Result<Reference<'de, 's, str>> {
        self.parse_r6rs_str_bytes(scratch, as_str)
            .map(Reference::Copied)
    }

    fn parse_elisp_str<'s>(
        &'s mut self,
        scratch: &'s mut Vec<u8>,
    ) -> Result<ElispStrReference<'de, 's>> {
        let mut seen_ub_escape = false;
        let mut seen_mb_escape = false;
        let mut seen_non_ascii = false;
        loop {
            let ch = next_or_eof(self)?;
            match ch {
                b'"' => {
                    let s = if seen_ub_escape && !(seen_mb_escape || seen_non_ascii) {
                        ElispStr::Unibyte(Reference::Copied(scratch.as_slice()))
                    } else {
                        ElispStr::Multibyte(Reference::Copied(as_str(self, scratch)?))
                    };
                    return Ok(s);
                }
                b'\\' => match parse_elisp_escape(self, scratch)? {
                    ElispEscape::Unibyte => seen_ub_escape = true,
                    ElispEscape::Multibyte => seen_mb_escape = true,
                    ElispEscape::Indeterminate => {}
                },
                _ => {
                    if ch > 127 {
                        seen_non_ascii = true;
                    }
                    scratch.push(ch);
                }
            }
        }
    }

    fn parse_symbol<'s>(&'s mut self, scratch: &'s mut Vec<u8>) -> Result<Reference<'de, 's, str>> {
        self.parse_symbol_bytes(scratch, as_str)
            .map(Reference::Copied)
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
                None | Some(b' ') | Some(b'\n') | Some(b'\t') | Some(b'\r') | Some(b')')
                | Some(b']') | Some(b'(') | Some(b'[') | Some(b';') => {
                    if scratch.is_empty() {
                        // Fast path: return a slice of the raw S-expression without any
                        // copying.
                        let borrowed = &self.slice[start..self.index];
                        if borrowed == b"." {
                            return error(self, ErrorCode::InvalidSymbol);
                        }
                        return result(self, borrowed).map(Reference::Borrowed);
                    } else {
                        scratch.extend_from_slice(&self.slice[start..self.index]);
                        if scratch == b"." {
                            return error(self, ErrorCode::InvalidSymbol);
                        }
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

    fn parse_elisp_str_bytes<'s, F>(
        &'s mut self,
        scratch: &'s mut Vec<u8>,
        result: F,
    ) -> Result<ElispStrReference<'a, 's>>
    where
        F: for<'f> FnOnce(&'s Self, &'f [u8]) -> Result<&'f str>,
    {
        // Index of the first byte not yet copied into the scratch space.
        let mut start = self.index;
        let mut seen_ub_escape = false;
        let mut seen_mb_escape = false;
        let mut seen_non_ascii = false;

        loop {
            while self.index < self.slice.len() {
                let ch = self.slice[self.index];
                if ch > 127 {
                    seen_non_ascii = true;
                }
                if needs_escape(ch) {
                    break;
                }
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
                        if seen_ub_escape && !(seen_mb_escape || seen_non_ascii) {
                            return Ok(ElispStr::Unibyte(Reference::Borrowed(borrowed)));
                        } else {
                            return result(self, borrowed)
                                .map(|s| ElispStr::Multibyte(Reference::Borrowed(s)));
                        }
                    } else {
                        scratch.extend_from_slice(&self.slice[start..self.index]);
                        self.index += 1;
                        if seen_ub_escape && !(seen_mb_escape || seen_non_ascii) {
                            return Ok(ElispStr::Unibyte(Reference::Copied(scratch)));
                        } else {
                            return result(self, scratch)
                                .map(|s| ElispStr::Multibyte(Reference::Copied(s)));
                        }
                    }
                }
                b'\\' => {
                    scratch.extend_from_slice(&self.slice[start..self.index]);
                    self.index += 1;
                    match parse_elisp_escape(self, scratch)? {
                        ElispEscape::Multibyte => seen_mb_escape = true,
                        ElispEscape::Unibyte => seen_ub_escape = true,
                        ElispEscape::Indeterminate => {}
                    }
                    start = self.index;
                }
                _ => unreachable!(),
            }
        }
    }

    /// The big optimization here over IoRead is that if the string contains no
    /// backslash escape sequences, the returned &str is a slice of the raw
    /// S-expression data so we avoid copying into the scratch space.
    fn parse_r6rs_str_bytes<'s, T: ?Sized, F>(
        &'s mut self,
        scratch: &'s mut Vec<u8>,
        result: F,
    ) -> Result<Reference<'a, 's, T>>
    where
        T: 's,
        F: for<'f> FnOnce(&'s Self, &'f [u8]) -> Result<&'f T>,
    {
        // Index of the first byte not yet copied into the scratch space.
        let mut start = self.index;

        loop {
            while self.index < self.slice.len() && !needs_escape(self.slice[self.index]) {
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
                _ => unreachable!(),
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

    fn parse_r6rs_str<'s>(
        &'s mut self,
        scratch: &'s mut Vec<u8>,
    ) -> Result<Reference<'a, 's, str>> {
        self.parse_r6rs_str_bytes(scratch, as_str)
    }

    fn parse_elisp_str<'s>(
        &'s mut self,
        scratch: &'s mut Vec<u8>,
    ) -> Result<ElispStrReference<'a, 's>> {
        self.parse_elisp_str_bytes(scratch, as_str)
    }

    fn parse_symbol<'s>(&'s mut self, scratch: &'s mut Vec<u8>) -> Result<Reference<'a, 's, str>> {
        self.parse_symbol_bytes(scratch, as_str)
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

    fn parse_r6rs_str<'s>(
        &'s mut self,
        scratch: &'s mut Vec<u8>,
    ) -> Result<Reference<'a, 's, str>> {
        self.delegate.parse_r6rs_str_bytes(scratch, |_, bytes| {
            // The input is assumed to be valid UTF-8 and the \x-escapes are
            // checked along the way, so don't need to check here.
            Ok(unsafe { str::from_utf8_unchecked(bytes) })
        })
    }

    fn parse_elisp_str<'s>(
        &'s mut self,
        scratch: &'s mut Vec<u8>,
    ) -> Result<ElispStrReference<'a, 's>> {
        // We cannot apply the same optimization as in `parse_r6rs_str`, because
        // Emacs Lisp allows embedding arbitrary bytes using hex and octal
        // escapes.
        self.delegate.parse_elisp_str_bytes(scratch, as_str)
    }

    fn parse_symbol<'s>(&'s mut self, scratch: &'s mut Vec<u8>) -> Result<Reference<'a, 's, str>> {
        self.delegate.parse_symbol_bytes(scratch, |_, bytes| {
            // The input is assumed to be valid UTF-8 and the \u-escapes are
            // checked along the way, so don't need to check here.
            Ok(unsafe { str::from_utf8_unchecked(bytes) })
        })
    }
}

fn next_or_eof<'de, R: ?Sized + Read<'de>>(read: &mut R) -> Result<u8> {
    match read.next()? {
        Some(b) => Ok(b),
        None => error(read, ErrorCode::EofWhileParsingString),
    }
}

fn next_or_eof_char<'de, R: ?Sized + Read<'de>>(read: &mut R) -> Result<u8> {
    match read.next()? {
        Some(b) => Ok(b),
        None => error(read, ErrorCode::EofWhileParsingCharacterConstant),
    }
}

fn error<'de, R: ?Sized + Read<'de>, T>(read: &R, reason: ErrorCode) -> Result<T> {
    let position = read.position();
    Err(Error::syntax(reason, position.line, position.column))
}

fn as_str<'de, 's, R: Read<'de>>(read: &R, slice: &'s [u8]) -> Result<&'s str> {
    str::from_utf8(slice).or_else(|_| error(read, ErrorCode::InvalidUnicodeCodePoint))
}

fn as_char<'de, 's, R: Read<'de> + ?Sized>(read: &R, value: u32) -> Result<char> {
    match char::from_u32(value) {
        None => error(read, ErrorCode::InvalidUnicodeCodePoint),
        Some(c) => Ok(c),
    }
}

fn needs_escape(c: u8) -> bool {
    c == b'\\' || c == b'"'
}

/// Assumes the previous byte was a hex escape sequnce ('\x') in a string.
/// Parses the hexadecimal sequence, including the terminating semicolon.
fn decode_r6rs_hex_escape<'de, R: Read<'de>>(read: &mut R) -> Result<u32> {
    let mut n = 0;
    loop {
        let next = next_or_eof(read)?;
        if next == b';' {
            return Ok(n);
        }
        match decode_hex_val(next) {
            None => return error(read, ErrorCode::EofWhileParsingString),
            Some(val) => {
                if n >= (1 << 24) {
                    // A codepoint never has more than 24 bits
                    return error(read, ErrorCode::InvalidUnicodeCodePoint);
                }
                n = (n << 4) + u32::from(val);
            }
        }
    }
}

/// Parses an R6RS escape sequence and appends it into the scratch
/// space. Assumes the previous byte read was a backslash.
fn parse_r6rs_escape<'de, R: Read<'de>>(read: &mut R, scratch: &mut Vec<u8>) -> Result<()> {
    let ch = next_or_eof(read)?;

    match ch {
        b'"' => scratch.push(b'"'),
        b'\\' => scratch.push(b'\\'),
        b'a' => scratch.push(0x07),
        b'b' => scratch.push(0x08),
        b'f' => scratch.push(0x0C),
        b'n' => scratch.push(b'\n'),
        b'r' => scratch.push(b'\r'),
        b't' => scratch.push(b'\t'),
        b'v' => scratch.push(0x0B),
        b'|' => scratch.push(b'|'),
        // TODO: trailing backspace (i.e., a continuation line)
        b'x' => {
            let c = {
                let n = decode_r6rs_hex_escape(read)?;
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

/// Assumes the previous byte was a hex escape sequence ('\x') in a string.
/// Parses the hexadecimal sequence, not including the terminating character
/// (i.e, first non hex-digit).
fn decode_elisp_hex_escape<'de, R: Read<'de> + ?Sized>(read: &mut R) -> Result<u32> {
    let mut n = 0;
    while let Some(c) = read.peek()? {
        match decode_hex_val(c) {
            None => break,
            Some(val) => {
                read.discard();
                if n >= (1 << 24) {
                    // A codepoint never has more than 24 bits
                    return error(read, ErrorCode::InvalidUnicodeCodePoint);
                }
                n = (n << 4) + u32::from(val);
            }
        }
    }
    Ok(n)
}

fn decode_elisp_uni_escape<'de, R: Read<'de> + ?Sized>(read: &mut R, count: u8) -> Result<u32> {
    let mut n = 0;
    for _ in 0..count {
        let c = next_or_eof(read)?;
        let val = match decode_hex_val(c) {
            None => return error(read, ErrorCode::InvalidEscape),
            Some(val) => val,
        };
        if n >= (1 << 24) {
            // A codepoint never has more than 24 bits
            return error(read, ErrorCode::InvalidUnicodeCodePoint);
        }
        n = (n << 4) + u32::from(val);
    }
    Ok(n)
}

/// Assumes the previous byte was a backslash, followed by a character is in the
/// octal range ('0'..'7'). This character is passed in as `initial`.  Parses
/// the octal sequence, not including the terminating character (i.e, first non
/// octal digit).
fn decode_elisp_octal_escape<'de, R: Read<'de> + ?Sized>(read: &mut R, initial: u8) -> Result<u32> {
    let mut n = u32::from(initial - b'0');
    while let Some(c) = read.peek()? {
        match decode_octal_val(c) {
            None => break,
            Some(val) => {
                read.discard();
                if n >= (1 << 24) {
                    // A codepoint never has more than 24 bits
                    return error(read, ErrorCode::InvalidUnicodeCodePoint);
                }
                n = (n << 3) + u32::from(val);
            }
        }
    }
    Ok(n)
}

fn parse_elisp_char_escape<'de, R, F>(
    read: &mut R,
    scratch: &mut Vec<u8>,
    decode: F,
) -> Result<ElispEscape>
where
    R: Read<'de>,
    F: FnOnce(&mut R) -> Result<u32>,
{
    let n = decode(read)?;
    match char::from_u32(n) {
        Some(c) => {
            if n > 255 {
                scratch.extend_from_slice(c.encode_utf8(&mut [0_u8; 4]).as_bytes());
                Ok(ElispEscape::Multibyte)
            } else {
                scratch.push(n as u8);
                Ok(ElispEscape::Unibyte)
            }
        }
        None => error(read, ErrorCode::InvalidUnicodeCodePoint),
    }
}

fn parse_elisp_uni_char_escape<'de, R, F>(
    read: &mut R,
    scratch: &mut Vec<u8>,
    decode: F,
) -> Result<ElispEscape>
where
    R: Read<'de>,
    F: FnOnce(&mut R) -> Result<u32>,
{
    let n = decode(read)?;
    match char::from_u32(n) {
        Some(c) => {
            scratch.extend_from_slice(c.encode_utf8(&mut [0_u8; 4]).as_bytes());
            Ok(ElispEscape::Multibyte)
        }
        None => error(read, ErrorCode::InvalidUnicodeCodePoint),
    }
}

/// Parses an Emacs Lisp escape sequence and appends it into the scratch
/// space. Assumes the previous byte read was a backslash.
///
/// Note that we explictly do not support meta-character syntax in
/// strings. Including meta-escaped characters in strings is recommended against
/// in the Emacs Lisp manual, Section 21.7.15 "Putting Keyboard Events in
/// Strings". The same goes for control characters written in the
/// "keyboard-oriented" syntax (see Section, 2.3.3.3 "Control-Character
/// Syntax").
fn parse_elisp_escape<'de, R: Read<'de>>(
    read: &mut R,
    scratch: &mut Vec<u8>,
) -> Result<ElispEscape> {
    let ch = next_or_eof(read)?;

    match ch {
        b'"' => scratch.push(b'"'),
        b'\\' => scratch.push(b'\\'),
        b' ' => {} // Escaped blank is ignored
        b'a' => scratch.push(0x07),
        b'b' => scratch.push(0x08),
        b't' => scratch.push(b'\t'),
        b'n' => scratch.push(b'\n'),
        b'v' => scratch.push(0x0B),
        b'f' => scratch.push(0x0C),
        b'r' => scratch.push(b'\r'),
        b'e' => scratch.push(0x1B),
        b's' => scratch.push(b' '),
        b'd' => scratch.push(0x7F),
        b'^' => {
            // Control character syntax
            let ch = next_or_eof(read)?.to_ascii_lowercase();
            if ch.is_ascii_lowercase() {
                scratch.push(ch - b'a');
            } else {
                return error(read, ErrorCode::InvalidEscape);
            }
        }
        b'N' => {
            // Name based unicode escape, `\N{NAME}` or `\N{U+X}`. These imply a
            // multibyte string.
            if next_or_eof(read)? != b'{' {
                return error(read, ErrorCode::InvalidEscape);
            }
            if next_or_eof(read)? != b'U' || next_or_eof(read)? != b'+' {
                return error(read, ErrorCode::InvalidEscape);
            }
            let escape = parse_elisp_uni_char_escape(read, scratch, decode_elisp_hex_escape)?;
            if next_or_eof(read)? != b'}' {
                return error(read, ErrorCode::InvalidEscape);
            }
            return Ok(escape);
        }
        b'u' => {
            // Codepoint unicode escape, `\uXXXX` (exactly four hex digits). These imply a multibyte
            // string.
            return parse_elisp_uni_char_escape(read, scratch, |read| {
                decode_elisp_uni_escape(read, 4)
            });
        }
        b'U' => {
            // Codepoint unicode escape, `\UXXXXXXXX` (exactly eight hex
            // digits). These imply a multibyte string.
            return parse_elisp_uni_char_escape(read, scratch, |read| {
                decode_elisp_uni_escape(read, 8)
            });
        }
        b'x' => {
            // Hexadecimal escape, allows arbitrary number of hex digits. If in
            // byte range, these imply a unibyte string, if the other conditions
            // are met.
            return parse_elisp_char_escape(read, scratch, decode_elisp_hex_escape);
        }
        b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' => {
            // Octal escape, allows arbitrary number of octale digits. If in
            // byte range, these imply a unibyte string, if the other conditions
            // are met.
            return parse_elisp_char_escape(read, scratch, |read| {
                decode_elisp_octal_escape(read, ch)
            });
        }
        _ => scratch.push(ch),
    }
    Ok(ElispEscape::Indeterminate)
}

/// Expects either `<any character>, `<character name>` or `x<hex scalar
/// value>`. Clears the scratch space if necessary.
fn parse_r6rs_char<'de, R: Read<'de> + ?Sized>(
    read: &mut R,
    scratch: &mut Vec<u8>,
) -> Result<char> {
    let initial = next_or_eof_char(read)?;
    if initial == b'x' {
        match decode_r6rs_char_hex_escape(read)? {
            Some(n) => match char::from_u32(n) {
                Some(c) => Ok(c),
                None => error(read, ErrorCode::InvalidUnicodeCodePoint),
            },
            None => Ok('x'),
        }
    } else if initial > 0x7F {
        decode_utf8_sequence(read, scratch, initial)
    } else {
        // ASCII character, may be standalone or part of a `<character name>`.
        let next = match read.peek()? {
            Some(next) => {
                if is_delimiter(next) {
                    return Ok(char::from(initial));
                } else {
                    next
                }
            }
            None => return Ok(char::from(initial)),
        };
        // Accumulate `<character name>` in scratch space
        scratch.clear();
        scratch.push(initial);
        scratch.push(next);
        read.discard();
        while let Some(next) = read.peek()? {
            if is_delimiter(next) {
                break;
            }
            scratch.push(next);
            read.discard();
        }
        match scratch.as_slice() {
            b"nul" => Ok('\x00'),
            b"alarm" => Ok('\x07'),
            b"backspace" => Ok('\x08'),
            b"tab" => Ok('\t'),
            b"linefeed" => Ok('\n'),
            b"newline" => Ok('\n'),
            b"vtab" => Ok('\x0B'),
            b"page" => Ok('\x0C'),
            b"return" => Ok('\r'),
            b"esc" => Ok('\x1B'),
            b"space" => Ok(' '),
            b"delete" => Ok('\x7F'),
            _ => error(read, ErrorCode::InvalidCharacterConstant),
        }
    }
}

/// Expects a `#\x` sequence has just been consumed; returns the value of the
/// subsequent hex digits, or `None`, if the sequence was empty.
fn decode_r6rs_char_hex_escape<'de, R: Read<'de> + ?Sized>(read: &mut R) -> Result<Option<u32>> {
    let mut n = 0;
    let mut first = true;
    loop {
        let next = match read.peek()? {
            Some(c) => {
                if is_delimiter(c) {
                    return Ok(if first { None } else { Some(n) });
                } else {
                    c
                }
            }
            None => return Ok(if first { None } else { Some(n) }),
        };
        read.discard();
        first = false;
        match decode_hex_val(next) {
            None => return error(read, ErrorCode::EofWhileParsingCharacterConstant),
            Some(val) => {
                if n >= (1 << 24) {
                    // A codepoint never has more than 24 bits
                    return error(read, ErrorCode::InvalidUnicodeCodePoint);
                }
                n = (n << 4) + u32::from(val);
            }
        }
    }
}

/// Expects either `\<memnonic escape>`, `\<normal escape>`, `\x<hex scalar
/// value>` or `<non-delimiter>` where `<non delimiter>` is any character but
/// `()[]\;"`. Clears the scratch space if necessary.
fn parse_elisp_char<'de, R: Read<'de> + ?Sized>(
    read: &mut R,
    scratch: &mut Vec<u8>,
) -> Result<char> {
    let initial = match read.next()? {
        None => return error(read, ErrorCode::EofWhileParsingCharacterConstant),
        Some(c) => c,
    };
    if initial > 0x7F {
        decode_utf8_sequence(read, scratch, initial)
    } else {
        // ASCII character
        match initial {
            b'(' | b')' | b'[' | b']' | b';' => error(read, ErrorCode::InvalidCharacterConstant),
            b'\\' => decode_elisp_char_escape(read, scratch),
            _ => Ok(char::from(initial)),
        }
    }
}

fn decode_elisp_char_escape<'de, R: Read<'de> + ?Sized>(
    read: &mut R,
    scratch: &mut Vec<u8>,
) -> Result<char> {
    let ch = next_or_eof_char(read)?;
    match ch {
        b'a' => Ok('\x07'),
        b'b' => Ok('\x08'),
        b't' => Ok('\t'),
        b'n' => Ok('\n'),
        b'v' => Ok('\x0B'),
        b'f' => Ok('\x0C'),
        b'r' => Ok('\r'),
        b'e' => Ok('\x1B'),
        b's' => Ok(' '),
        b'\\' => Ok('\\'),
        b'd' => Ok('\x7F'),
        b'^' => {
            // Control character syntax
            let ch = next_or_eof_char(read)?.to_ascii_lowercase();
            if ch.is_ascii_lowercase() {
                Ok(char::from(ch - b'a'))
            } else {
                error(read, ErrorCode::InvalidEscape)
            }
        }
        b'N' => {
            // Name based unicode escape, `\N{NAME}` or `\N{U+X}`. These imply a
            // multibyte string.
            if next_or_eof_char(read)? != b'{' {
                return error(read, ErrorCode::InvalidEscape);
            }
            if next_or_eof_char(read)? != b'U' || next_or_eof_char(read)? != b'+' {
                return error(read, ErrorCode::InvalidEscape);
            }
            let n = decode_elisp_hex_escape(read)?;
            if next_or_eof(read)? != b'}' {
                return error(read, ErrorCode::InvalidEscape);
            }
            match char::from_u32(n) {
                Some(c) => Ok(c),
                None => error(read, ErrorCode::InvalidEscape),
            }
        }
        b'u' => {
            // Codepoint unicode escape, `\uXXXX` (exactly four hex digits).
            decode_elisp_uni_escape(read, 4).and_then(|n| as_char(read, n))
        }
        b'U' => {
            // Codepoint unicode escape, `\UXXXXXXXX` (exactly eight hex
            // digits).
            decode_elisp_uni_escape(read, 8).and_then(|n| as_char(read, n))
        }
        b'x' => {
            // Hexadecimal escape, allows arbitrary number of hex digits.
            decode_elisp_hex_escape(read).and_then(|n| as_char(read, n))
        }
        b'0' | b'1' | b'2' | b'3' | b'4' | b'5' | b'6' | b'7' => {
            // Octal escape, allows arbitrary number of octale digits.
            decode_elisp_octal_escape(read, ch).and_then(|n| as_char(read, n))
        }
        next => {
            if next > 0x7F {
                decode_utf8_sequence(read, scratch, next)
            } else {
                Ok(char::from(next))
            }
        }
    }
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

fn decode_octal_val(val: u8) -> Option<u8> {
    if (b'0'..=b'7').contains(&val) {
        Some(val - b'0')
    } else {
        None
    }
}

fn is_delimiter(c: u8) -> bool {
    DELIMITER.contains(&c)
}

// This could probably profit from being a `u8 -> bool` LUT instead.
static DELIMITER: [u8; 12] = [
    b'(', b')', b'[', b']', b'"', b';', b'#', b' ', b'\n', b'\t', b'\r', 0x0C,
];

/// Decode a UTF8 multibyte sequence starting with `initial` and return the
/// decoded codepoint.
fn decode_utf8_sequence<'de, R: Read<'de> + ?Sized>(
    read: &mut R,
    scratch: &mut Vec<u8>,
    initial: u8,
) -> Result<char> {
    // Start of a UTF-8 sequence. It seems there is no simple way to
    // consume a single UTF-8 codepoint from a source of bytes, so
    // we implement just enough of an UTF-8 decoder to consume the
    // correct number of bytes, and then use the standard library to
    // turn these into a codepoint. If one would like to optimize
    // this, doing the complete decoding here could eliminate the
    // use of `scratch` and the calls into the standard library.
    let len = match initial {
        0b1100_0000..=0b1101_1111 => 1,
        0b1110_0000..=0b1111_0111 => (initial - 0b1100_0000) >> 4,
        _ => return error(read, ErrorCode::InvalidUnicodeCodePoint),
    };
    scratch.clear();
    scratch.push(initial);
    for _ in 0..len {
        let b = match read.next()? {
            Some(c) => c,
            None => return error(read, ErrorCode::InvalidUnicodeCodePoint),
        };
        scratch.push(b);
    }
    match str::from_utf8(scratch) {
        Err(_) => error(read, ErrorCode::InvalidUnicodeCodePoint),
        Ok(s) => Ok(s.chars().next().unwrap()),
    }
}
