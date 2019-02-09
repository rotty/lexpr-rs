use std::io;

use crate::style::KeywordStyle;
use crate::{Atom, Number, Value};

#[derive(Clone, Debug)]
pub struct Options {
    keyword_style: KeywordStyle,
    nil_style: NilStyle,
}

#[derive(Debug, Clone, Copy)]
pub enum NilStyle {
    Symbol,
    Token,
    EmptyList,
}

/// Represents a character escape code in a type-safe manner.
pub enum CharEscape {
    /// An escaped quote `"`
    Quote,
    /// An escaped reverse solidus `\`
    ReverseSolidus,
    /// An escaped solidus `/`
    Solidus,
    /// An escaped backspace character (usually escaped as `\b`)
    Backspace,
    /// An escaped form feed character (usually escaped as `\f`)
    FormFeed,
    /// An escaped line feed character (usually escaped as `\n`)
    LineFeed,
    /// An escaped carriage return character (usually escaped as `\r`)
    CarriageReturn,
    /// An escaped tab character (usually escaped as `\t`)
    Tab,
    /// An escaped ASCII plane control character (usually escaped as
    /// `\u00XX` where `XX` are two hex characters)
    AsciiControl(u8),
}

impl CharEscape {
    #[inline]
    fn from_escape_table(escape: u8, byte: u8) -> CharEscape {
        match escape {
            self::BB => CharEscape::Backspace,
            self::TT => CharEscape::Tab,
            self::NN => CharEscape::LineFeed,
            self::FF => CharEscape::FormFeed,
            self::RR => CharEscape::CarriageReturn,
            self::QU => CharEscape::Quote,
            self::BS => CharEscape::ReverseSolidus,
            self::UU => CharEscape::AsciiControl(byte),
            _ => unreachable!(),
        }
    }
}

/// This trait abstracts away serializing the S-expression pieces,
/// which allows the implementer to optionally pretty print the
/// S-expression output, as well as to allow customizing the printing
/// for various S-expression "dialects".
///
/// The default implementation produces Scheme-style S-expression
/// text.
pub trait Formatter {
    /// Writes a `nil` value to the specified writer.
    #[inline]
    fn write_nil<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        writer.write_all(b"#nil")
    }

    /// Writes a `true` or `false` value to the specified writer.
    #[inline]
    fn write_bool<W: ?Sized>(&mut self, writer: &mut W, value: bool) -> io::Result<()>
    where
        W: io::Write,
    {
        writer.write_all(if value { b"#t" } else { b"#f" })
    }

    /// Writes an integer value like `-123` to the specified writer.
    #[inline]
    fn write_number<W: ?Sized>(&mut self, writer: &mut W, value: &Number) -> io::Result<()>
    where
        W: io::Write,
    {
        value.write(writer)
    }

    /// Called before each series of `write_string_fragment` and
    /// `write_char_escape`.  Writes a `"` to the specified writer.
    #[inline]
    fn begin_string<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        writer.write_all(b"\"")
    }

    /// Called after each series of `write_string_fragment` and
    /// `write_char_escape`.  Writes a `"` to the specified writer.
    #[inline]
    fn end_string<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        writer.write_all(b"\"")
    }

    /// Writes a string fragment that doesn't need any escaping to the
    /// specified writer.
    #[inline]
    fn write_string_fragment<W: ?Sized>(&mut self, writer: &mut W, fragment: &str) -> io::Result<()>
    where
        W: io::Write,
    {
        writer.write_all(fragment.as_bytes())
    }

    /// Writes a character escape code to the specified writer.
    #[inline]
    fn write_char_escape<W: ?Sized>(
        &mut self,
        writer: &mut W,
        char_escape: CharEscape,
    ) -> io::Result<()>
    where
        W: io::Write,
    {
        use self::CharEscape::*;

        let s = match char_escape {
            Quote => b"\\\"",
            ReverseSolidus => b"\\\\",
            Solidus => b"\\/",
            Backspace => b"\\b",
            FormFeed => b"\\f",
            LineFeed => b"\\n",
            CarriageReturn => b"\\r",
            Tab => b"\\t",
            AsciiControl(byte) => {
                static HEX_DIGITS: [u8; 16] = *b"0123456789abcdef";
                let bytes = &[
                    b'\\',
                    b'u',
                    b'0',
                    b'0',
                    HEX_DIGITS[(byte >> 4) as usize],
                    HEX_DIGITS[(byte & 0xF) as usize],
                ];
                return writer.write_all(bytes);
            }
        };

        writer.write_all(s)
    }

    #[inline]
    fn write_symbol<W: ?Sized>(&mut self, writer: &mut W, name: &str) -> io::Result<()>
    where
        W: io::Write,
    {
        // TODO: We might need to escape and/or use pipe notation.
        writer.write_all(name.as_bytes())
    }

    #[inline]
    fn write_keyword<W: ?Sized>(&mut self, writer: &mut W, name: &str) -> io::Result<()>
    where
        W: io::Write,
    {
        writer.write_all(b"#:")?;
        writer.write_all(name.as_bytes())?;
        Ok(())
    }

    /// Called before any list elements.  Writes a `(` to the specified
    /// writer.
    #[inline]
    fn begin_list<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        writer.write_all(b"(")
    }

    #[inline]
    fn begin_list_element<W: ?Sized>(&mut self, writer: &mut W, first: bool) -> io::Result<()>
    where
        W: io::Write,
    {
        if first {
            Ok(())
        } else {
            writer.write_all(b" ")
        }
    }

    #[inline]
    fn end_list_element<W: ?Sized>(&mut self, _writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        Ok(())
    }

    /// Called after all list elements.  Writes a `)` to the specified
    /// writer.
    #[inline]
    fn end_list<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        writer.write_all(b")")
    }

    /// Called before writing the tail of an improper list, or more
    /// generally, the `cdr` field of a cons cell.  Writes a `.` to
    /// the specified writer.
    #[inline]
    fn write_dot<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        writer.write_all(b".")
    }
}

/// This structure compacts a S-expression value on a single line,
/// using the default representation, which is similar to Scheme.
#[derive(Clone, Debug)]
pub struct DefaultFormatter;

impl Formatter for DefaultFormatter {}

#[derive(Clone, Debug)]
pub struct CustomizedFormatter {
    options: Options,
}

#[derive(Debug)]
pub struct Printer<W, F = DefaultFormatter> {
    writer: W,
    formatter: F,
}

impl<W, F> Printer<W, F>
where
    W: io::Write,
    F: Formatter,
{
    /// Creates a new S-expression printer whose output will be
    /// written to the writer specified.
    #[inline]
    pub fn with_formatter(writer: W, formatter: F) -> Self {
        Printer { writer, formatter }
    }

    /// Unwrap the `Writer` from the `Serializer`.
    #[inline]
    pub fn into_inner(self) -> W {
        self.writer
    }

    fn print_atom(&mut self, atom: &Atom) -> io::Result<()> {
        match atom {
            Atom::Nil => self.formatter.write_nil(&mut self.writer),
            Atom::Bool(b) => self.formatter.write_bool(&mut self.writer, *b),
            Atom::Number(n) => self.formatter.write_number(&mut self.writer, &n),
            Atom::Symbol(name) => self.formatter.write_symbol(&mut self.writer, &name),
            Atom::Keyword(name) => self.formatter.write_keyword(&mut self.writer, &name),
            Atom::String(s) => format_escaped_str(&mut self.writer, &mut self.formatter, &s),
        }
    }
    pub fn print(&mut self, value: &Value) -> io::Result<()> {
        match value {
            Value::Atom(atom) => self.print_atom(atom)?,
            Value::List(elements) => {
                self.formatter.begin_list(&mut self.writer)?;
                for (i, element) in elements.iter().enumerate() {
                    self.formatter
                        .begin_list_element(&mut self.writer, i == 0)?;
                    self.print(element)?;
                    self.formatter.end_list_element(&mut self.writer)?;
                }
                self.formatter.end_list(&mut self.writer)?;
            }
            Value::ImproperList(elements, tail) => {
                if elements.len() != 0 {
                    self.formatter.begin_list(&mut self.writer)?;
                    for (i, element) in elements.iter().enumerate() {
                        self.formatter
                            .begin_list_element(&mut self.writer, i == 0)?;
                        self.print(element)?;
                        self.formatter.end_list_element(&mut self.writer)?;
                    }
                    self.formatter.begin_list_element(&mut self.writer, false)?;
                    self.formatter.write_dot(&mut self.writer)?;
                    self.formatter.end_list_element(&mut self.writer)?;
                    self.formatter.begin_list_element(&mut self.writer, false)?;
                    self.print_atom(tail)?;
                    self.formatter.end_list_element(&mut self.writer)?;
                    self.formatter.end_list(&mut self.writer)?;
                } else {
                    self.print_atom(tail)?;
                }
            }
        }
        Ok(())
    }
}

impl<W> Printer<W>
where
    W: io::Write,
{
    /// Creates a new S-expression printer.
    #[inline]
    pub fn new(writer: W) -> Self {
        Printer::with_formatter(writer, DefaultFormatter)
    }
}

fn format_escaped_str<W: ?Sized, F: ?Sized>(
    writer: &mut W,
    formatter: &mut F,
    value: &str,
) -> io::Result<()>
where
    W: io::Write,
    F: Formatter,
{
    formatter.begin_string(writer)?;
    format_escaped_str_contents(writer, formatter, value)?;
    formatter.end_string(writer)?;
    Ok(())
}

fn format_escaped_str_contents<W: ?Sized, F: ?Sized>(
    writer: &mut W,
    formatter: &mut F,
    value: &str,
) -> io::Result<()>
where
    W: io::Write,
    F: Formatter,
{
    let bytes = value.as_bytes();

    let mut start = 0;

    for (i, &byte) in bytes.iter().enumerate() {
        let escape = ESCAPE[byte as usize];
        if escape == 0 {
            continue;
        }

        if start < i {
            formatter.write_string_fragment(writer, &value[start..i])?;
        }

        let char_escape = CharEscape::from_escape_table(escape, byte);
        formatter.write_char_escape(writer, char_escape)?;

        start = i + 1;
    }

    if start != bytes.len() {
        formatter.write_string_fragment(writer, &value[start..])?;
    }

    Ok(())
}

const BB: u8 = b'b'; // \x08
const TT: u8 = b't'; // \x09
const NN: u8 = b'n'; // \x0A
const FF: u8 = b'f'; // \x0C
const RR: u8 = b'r'; // \x0D
const QU: u8 = b'"'; // \x22
const BS: u8 = b'\\'; // \x5C
const UU: u8 = b'u'; // \x00...\x1F except the ones above
const __: u8 = 0;

// Lookup table of escape sequences. A value of b'x' at index i means that byte
// i is escaped as "\x" in JSON. A value of 0 means that byte i is not escaped.
static ESCAPE: [u8; 256] = [
    //   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
    UU, UU, UU, UU, UU, UU, UU, UU, BB, TT, NN, UU, FF, RR, UU, UU, // 0
    UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, // 1
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
];

/// Serialize the given data structure as S-expression into the IO stream.
///
/// # Errors
///
/// Serialization can fail if `T`'s implementation of `Serialize` decides to
/// fail, or if `T` contains a map with non-string keys.
#[inline]
pub fn to_writer<W: io::Write>(writer: W, value: &Value) -> io::Result<()> {
    let mut printer = Printer::new(writer);
    printer.print(value)?;
    Ok(())
}

/// Serialize the given data structure as a S-expression byte vector.
///
/// # Errors
///
/// Serialization can fail if `T`'s implementation of `Serialize` decides to
/// fail, or if `T` contains a map with non-string keys.
#[inline]
pub fn to_vec(value: &Value) -> io::Result<Vec<u8>> {
    let mut writer = Vec::with_capacity(128);
    to_writer(&mut writer, value)?;
    Ok(writer)
}

/// Serialize the given data structure as a String of S-expression.
///
/// # Errors
///
/// Serialization can fail if `T`'s implementation of `Serialize` decides to
/// fail, or if `T` contains a map with non-string keys.
#[inline]
pub fn to_string(value: &Value) -> io::Result<String> {
    let vec = to_vec(value)?;
    let string = unsafe {
        // We do not emit invalid UTF-8.
        String::from_utf8_unchecked(vec)
    };
    Ok(string)
}
