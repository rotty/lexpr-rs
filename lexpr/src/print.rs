//! Converting S-expression values into text.
//!
//! # Terminology
//!
//! The process of serializing S-expressions to their textual
//! representation is referred to "writing" in Lisp. To avoid
//! confusion with Rust's `Write` trait, `lexpr` uses "printing"
//! instead.

use std::io;

use crate::number::{self, Number};
pub use crate::style::KeywordStyle;
use crate::Value;

/// Options for printing S-expressions.
#[derive(Clone, Debug)]
pub struct Options {
    keyword_style: KeywordStyle,
    nil_style: NilStyle,
    bool_style: BoolStyle,
}

impl Options {
    /// Construct parser options suitable for printing Emacs Lisp.
    ///
    /// Keywords will use prefix notation (i.e. `:some-keyword`), the
    /// special nil value will be represented as a symbol, and
    /// booleans will be represented by `nil` and `t`.
    pub fn elisp() -> Self {
        Options {
            keyword_style: KeywordStyle::ColonPrefix,
            nil_style: NilStyle::Symbol,
            bool_style: BoolStyle::Symbol,
        }
    }

    /// Set the style to use for printing keywords.
    pub fn with_keyword_style(mut self, style: KeywordStyle) -> Self {
        self.keyword_style = style;
        self
    }

    /// Set the style to use to print the special nil value.
    pub fn with_nil_style(mut self, style: NilStyle) -> Self {
        self.nil_style = style;
        self
    }

    /// Set the style to print boolean values.
    pub fn with_bool_style(mut self, style: BoolStyle) -> Self {
        self.bool_style = style;
        self
    }
}

impl Default for Options {
    fn default() -> Self {
        Options {
            keyword_style: KeywordStyle::Octothorpe,
            nil_style: NilStyle::Token,
            bool_style: BoolStyle::Token,
        }
    }
}

/// How to print the special nil value.
#[derive(Debug, Clone, Copy)]
pub enum NilStyle {
    /// Output a `nil` symbol.
    Symbol,
    /// Output the `#nil` token.
    Token,
    /// Output the empty list.
    EmptyList,
    /// Output a boolean false value.
    False,
}

/// How to print boolean values.
#[derive(Debug, Clone, Copy)]
pub enum BoolStyle {
    /// Use the Scheme tokens `#t` and `#f`
    Token,
    /// Use symbols `nil` and `t`.
    Symbol,
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
    /// Writes a representation of the special nil value to the specified writer.
    #[inline]
    fn write_nil<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        writer.write_all(b"#nil")
    }

    /// Writes a representation of the special nil value to the specified writer.
    #[inline]
    fn write_null<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        writer.write_all(b"()")
    }

    /// Writes a representation of a boolean value to the specified writer.
    ///
    /// The implementation provided by the trait will use the Scheme notation
    /// (`#t` and `#f`).
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
        struct Write<'a, W: io::Write + ?Sized> {
            writer: &'a mut W,
        }
        impl<'a, W: io::Write + ?Sized> number::Visitor for Write<'a, W> {
            type Value = ();
            type Error = io::Error;

            fn error<T: Into<String>>(msg: T) -> io::Error {
                io::Error::new(io::ErrorKind::Other, msg.into())
            }
            fn visit_u64(self, n: u64) -> io::Result<()> {
                itoa::write(self.writer, n).map(drop)
            }
            fn visit_i64(self, n: i64) -> io::Result<()> {
                itoa::write(self.writer, n).map(drop)
            }
            fn visit_f64(self, n: f64) -> io::Result<()> {
                let mut buffer = ryu::Buffer::new();
                let s = buffer.format(n);
                self.writer.write_all(s.as_bytes())
            }
        }
        value.visit(Write { writer })
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

    /// Writes a string fragment that doesn't need any escaping to the specified
    /// writer.
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

    /// Writes a symbol to the specified writer.
    #[inline]
    fn write_symbol<W: ?Sized>(&mut self, writer: &mut W, name: &str) -> io::Result<()>
    where
        W: io::Write,
    {
        // TODO: We might need to escape and/or use pipe notation.
        writer.write_all(name.as_bytes())
    }

    /// Writes a keyword to the specified writer.
    #[inline]
    fn write_keyword<W: ?Sized>(&mut self, writer: &mut W, name: &str) -> io::Result<()>
    where
        W: io::Write,
    {
        writer.write_all(b"#:")?;
        writer.write_all(name.as_bytes())
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

    /// Called before starting to write a list element. Writes a space if needed
    /// to the specified writer.
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

    /// Called after every list element.
    #[inline]
    fn end_list_element<W: ?Sized>(&mut self, _writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        Ok(())
    }

    /// Called after all list elements have been written.  Writes a `)` to the
    /// specified writer.
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

/// A formatter which can be tuned with regards to S-expressions representation.
#[derive(Clone, Debug)]
pub struct CustomizedFormatter {
    options: Options,
}

impl Formatter for CustomizedFormatter {
    fn write_nil<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        match self.options.nil_style {
            NilStyle::EmptyList => writer.write_all(b"()"),
            NilStyle::Symbol => writer.write_all(b"nil"),
            NilStyle::Token => writer.write_all(b"#nil"),
            NilStyle::False => self.write_bool(writer, false),
        }
    }

    fn write_bool<W: ?Sized>(&mut self, writer: &mut W, value: bool) -> io::Result<()>
    where
        W: io::Write,
    {
        match self.options.bool_style {
            BoolStyle::Symbol => writer.write_all(if value { b"t" } else { b"nil" }),
            BoolStyle::Token => writer.write_all(if value { b"#t" } else { b"#f" }),
        }
    }

    fn write_keyword<W: ?Sized>(&mut self, writer: &mut W, name: &str) -> io::Result<()>
    where
        W: io::Write,
    {
        match self.options.keyword_style {
            KeywordStyle::ColonPostfix => {
                writer.write_all(name.as_bytes())?;
                writer.write_all(b":")
            }
            KeywordStyle::ColonPrefix => {
                writer.write_all(b":")?;
                writer.write_all(name.as_bytes())
            }
            KeywordStyle::Octothorpe => {
                writer.write_all(b"#:")?;
                writer.write_all(name.as_bytes())
            }
        }
    }
}

/// A printer for S-expression values.
#[derive(Debug)]
pub struct Printer<W, F = DefaultFormatter> {
    writer: W,
    formatter: F,
}

impl<W> Printer<W, CustomizedFormatter>
where
    W: io::Write,
{
    /// Construct an S-expression printer tuned given the specified options.
    pub fn with_options(writer: W, options: Options) -> Self {
        Printer {
            writer,
            formatter: CustomizedFormatter { options },
        }
    }
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

    /// Unwrap the `Writer` from the `Printer`.
    #[inline]
    pub fn into_inner(self) -> W {
        self.writer
    }

    /// Output the representation of the specified value to the underlying
    /// writer.
    pub fn print(&mut self, value: &Value) -> io::Result<()> {
        match value {
            Value::Nil => self.formatter.write_nil(&mut self.writer),
            Value::Null => self.formatter.write_null(&mut self.writer),
            Value::Bool(b) => self.formatter.write_bool(&mut self.writer, *b),
            Value::Number(n) => self.formatter.write_number(&mut self.writer, &n),
            Value::Symbol(name) => self.formatter.write_symbol(&mut self.writer, &name),
            Value::Keyword(name) => self.formatter.write_keyword(&mut self.writer, &name),
            Value::String(s) => format_escaped_str(&mut self.writer, &mut self.formatter, &s),
            Value::Cons(elements) => {
                self.formatter.begin_list(&mut self.writer)?;
                for (i, pair) in elements.iter().enumerate() {
                    self.formatter
                        .begin_list_element(&mut self.writer, i == 0)?;
                    self.print(pair.car())?;
                    self.formatter.end_list_element(&mut self.writer)?;
                    match pair.cdr() {
                        Value::Null | Value::Cons(_) => {}
                        _ => {
                            self.formatter.begin_list_element(&mut self.writer, false)?;
                            self.formatter.write_dot(&mut self.writer)?;
                            self.formatter.end_list_element(&mut self.writer)?;
                            self.formatter.begin_list_element(&mut self.writer, false)?;
                            self.print(pair.cdr())?;
                            self.formatter.end_list_element(&mut self.writer)?;
                        }
                    }
                }
                self.formatter.end_list(&mut self.writer)?;
                Ok(())
            }
        }
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

impl<W, F> io::Write for Printer<W, F>
where
    W: io::Write,
{
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.writer.write(buf)
    }

    #[inline]
    fn flush(&mut self) -> io::Result<()> {
        self.writer.flush()
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

/// Serialize the given value value as S-expression text into the IO stream,
/// using the default printer options.
#[inline]
pub fn to_writer<W: io::Write>(writer: W, value: &Value) -> io::Result<()> {
    let mut printer = Printer::new(writer);
    printer.print(value)?;
    Ok(())
}

/// Serialize the given value value as S-expression text into the IO stream.
#[inline]
pub fn to_writer_custom<W: io::Write>(
    writer: W,
    value: &Value,
    options: Options,
) -> io::Result<()> {
    let mut printer = Printer::with_options(writer, options);
    printer.print(value)?;
    Ok(())
}

/// Serialize the given value as byte vector containing S-expression text, using
/// the default printer options.
#[inline]
pub fn to_vec(value: &Value) -> io::Result<Vec<u8>> {
    let mut writer = Vec::with_capacity(128);
    to_writer(&mut writer, value)?;
    Ok(writer)
}

/// Serialize the given value as byte vector containing S-expression text.
#[inline]
pub fn to_vec_custom(value: &Value, options: Options) -> io::Result<Vec<u8>> {
    let mut writer = Vec::with_capacity(128);
    to_writer_custom(&mut writer, value, options)?;
    Ok(writer)
}

/// Serialize the given value an S-expression string,
/// using the default printer options.
#[inline]
pub fn to_string(value: &Value) -> io::Result<String> {
    let vec = to_vec(value)?;
    let string = unsafe {
        // We do not emit invalid UTF-8.
        String::from_utf8_unchecked(vec)
    };
    Ok(string)
}

/// Serialize the given value an S-expression string.
#[inline]
pub fn to_string_custom(value: &Value, options: Options) -> io::Result<String> {
    let vec = to_vec_custom(value, options)?;
    let string = unsafe {
        // We do not emit invalid UTF-8.
        String::from_utf8_unchecked(vec)
    };
    Ok(string)
}
