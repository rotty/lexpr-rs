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
pub use crate::style::{CharSyntax, KeywordStyle, StringSyntax};
use crate::Value;

/// Options for printing S-expressions.
#[derive(Clone, Debug)]
pub struct Options {
    keyword_style: KeywordStyle,
    nil_style: NilStyle,
    bool_style: BoolStyle,
    vector_style: VectorStyle,
    bytes_style: BytesStyle,
    string_syntax: StringSyntax,
    char_syntax: CharSyntax,
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
            vector_style: VectorStyle::Brackets,
            bytes_style: BytesStyle::Elisp,
            string_syntax: StringSyntax::Elisp,
            char_syntax: CharSyntax::Elisp,
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

    /// Set the style to use to print boolean values.
    pub fn with_bool_style(mut self, style: BoolStyle) -> Self {
        self.bool_style = style;
        self
    }

    /// Set the style for printing vectors.
    pub fn with_vector_style(mut self, style: VectorStyle) -> Self {
        self.vector_style = style;
        self
    }

    /// Set the style to use for printing byte vectors.
    pub fn with_bytes_style(mut self, style: BytesStyle) -> Self {
        self.bytes_style = style;
        self
    }

    /// Set the syntax used for printing strings.
    pub fn with_string_syntax(mut self, syntax: StringSyntax) -> Self {
        self.string_syntax = syntax;
        self
    }

    /// Set the syntax used for printing characters.
    pub fn with_char_syntax(mut self, syntax: CharSyntax) -> Self {
        self.char_syntax = syntax;
        self
    }
}

impl Default for Options {
    fn default() -> Self {
        Options {
            keyword_style: KeywordStyle::Octothorpe,
            nil_style: NilStyle::Token,
            bool_style: BoolStyle::Token,
            vector_style: VectorStyle::Octothorpe,
            bytes_style: BytesStyle::R7RS,
            string_syntax: StringSyntax::R6RS,
            char_syntax: CharSyntax::R6RS,
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

/// How to print vectors.
#[derive(Debug, Clone, Copy)]
pub enum VectorStyle {
    /// Use Scheme notation, i.e. `#(...)`.
    Octothorpe,
    /// Use brackets, as used in Emacs Lisp.
    Brackets,
}

/// How to print byte vectors.
#[derive(Debug, Clone, Copy)]
pub enum BytesStyle {
    /// Use R6RS byte vector syntax, e.g. `#vu8(1 2 3)`.
    R6RS,
    /// Use R7RS byte vector syntax, e.g. `#u8(1 2 3)`.
    R7RS,
    /// Use Emacs Lisp unibyte string syntax, e.g. `"\001\002\003"`.
    Elisp,
}

/// Represents a character escape code in a type-safe manner.
pub enum CharEscape {
    /// An escaped quote `"`
    Quote,
    /// An escaped reverse solidus `\`
    ReverseSolidus,
    /// Alert, also known as "bell" (usually escaped as `\a`)
    Alert,
    /// An escaped backspace character (usually escaped as `\b`)
    Backspace,
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
            self::AA => CharEscape::Alert,
            self::BB => CharEscape::Backspace,
            self::TT => CharEscape::Tab,
            self::NN => CharEscape::LineFeed,
            self::RR => CharEscape::CarriageReturn,
            self::QU => CharEscape::Quote,
            self::BS => CharEscape::ReverseSolidus,
            self::UU => CharEscape::AsciiControl(byte),
            _ => unreachable!(),
        }
    }
}

/// Different vector types
pub enum VectorType {
    /// Generic vector, containing elements of any type.
    Generic,
    /// Byte vector, containing only byte (octet) values.
    Byte,
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

    /// Writes a charactor to the specified writer.
    ///
    /// The implementation provided by the trait will use Scheme notation
    /// (`#\C`).
    fn write_char<W: ?Sized>(&mut self, writer: &mut W, c: char) -> io::Result<()>
    where
        W: io::Write,
    {
        write_scheme_char(writer, c)
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
        write_r6rs_char_escape(writer, char_escape)
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

    /// Writes a byte vector to the specified writer.
    #[inline]
    fn write_bytes<W: ?Sized>(&mut self, writer: &mut W, bytes: &[u8]) -> io::Result<()>
    where
        W: io::Write,
    {
        write_scheme_vector(self, writer, VectorType::Byte, bytes, |writer, &octet| {
            itoa::write(writer, octet).map(|_| ())
        })
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

    /// Called after all list elements have been written.  Writes a `)` to the
    /// specified writer.
    #[inline]
    fn end_list<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        writer.write_all(b")")
    }

    /// Called before starting to write a list or vector element. Writes a space
    /// to the specified writer, if needed.
    #[inline]
    fn begin_seq_element<W: ?Sized>(&mut self, writer: &mut W, first: bool) -> io::Result<()>
    where
        W: io::Write,
    {
        if first {
            Ok(())
        } else {
            writer.write_all(b" ")
        }
    }

    /// Called after every list or vector element.
    #[inline]
    fn end_seq_element<W: ?Sized>(&mut self, _writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        Ok(())
    }

    /// Called before any vector elements.  Will write `#(` for generic vectors,
    /// or `#u8(` for byte vectors, to the specified writer.
    #[inline]
    fn begin_vector<W: ?Sized>(&mut self, kind: VectorType, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        match kind {
            VectorType::Generic => writer.write_all(b"#("),
            VectorType::Byte => writer.write_all(b"#u8("),
        }
    }

    /// Called after all vector elements have been written.  Writes a `)` to the
    /// specified writer.
    #[inline]
    fn end_vector<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
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

    fn begin_vector<W: ?Sized>(&mut self, kind: VectorType, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        match self.options.vector_style {
            VectorStyle::Brackets => writer.write_all(b"["),
            VectorStyle::Octothorpe => match kind {
                VectorType::Generic => writer.write_all(b"#("),
                VectorType::Byte => match self.options.bytes_style {
                    BytesStyle::R6RS => writer.write_all(b"#vu8("),
                    BytesStyle::R7RS => writer.write_all(b"#u8("),
                    _ => panic!("invalid combination of VectorStyle and ByteStyle"),
                },
            },
        }
    }

    fn end_vector<W: ?Sized>(&mut self, writer: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        match self.options.vector_style {
            VectorStyle::Brackets => writer.write_all(b"]"),
            VectorStyle::Octothorpe => writer.write_all(b")"),
        }
    }

    fn write_char<W: ?Sized>(&mut self, writer: &mut W, c: char) -> io::Result<()>
    where
        W: io::Write,
    {
        match self.options.char_syntax {
            CharSyntax::R6RS => write_scheme_char(writer, c),
            CharSyntax::Elisp => write_elisp_char(writer, c),
        }
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
        match self.options.string_syntax {
            StringSyntax::R6RS => write_r6rs_char_escape(writer, char_escape),
            StringSyntax::Elisp => write_elisp_char_escape(writer, char_escape),
        }
    }

    fn write_bytes<W: ?Sized>(&mut self, writer: &mut W, bytes: &[u8]) -> io::Result<()>
    where
        W: io::Write,
    {
        match self.options.bytes_style {
            BytesStyle::R6RS | BytesStyle::R7RS => {
                write_scheme_vector(self, writer, VectorType::Byte, bytes, |writer, &octet| {
                    itoa::write(writer, octet).map(|_| ())
                })
            }
            BytesStyle::Elisp => {
                static OCTAL_CHARS: &[u8] = b"012345678";
                writer.write_all(b"\"")?;
                for octet in bytes {
                    writer.write_all(b"\\")?;
                    for &triplet in &[
                        (octet >> 6) & 0b111u8,
                        (octet >> 3) & 0b111u8,
                        octet & 0b111u8,
                    ] {
                        let index = triplet as usize;
                        writer.write_all(&OCTAL_CHARS[index..=index])?;
                    }
                }
                writer.write_all(b"\"")
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
            Value::Char(c) => self.formatter.write_char(&mut self.writer, *c),
            Value::Symbol(name) => self.formatter.write_symbol(&mut self.writer, &name),
            Value::Keyword(name) => self.formatter.write_keyword(&mut self.writer, &name),
            Value::String(s) => format_escaped_str(&mut self.writer, &mut self.formatter, &s),
            Value::Bytes(bytes) => self.formatter.write_bytes(&mut self.writer, bytes),
            Value::Cons(elements) => {
                self.formatter.begin_list(&mut self.writer)?;
                for (i, pair) in elements.iter().enumerate() {
                    self.formatter.begin_seq_element(&mut self.writer, i == 0)?;
                    self.print(pair.car())?;
                    self.formatter.end_seq_element(&mut self.writer)?;
                    match pair.cdr() {
                        Value::Null | Value::Cons(_) => {}
                        _ => {
                            self.formatter.begin_seq_element(&mut self.writer, false)?;
                            self.formatter.write_dot(&mut self.writer)?;
                            self.formatter.end_seq_element(&mut self.writer)?;
                            self.formatter.begin_seq_element(&mut self.writer, false)?;
                            self.print(pair.cdr())?;
                            self.formatter.end_seq_element(&mut self.writer)?;
                        }
                    }
                }
                self.formatter.end_list(&mut self.writer)
            }
            Value::Vector(elements) => {
                self.write_vector(VectorType::Generic, elements.iter(), |printer, element| {
                    printer.print(element)
                })
            }
        }
    }

    fn write_vector<I, O>(&mut self, kind: VectorType, elements: I, mut output: O) -> io::Result<()>
    where
        I: IntoIterator,
        O: FnMut(&mut Self, I::Item) -> io::Result<()>,
    {
        self.formatter.begin_vector(kind, &mut self.writer)?;
        for (i, element) in elements.into_iter().enumerate() {
            self.formatter.begin_seq_element(&mut self.writer, i == 0)?;
            output(self, element)?;
            self.formatter.end_seq_element(&mut self.writer)?;
        }
        self.formatter.end_vector(&mut self.writer)
    }
}

fn write_scheme_vector<F: ?Sized, W: ?Sized, I, O>(
    fmt: &mut F,
    writer: &mut W,
    kind: VectorType,
    elements: I,
    mut output: O,
) -> io::Result<()>
where
    F: Formatter,
    W: io::Write,
    I: IntoIterator,
    O: FnMut(&mut W, I::Item) -> io::Result<()>,
{
    fmt.begin_vector(kind, writer)?;
    for (i, element) in elements.into_iter().enumerate() {
        fmt.begin_seq_element(writer, i == 0)?;
        output(writer, element)?;
        fmt.end_seq_element(writer)?;
    }
    fmt.end_vector(writer)
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

fn write_r6rs_char_escape<W: ?Sized>(writer: &mut W, char_escape: CharEscape) -> io::Result<()>
where
    W: io::Write,
{
    use self::CharEscape::*;

    let s = match char_escape {
        Quote => b"\\\"",
        ReverseSolidus => b"\\\\",
        Alert => b"\\a",
        Backspace => b"\\b",
        LineFeed => b"\\n",
        CarriageReturn => b"\\r",
        Tab => b"\\t",
        AsciiControl(byte) => {
            static HEX_DIGITS: [u8; 16] = *b"0123456789ABCDEF";
            let bytes = &[
                b'\\',
                b'x',
                HEX_DIGITS[(byte >> 4) as usize],
                HEX_DIGITS[(byte & 0xF) as usize],
                b';',
            ];
            return writer.write_all(bytes);
        }
    };

    writer.write_all(s)
}

fn write_elisp_char_escape<W: ?Sized>(writer: &mut W, char_escape: CharEscape) -> io::Result<()>
where
    W: io::Write,
{
    use self::CharEscape::*;

    let s = match char_escape {
        Quote => b"\\\"",
        ReverseSolidus => b"\\\\",
        Alert => b"\\a",
        Backspace => b"\\b",
        LineFeed => b"\\n",
        CarriageReturn => b"\\r",
        Tab => b"\\t",
        AsciiControl(byte) => {
            // Note we use the `\uNNNN` syntax here, as a hexadecimal or octal
            // escape might turn the string into a unibyte string.
            static HEX_DIGITS: [u8; 16] = *b"0123456789ABCDEF";
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

fn write_scheme_char<W: ?Sized>(writer: &mut W, c: char) -> io::Result<()>
where
    W: io::Write,
{
    let n = u32::from(c);
    if n >= 32 && n < 127 {
        // ASCII, excluding non-printable characters
        let buf = [b'#', b'\\', n as u8];
        writer.write_all(&buf)
    } else {
        // TODO: we should probably output UTF-8 here, if reasonable, to be
        // consistent with the behaviour inside strings.
        write!(writer, "#\\x{:x}", n)
    }
}

fn write_elisp_char<W: ?Sized>(writer: &mut W, c: char) -> io::Result<()>
where
    W: io::Write,
{
    let n = u32::from(c);
    if n >= 32 && n < 127 {
        let c = n as u8;
        // ASCII, excluding non-printable characters
        if ELISP_ESCAPE_CHARS.contains(&c) {
            writer.write_all(&[b'?', b'\\', c])
        } else {
            writer.write_all(&[b'?', c])
        }
    } else {
        // TODO: we should probably output UTF-8 here, if reasonable, to be
        // consistent with the behaviour inside strings.
        write!(writer, "#\\x{:x}", n)
    }
}

const AA: u8 = b'a'; // \x07
const BB: u8 = b'b'; // \x08
const TT: u8 = b't'; // \x09
const NN: u8 = b'n'; // \x0A
const RR: u8 = b'r'; // \x0D
const QU: u8 = b'"'; // \x22
const BS: u8 = b'\\'; // \x5C
const UU: u8 = b'u'; // \x00...\x1F except the ones above
const __: u8 = 0;

// Lookup table of escape sequences. A value of b'x' at index i means that byte
// i is escaped as "\x" in JSON. A value of 0 means that byte i is not escaped.
static ESCAPE: [u8; 256] = [
    //   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
    UU, UU, UU, UU, UU, UU, UU, AA, BB, TT, NN, UU, UU, RR, UU, UU, // 0
    UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, UU, // 1
    __, __, QU, __, __, __, __, __, __, __, __, __, __, __, __, __, // 2
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 3
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 4
    __, __, __, __, __, __, __, __, __, __, __, __, BS, __, __, __, // 5
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 6
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, UU, // 7
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 8
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // 9
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // A
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // B
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // C
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // D
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // E
    __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, // F
];

static ELISP_ESCAPE_CHARS: &[u8] = b"()[]\\;|'`#.,";

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
