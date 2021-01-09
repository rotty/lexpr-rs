//! S-expression parser and options.
//!
//! # Terminology
//!
//! The process of converting S-expressions from their textual representation to
//! values is referred to "reading" in Lisp. To avoid confusion with Rust's
//! `Write` trait, `lexpr` uses "parsing" instead.

use std::borrow::Borrow;
use std::io;
use std::str;
use std::u64;

use error::ErrorCode;
use read::{ElispStr, Reference};

use crate::{datum::SpanInfo, Cons, Number, Value};

pub use crate::{
    datum::{Datum, Span},
    syntax::{CharSyntax, KeywordSyntax, StringSyntax},
};

pub use read::{IoRead, Position, Read, SliceRead, StrRead};

#[doc(inline)]
pub use error::{Error, Result};

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
    options: Options,
}

/// Various options to influence parser behavior.
#[derive(Debug, Copy, Clone)]
pub struct Options {
    keyword_syntaxes: u8,
    nil_symbol: NilSymbol,
    t_symbol: TSymbol,
    brackets: Brackets,
    string_syntax: StringSyntax,
    char_syntax: CharSyntax,
}

/// Defines the treatment of the symbol `nil`.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum NilSymbol {
    /// Parse `nil` like as the empty list. This the behavior of Emacs
    /// Lisp and Common Lisp.
    ///
    /// In the parsed `Value`, the empty list, written as `()` and the
    /// empty list, written as `nil` are both represented by the
    /// `Value::Null` variant.
    EmptyList,

    /// Parse `nil` as a regular symbol. This is the behavior found in
    /// Scheme.
    ///
    /// The parsed `Value` will be equal to `Value::symbol("nil")`.
    Default,

    /// Parse `nil` as a special value. This allows treating the `nil`
    /// symbol specially when processing the parsed data.
    ///
    /// The parsed `Value` will be equal to `Value::Nil`.
    Special,
}

/// Defines the treatment of the symbol `t`.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TSymbol {
    /// Parse `t` as the boolean true value.
    True,

    /// Parse `t` as a regular symbol.
    Default,
}

/// Defines the treatment of brackets.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Brackets {
    /// Brackets are synonymous with regular parentheses, and indicate a list,
    /// as in R6RS Scheme.
    List,

    /// Brackets indicate a vector, like in Emacs Lisp.
    Vector,
}

impl Options {
    /// Construct an empty set of options.
    ///
    /// This will recognize no keywords, and parse `nil` and `t` as
    /// regular symbols.
    pub fn new() -> Self {
        Options {
            keyword_syntaxes: 0,
            nil_symbol: NilSymbol::Default,
            t_symbol: TSymbol::Default,
            brackets: Brackets::List,
            string_syntax: StringSyntax::R6RS,
            char_syntax: CharSyntax::R6RS,
        }
    }

    /// Construct a set of options suitable for parsing Emacs Lisp.
    pub fn elisp() -> Self {
        Self::new()
            .with_keyword_syntax(KeywordSyntax::ColonPrefix)
            .with_nil_symbol(NilSymbol::EmptyList)
            .with_brackets(Brackets::Vector)
            .with_string_syntax(StringSyntax::Elisp)
            .with_char_syntax(CharSyntax::Elisp)
    }

    /// Add `syntax` to the recognized keyword syntaxes.
    pub fn with_keyword_syntax(mut self, syntax: KeywordSyntax) -> Self {
        self.keyword_syntaxes |= syntax.to_flag();
        self
    }

    /// Set the recognized keyword syntaxes.
    pub fn with_keyword_syntaxes<I, T>(mut self, styles: I) -> Self
    where
        I: IntoIterator<Item = T>,
        T: Borrow<KeywordSyntax>,
    {
        self.keyword_syntaxes = styles
            .into_iter()
            .fold(0, |flags, syntax| flags | syntax.borrow().to_flag());
        self
    }

    /// Choose how to parse the `nil` symbol.
    pub fn with_nil_symbol(mut self, treatment: NilSymbol) -> Self {
        self.nil_symbol = treatment;
        self
    }

    /// Choose how to parse the `t` symbol.
    pub fn with_t_symbol(mut self, treatment: TSymbol) -> Self {
        self.t_symbol = treatment;
        self
    }

    /// Choose how to handle brackets.
    pub fn with_brackets(mut self, treatment: Brackets) -> Self {
        self.brackets = treatment;
        self
    }

    /// Choose the accepted string syntax.
    pub fn with_string_syntax(mut self, syntax: StringSyntax) -> Self {
        self.string_syntax = syntax;
        self
    }

    /// Choose the accepted character syntax.
    pub fn with_char_syntax(mut self, syntax: CharSyntax) -> Self {
        self.char_syntax = syntax;
        self
    }

    /// Check wether a keyword syntax is enabled.
    #[inline]
    pub fn keyword_syntax(self, syntax: KeywordSyntax) -> bool {
        (self.keyword_syntaxes & syntax.to_flag()) != 0
    }

    /// Query the way the `nil` symbol is handled.
    pub fn nil_symbol(self) -> NilSymbol {
        self.nil_symbol
    }

    /// Query the way the `t` symbol is handled.
    pub fn t_symbol(self) -> TSymbol {
        self.t_symbol
    }

    /// Query the way brackets are handled.
    pub fn brackets(self) -> Brackets {
        self.brackets
    }

    /// Query the accepted string syntax.
    pub fn string_syntax(self) -> StringSyntax {
        self.string_syntax
    }

    /// Query the accepted character syntax.
    pub fn char_syntax(self) -> CharSyntax {
        self.char_syntax
    }
}

impl Default for Options {
    /// Construct a default set of options. This corresponds most
    /// closely to the Scheme dialect of S-expressions:
    ///
    /// - The identifiers `nil` and `t` are treated as regular symbols.
    /// - Only octothorpe keywords are recognized.
    /// - Brackets are treated just like parentheses, i.e. indicating a list.
    fn default() -> Self {
        Options {
            keyword_syntaxes: KeywordSyntax::Octothorpe.to_flag(),
            nil_symbol: NilSymbol::Default,
            t_symbol: TSymbol::Default,
            brackets: Brackets::List,
            string_syntax: StringSyntax::R6RS,
            char_syntax: CharSyntax::R6RS,
        }
    }
}

#[derive(Debug, Clone)]
enum Token {
    Null,
    Nil,
    Bool(bool),
    Char(char),
    Number(Number),
    Symbol(Box<str>),
    Keyword(Box<str>),
    String(Box<str>),
    Bytes(Box<[u8]>),
    ListOpen(u8),
    Quotation(&'static str),
    VecOpen(u8),
    ByteVecOpen(u8),
}

impl<'de, R> Parser<R>
where
    R: read::Read<'de>,
{
    /// Create an S-expression parser from one of the possible sexpr
    /// input sources.
    ///
    /// Typically it is more convenient to use one of these methods
    /// instead:
    ///
    ///   - `Parser::from_str`
    ///   - `Parser::from_slice`
    ///   - `Parser::from_reader`
    pub fn new(read: R) -> Self {
        Parser {
            read,
            scratch: Vec::with_capacity(128),
            remaining_depth: 128,
            options: Options::default(),
        }
    }

    /// Create a customized S-expression parser parser from one of the possible sexpr
    /// input sources.
    ///
    /// Typically it is more convenient to use one of these methods
    /// instead:
    ///
    ///   - `Parser::from_str_custom`
    ///   - `Parser::from_slice_custom`
    ///   - `Parser::from_reader_custom`
    pub fn with_options(read: R, options: Options) -> Self {
        Parser {
            read,
            scratch: Vec::with_capacity(128),
            remaining_depth: 128,
            options,
        }
    }
}

impl<R> Parser<read::IoRead<R>>
where
    R: io::Read,
{
    /// Creates an S-expression parser from an `io::Read`.
    pub fn from_reader(reader: R) -> Self {
        Parser::new(read::IoRead::new(reader))
    }

    /// Creates an S-expression parser from an `io::Read`.
    pub fn from_reader_custom(reader: R, options: Options) -> Self {
        Parser::with_options(read::IoRead::new(reader), options)
    }
}

impl<'a> Parser<read::SliceRead<'a>> {
    /// Creates an S-expression parser from a `&[u8]`.
    pub fn from_slice(bytes: &'a [u8]) -> Self {
        Parser::new(read::SliceRead::new(bytes))
    }

    /// Creates an S-expression parser from a `&[u8]`.
    pub fn from_slice_custom(bytes: &'a [u8], options: Options) -> Self {
        Parser::with_options(read::SliceRead::new(bytes), options)
    }
}

impl<'a> Parser<read::StrRead<'a>> {
    /// Creates a S-expression parser from a `&str`.
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(s: &'a str) -> Self {
        Parser::new(read::StrRead::new(s))
    }

    /// Creates a S-expression parser from a `&str`.
    #[allow(clippy::should_implement_trait)]
    pub fn from_str_custom(s: &'a str, options: Options) -> Self {
        Parser::with_options(read::StrRead::new(s), options)
    }
}

macro_rules! overflow {
    ($a:ident * $radix:literal + $b:ident, $c:expr) => {
        $a >= $c / $radix && ($a > $c / $radix || $b > $c % $radix)
    };
    ($a:ident * $radix:ident + $b:ident, $c:expr) => {
        $a >= $c / $radix && ($a > $c / $radix || $b > $c % $radix)
    };
}

impl<'de, R: Read<'de>> Parser<R> {
    /// Expect the end of input.
    ///
    /// The `Parser::expect_end` method should be called after the last
    /// S-expression has been consumed.  This allows the parser` to validate
    /// that the input stream is at the end or that it only has trailing
    /// whitespace.
    pub fn expect_end(&mut self) -> Result<()> {
        match self.parse_whitespace()? {
            Some(_) => Err(self.peek_error(ErrorCode::TrailingCharacters)),
            None => Ok(()),
        }
    }

    /// Expect the end of input.
    #[deprecated(since = "0.2.5", note = "Please use the `expect_end` method instead")]
    pub fn end(&mut self) -> Result<()> {
        self.expect_end()
    }

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
        Error::syntax(reason, pos.line(), pos.column())
    }

    /// Error caused by a byte from peek().
    fn peek_error(&mut self, reason: ErrorCode) -> Error {
        let pos = self.read.peek_position();
        Error::syntax(reason, pos.line(), pos.column())
    }

    /// Returns the first non-whitespace byte without consuming it, or `None` if
    /// EOF is encountered.
    fn parse_whitespace(&mut self) -> Result<Option<u8>> {
        loop {
            match self.peek()? {
                Some(b';') => loop {
                    match self.next_char()? {
                        Some(b'\n') => break,
                        Some(_) => {}
                        None => return Ok(None),
                    }
                },
                Some(b' ') | Some(b'\n') | Some(b'\t') | Some(b'\r') | Some(0x0C) => {
                    self.eat_char();
                }
                other => {
                    return Ok(other);
                }
            }
        }
    }

    /// Obtain an iterator over the values produced by the parser.
    ///
    /// ```
    /// # use lexpr::Parser;
    /// let mut parser = Parser::from_str(r#"foo ("bar" . 3.14) #:baz (1 2 3)"#);
    /// for value in parser.value_iter() {
    ///     println!("parsed value: {}", value.expect("parse error"));
    /// }
    /// ```
    pub fn value_iter(&mut self) -> ValueIter<'_, R> {
        ValueIter(self)
    }

    /// Obtain an iterator over the values produced by the parser, including location information.
    /// # use lexpr::Parser;
    /// let mut parser = Parser::from_str(r#"foo ("bar" . 3.14) #:baz (1 2 3)"#);
    /// for datum in parser.datum_iter() {
    ///     let datum = datum.expect("parse error");
    ///     let span = datum.span();
    ///     let start = span.start();
    ///     let end = span.end();
    ///     println!("parsed datum at {}:{}--{}:{}: {}", start.line(), start.column(),
    ///              end.start(), end.column(),
    ///              datum.value());
    /// }
    /// ```
    pub fn datum_iter(&mut self) -> DatumIter<'_, R> {
        DatumIter(self)
    }

    /// Parse a single S-expression from the input source.
    #[deprecated(since = "0.2.5", note = "Please use the `expect_value` method instead")]
    pub fn parse_value(&mut self) -> Result<Value> {
        self.expect_value()
    }

    /// Parse a single S-expression from the input source.
    ///
    /// This expects an S-expression value to be actually present, and
    /// returns an `Err` when called at the end of input. Use
    /// `Parser::value_iter()` if you need to handle end of input gracefully.
    ///
    /// ```
    /// # use lexpr::{sexp, parse, Parser};
    /// let mut parser = Parser::from_str(r#"foo ("bar" . 3.14) #:baz (1 2 3)"#);
    /// assert_eq!(parser.expect_value().unwrap(), sexp!(foo));
    /// assert_eq!(parser.expect_value().unwrap(), sexp!(("bar" . 3.14)));
    /// assert_eq!(parser.expect_value().unwrap(), sexp!(#:baz));
    /// assert_eq!(parser.expect_value().unwrap(), sexp!((1 2 3)));
    /// assert!(parser.expect_end().is_ok());
    /// ```
    pub fn expect_value(&mut self) -> Result<Value> {
        self.next_value()
            .and_then(|o| o.ok_or_else(|| self.peek_error(ErrorCode::EofWhileParsingValue)))
    }

    fn parse_token(&mut self, peek: u8) -> Result<Token> {
        let token = match peek {
            b'#' => {
                self.eat_char();
                match self.next_char()? {
                    Some(b't') => Token::Bool(true),
                    Some(b'f') => Token::Bool(false),
                    Some(b'n') => {
                        self.expect_ident(b"il")?;
                        Token::Nil
                    }
                    Some(b'(') => Token::VecOpen(b')'),
                    Some(b':') if self.options.keyword_syntax(KeywordSyntax::Octothorpe) => {
                        Token::Keyword(self.parse_symbol()?.into())
                    }
                    Some(b'v') => {
                        self.expect_ident(b"u8")?;
                        Token::ByteVecOpen(b')')
                    }
                    Some(b'u') => {
                        self.expect_ident(b"8")?;
                        Token::ByteVecOpen(b')')
                    }
                    Some(b'b') => Token::Number(self.parse_radix_literal(2)?),
                    Some(b'o') => Token::Number(self.parse_radix_literal(8)?),
                    Some(b'd') => Token::Number(self.parse_radix_literal(10)?),
                    Some(b'x') => Token::Number(self.parse_radix_literal(16)?),
                    Some(b'\\') => Token::Char(self.read.parse_r6rs_char(&mut self.scratch)?),
                    Some(_) => return Err(self.peek_error(ErrorCode::ExpectedSomeIdent)),
                    None => return Err(self.peek_error(ErrorCode::EofWhileParsingValue)),
                }
            }
            b'-' => {
                self.eat_char();
                let next = self.peek_or_null()?;
                if next == 0 || is_delimiter(next) || is_sign_subsequent(next) {
                    Token::Symbol(self.parse_symbol_suffix("-")?.into())
                } else {
                    Token::Number(self.parse_num_literal(10, false)?)
                }
            }
            b'+' => {
                self.eat_char();
                let next = self.peek_or_null()?;
                if next == 0 || is_delimiter(next) || is_sign_subsequent(next) {
                    Token::Symbol(self.parse_symbol_suffix("+")?.into())
                } else {
                    Token::Number(self.parse_num_literal(10, true)?)
                }
            }
            b'0'..=b'9' => Token::Number(self.parse_num_literal(10, true)?),
            b'"' => {
                self.eat_char();
                self.scratch.clear();
                match self.options.string_syntax {
                    StringSyntax::R6RS => match self.read.parse_r6rs_str(&mut self.scratch)? {
                        Reference::Borrowed(s) => Token::String(s.into()),
                        Reference::Copied(s) => Token::String(s.into()),
                    },
                    StringSyntax::Elisp => match self.read.parse_elisp_str(&mut self.scratch)? {
                        ElispStr::Multibyte(mb) => match mb {
                            Reference::Borrowed(s) => Token::String(s.into()),
                            Reference::Copied(s) => Token::String(s.into()),
                        },
                        ElispStr::Unibyte(ub) => match ub {
                            Reference::Borrowed(b) => Token::Bytes(b.into()),
                            Reference::Copied(b) => Token::Bytes(b.into()),
                        },
                    },
                }
            }
            b'(' => {
                self.eat_char();
                Token::ListOpen(b')')
            }
            b'[' => {
                self.eat_char();
                match self.options.brackets {
                    Brackets::Vector => Token::VecOpen(b']'),
                    Brackets::List => Token::ListOpen(b']'),
                }
            }
            b':' => {
                if self.options.keyword_syntax(KeywordSyntax::ColonPrefix) {
                    self.eat_char();
                    Token::Keyword(self.parse_symbol()?.into())
                } else {
                    Token::Symbol(self.parse_symbol()?.into())
                }
            }
            b'a'..=b'z' | b'A'..=b'Z' => {
                let mut name = self.parse_symbol()?;
                if self.options.keyword_syntax(KeywordSyntax::ColonPostfix) && name.ends_with(':') {
                    name.pop();
                    Token::Keyword(name.into())
                } else if self.options.nil_symbol() != NilSymbol::Default && name == "nil" {
                    match self.options.nil_symbol() {
                        NilSymbol::EmptyList => Token::Null,
                        NilSymbol::Special => Token::Nil,
                        NilSymbol::Default => unreachable!(),
                    }
                } else if self.options.t_symbol() != TSymbol::Default && name == "t" {
                    match self.options.t_symbol() {
                        TSymbol::True => Token::Bool(true),
                        TSymbol::Default => unreachable!(),
                    }
                } else {
                    Token::Symbol(name.into())
                }
            }
            b'?' if self.options.char_syntax == CharSyntax::Elisp => {
                self.eat_char();
                Token::Char(self.read.parse_elisp_char(&mut self.scratch)?)
            }
            b'\'' => {
                self.eat_char();
                Token::Quotation("quote")
            }
            b'`' => {
                self.eat_char();
                Token::Quotation("quasiquote")
            }
            b',' => {
                self.eat_char();
                match self.peek_or_null()? {
                    b'@' => {
                        self.eat_char();
                        Token::Quotation("unquote-splicing")
                    }
                    _ => Token::Quotation("unquote"),
                }
            }
            _ => {
                if SYMBOL_EXTENDED.contains(&peek) {
                    Token::Symbol(self.parse_symbol()?.into())
                } else {
                    return Err(self.peek_error(ErrorCode::ExpectedSomeValue));
                }
            }
        };
        Ok(token)
    }

    /// Parse an S-expression, returning `None` on end-of-input.
    ///
    /// For consuming the entire sequence of parsed S-expression values, the
    /// `value_iter` method may be more convenient than calling this method in a
    /// loop.
    pub fn next_value(&mut self) -> Result<Option<Value>> {
        let peek = match self.parse_whitespace()? {
            Some(b) => b,
            None => return Ok(None),
        };
        let value = match self.parse_token(peek)? {
            Token::Nil => Value::Nil,
            Token::Null => Value::Null,
            Token::Char(c) => Value::Char(c),
            Token::Bool(b) => Value::Bool(b),
            Token::Number(n) => Value::Number(n),
            Token::Symbol(s) => Value::Symbol(s),
            Token::Keyword(name) => Value::Keyword(name),
            Token::String(s) => Value::String(s),
            Token::Bytes(b) => Value::Bytes(b),
            Token::ByteVecOpen(close) => {
                Value::Bytes(self.parse_byte_list(close)?.into_boxed_slice())
            }
            Token::VecOpen(close) => {
                self.remaining_depth -= 1;
                if self.remaining_depth == 0 {
                    return Err(self.peek_error(ErrorCode::RecursionLimitExceeded));
                }

                let ret = self.parse_vector(close);

                self.remaining_depth += 1;

                match (ret, self.end_seq(close)) {
                    (Ok(elements), Ok(())) => Value::Vector(elements.into()),
                    (Err(err), _) | (_, Err(err)) => return Err(err),
                }
            }
            Token::ListOpen(close) => {
                self.remaining_depth -= 1;
                if self.remaining_depth == 0 {
                    return Err(self.peek_error(ErrorCode::RecursionLimitExceeded));
                }

                let ret = self.parse_list(close);

                self.remaining_depth += 1;

                match (ret, self.end_seq(close)) {
                    (Ok(list), Ok(())) => list,
                    (Err(err), _) | (_, Err(err)) => return Err(err),
                }
            }
            Token::Quotation(name) => {
                // TODO: more specific error
                let datum = self
                    .next_value()?
                    .ok_or_else(|| self.peek_error(ErrorCode::EofWhileParsingList))?;
                Value::list(vec![Value::symbol(name), datum])
            }
        };
        Ok(Some(value))
    }

    /// Parse a single S-expression from the input source.
    #[deprecated(since = "0.2.5", note = "Please use the  `next_value` method instead")]
    pub fn parse(&mut self) -> Result<Option<Value>> {
        self.next_value()
    }

    /// Parse a single S-expression including location information, returning an
    /// error on end-of-input.
    pub fn expect_datum(&mut self) -> Result<Datum> {
        self.next_datum()
            .and_then(|o| o.ok_or_else(|| self.peek_error(ErrorCode::EofWhileParsingValue)))
    }

    /// Parse a single S-expression including location information.
    ///
    /// When end of input is reached, `None` is returned.
    ///
    /// For consuming the entire sequence of parsed S-expression datums, the
    /// `datum_iter` method may be more convenient than calling this method in a
    /// loop.
    pub fn next_datum(&mut self) -> Result<Option<Datum>> {
        let peek = match self.parse_whitespace()? {
            Some(b) => b,
            None => return Ok(None),
        };
        let start = self.read.position();
        let primitive =
            |value, parser: &Self| Datum::primitive(value, start, parser.read.position());
        let syntax = match self.parse_token(peek)? {
            Token::Nil => primitive(Value::Nil, self),
            Token::Null => primitive(Value::Null, self),
            Token::Char(c) => primitive(Value::Char(c), self),
            Token::Bool(b) => primitive(Value::Bool(b), self),
            Token::Number(n) => primitive(Value::Number(n), self),
            Token::Symbol(s) => primitive(Value::Symbol(s), self),
            Token::Keyword(name) => primitive(Value::Keyword(name), self),
            Token::String(s) => primitive(Value::String(s), self),
            Token::Bytes(b) => primitive(Value::Bytes(b), self),
            Token::ByteVecOpen(close) => primitive(
                Value::Bytes(self.parse_byte_list(close)?.into_boxed_slice()),
                self,
            ),
            Token::VecOpen(close) => {
                self.remaining_depth -= 1;
                if self.remaining_depth == 0 {
                    return Err(self.peek_error(ErrorCode::RecursionLimitExceeded));
                }

                let ret = self.parse_vector_meta(close);

                self.remaining_depth += 1;

                match (ret, self.end_seq(close)) {
                    (Ok((elements, info)), Ok(())) => {
                        Datum::vec(elements, info, start, self.read.position())
                    }
                    (Err(err), _) | (_, Err(err)) => return Err(err),
                }
            }
            Token::ListOpen(close) => {
                self.remaining_depth -= 1;
                if self.remaining_depth == 0 {
                    return Err(self.peek_error(ErrorCode::RecursionLimitExceeded));
                }

                let ret = self.parse_list_meta(close);

                self.remaining_depth += 1;

                match (ret, self.end_seq(close)) {
                    (Ok(Some((head, meta))), Ok(())) => {
                        Datum::cons(head, meta, start, self.read.position())
                    }
                    (Ok(None), Ok(())) => {
                        Datum::primitive(Value::Null, start, self.read.position())
                    }
                    (Err(err), _) | (_, Err(err)) => return Err(err),
                }
            }
            Token::Quotation(name) => {
                // TODO: more specific error
                let token_end = self.read.position();
                let quoted = self
                    .next_datum()?
                    .ok_or_else(|| self.peek_error(ErrorCode::EofWhileParsingList))?;
                Datum::quotation(name, quoted, Span::new(start, token_end))
            }
        };
        Ok(Some(syntax))
    }

    fn parse_symbol(&mut self) -> Result<String> {
        self.scratch.clear();
        match self.read.parse_symbol(&mut self.scratch)? {
            Reference::Borrowed(s) => Ok(s.into()),
            Reference::Copied(s) => Ok(s.into()),
        }
    }

    fn parse_symbol_suffix(&mut self, prefix: &str) -> Result<String> {
        self.scratch.clear();
        self.scratch.extend(prefix.as_bytes());
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

    fn parse_byte_list(&mut self, close: u8) -> Result<Vec<u8>> {
        match self.parse_whitespace() {
            Err(e) => return Err(e),
            Ok(Some(b'(')) => self.eat_char(),
            Ok(Some(_)) => return Err(self.peek_error(ErrorCode::ExpectedVector)),
            Ok(None) => return Err(self.peek_error(ErrorCode::EofWhileParsingList)),
        }
        let mut bytes = Vec::new();
        loop {
            match self.parse_whitespace() {
                Err(e) => return Err(e),
                Ok(Some(c)) => {
                    if c == close {
                        self.eat_char();
                        break;
                    } else {
                        let n = self
                            .parse_number()?
                            .as_u64()
                            .ok_or_else(|| self.peek_error(ErrorCode::ExpectedOctet))?;
                        if n > 255 {
                            return Err(self.peek_error(ErrorCode::ExpectedOctet));
                        }
                        bytes.push(n as u8);
                    }
                }
                Ok(None) => return Err(self.peek_error(ErrorCode::EofWhileParsingList)),
            }
        }
        Ok(bytes)
    }

    fn parse_list(&mut self, terminator: u8) -> Result<Value> {
        let mut list = Cons::new(Value::Nil, Value::Null);
        let mut pair = &mut list;
        let mut have_value = false;
        loop {
            match self.parse_whitespace() {
                Err(e) => return Err(e),
                Ok(Some(c)) => match c {
                    b')' | b']' => {
                        if c != terminator {
                            return Err(self.peek_error(ErrorCode::MismatchedParenthesis));
                        }
                        if have_value {
                            return Ok(Value::Cons(list));
                        } else {
                            return Ok(Value::Null);
                        }
                    }
                    b'.' => {
                        self.eat_char();
                        let next = self.peek_or_null()?;
                        if next == 0 || is_delimiter(next) {
                            if !have_value {
                                return Err(self.peek_error(ErrorCode::ExpectedSomeValue));
                            }
                            pair.set_cdr(self.expect_value()?);
                            match self.parse_whitespace()? {
                                Some(b')') => return Ok(Value::Cons(list)),
                                Some(_) => {
                                    return Err(self.peek_error(ErrorCode::TrailingCharacters))
                                }
                                None => return Err(self.peek_error(ErrorCode::EofWhileParsingList)),
                            }
                        } else {
                            if have_value {
                                pair.set_cdr(Value::from((Value::Nil, Value::Null)));
                                pair = pair.cdr_mut().as_cons_mut().unwrap();
                            }
                            pair.set_car(Value::symbol(self.parse_symbol_suffix(".")?));
                            have_value = true;
                        }
                    }
                    _ => {
                        if have_value {
                            pair.set_cdr(Value::from((Value::Nil, Value::Null)));
                            pair = pair.cdr_mut().as_cons_mut().unwrap();
                        }
                        pair.set_car(self.expect_value()?);
                        have_value = true;
                    }
                },
                Ok(None) => return Err(self.peek_error(ErrorCode::EofWhileParsingList)),
            }
        }
    }

    fn parse_list_meta(&mut self, terminator: u8) -> Result<Option<(Cons, [SpanInfo; 2])>> {
        let mut list = Cons::new(Value::Nil, Value::Null);
        let null_meta = [SpanInfo::Prim(Span::empty()), SpanInfo::Prim(Span::empty())];
        let mut list_meta = null_meta.clone();
        let mut pair = &mut list;
        let mut meta = &mut list_meta;
        let mut have_value = false;
        loop {
            match self.parse_whitespace() {
                Err(e) => return Err(e),
                Ok(Some(c)) => match c {
                    b')' | b']' => {
                        if c != terminator {
                            return Err(self.peek_error(ErrorCode::MismatchedParenthesis));
                        }
                        if have_value {
                            return Ok(Some((list, list_meta)));
                        } else {
                            return Ok(None);
                        }
                    }
                    b'.' => {
                        let start = self.read.position();
                        self.eat_char();
                        let next = self.peek_or_null()?;
                        if next == 0 || is_delimiter(next) {
                            if !have_value {
                                return Err(self.peek_error(ErrorCode::ExpectedSomeValue));
                            }
                            let (cdr, cdr_meta) = self.expect_datum()?.into_inner();
                            pair.set_cdr(cdr);
                            meta[1] = cdr_meta;
                            match self.parse_whitespace()? {
                                Some(b')') => return Ok(Some((list, list_meta))),
                                Some(_) => {
                                    return Err(self.peek_error(ErrorCode::TrailingCharacters))
                                }
                                None => return Err(self.peek_error(ErrorCode::EofWhileParsingList)),
                            }
                        } else {
                            if have_value {
                                pair.set_cdr(Value::from((Value::Nil, Value::Null)));
                                meta[1] =
                                    SpanInfo::Cons(Span::empty(), Box::new(null_meta.clone()));
                                pair = pair.cdr_mut().as_cons_mut().unwrap();
                                meta = meta[1].cons_mut().unwrap();
                            }
                            pair.set_car(Value::symbol(self.parse_symbol_suffix(".")?));
                            meta[0] = SpanInfo::Prim(Span::new(start, self.read.position()));
                            have_value = true;
                        }
                    }
                    _ => {
                        if have_value {
                            pair.set_cdr(Value::from((Value::Nil, Value::Null)));
                            meta[1] = SpanInfo::Cons(Span::empty(), Box::new(null_meta.clone()));
                            pair = pair.cdr_mut().as_cons_mut().unwrap();
                            meta = meta[1].cons_mut().unwrap();
                        }
                        let (car, car_meta) = self.expect_datum()?.into_inner();
                        pair.set_car(car);
                        meta[0] = car_meta;
                        have_value = true;
                    }
                },
                Ok(None) => return Err(self.peek_error(ErrorCode::EofWhileParsingList)),
            }
        }
    }

    fn parse_vector(&mut self, terminator: u8) -> Result<Vec<Value>> {
        let mut elements = Vec::new();
        loop {
            match self.parse_whitespace() {
                Err(e) => return Err(e),
                Ok(Some(c)) => match c {
                    b')' | b']' => {
                        if c != terminator {
                            return Err(self.peek_error(ErrorCode::MismatchedParenthesis));
                        }
                        return Ok(elements);
                    }
                    _ => elements.push(self.expect_value()?),
                },
                Ok(None) => return Err(self.peek_error(ErrorCode::EofWhileParsingVector)),
            }
        }
    }

    fn parse_vector_meta(&mut self, terminator: u8) -> Result<(Vec<Value>, Vec<SpanInfo>)> {
        let mut elements = Vec::new();
        let mut element_meta = Vec::new();
        loop {
            match self.parse_whitespace() {
                Err(e) => return Err(e),
                Ok(Some(c)) => match c {
                    b')' | b']' => {
                        if c != terminator {
                            return Err(self.peek_error(ErrorCode::MismatchedParenthesis));
                        }
                        return Ok((elements, element_meta));
                    }
                    _ => {
                        let (value, meta) = self.expect_datum()?.into_inner();
                        elements.push(value);
                        element_meta.push(meta);
                    }
                },
                Ok(None) => return Err(self.peek_error(ErrorCode::EofWhileParsingVector)),
            }
        }
    }

    // Parses a full numeric literal, including a potential radix prefix
    fn parse_number(&mut self) -> Result<Number> {
        match self.peek_or_null()? {
            b'#' => {
                self.eat_char();
                match self.next_char_or_null()? {
                    b'b' => self.parse_radix_literal(2),
                    b'o' => self.parse_radix_literal(8),
                    b'd' => self.parse_radix_literal(10),
                    b'x' => self.parse_radix_literal(16),
                    _ => Err(self.peek_error(ErrorCode::InvalidNumber)),
                }
            }
            _ => self.parse_radix_literal(10),
        }
    }

    // Parses a numeric literal, including a potential sign, with the given radix
    fn parse_radix_literal(&mut self, radix: u8) -> Result<Number> {
        match self.peek_or_null()? {
            b'-' => {
                self.eat_char();
                self.parse_num_literal(radix, false)
            }
            b'+' => {
                self.eat_char();
                self.parse_num_literal(radix, true)
            }
            _ => self.parse_num_literal(radix, true),
        }
    }

    // Parses a numeric literal without a leading sign, with the given radix.
    fn parse_num_literal(&mut self, radix: u8, pos: bool) -> Result<Number> {
        let r = u64::from(radix);
        // There needs to be a leading digit (R7RS 7.1)
        let first_digit = match self.next_char_or_null()? {
            c @ b'0'..=b'9' => c - b'0',
            c @ b'a'..=b'f' => 10 + (c - b'a'),
            c @ b'A'..=b'F' => 10 + (c - b'A'),
            _ => return Err(self.peek_error(ErrorCode::InvalidNumber)),
        };
        if first_digit >= radix {
            return Err(self.peek_error(ErrorCode::InvalidNumber));
        }
        let mut res = u64::from(first_digit);
        loop {
            let digit = match self.peek_or_null()? {
                c @ b'0'..=b'9' => c - b'0',
                c @ b'a'..=b'f' => 10 + (c - b'a'),
                c @ b'A'..=b'F' => 10 + (c - b'A'),
                _ => return self.parse_num_tail(radix, pos, res),
            };
            if digit >= radix {
                return Err(self.peek_error(ErrorCode::InvalidNumber));
            }
            self.eat_char();
            let digit = u64::from(digit);
            // We need to be careful with overflow. If we can, try to keep the
            // number as a `u64` until we grow too large. At that point, switch to
            // parsing the value as a `f64`.
            if overflow!(res * r + digit, u64::MAX) {
                return Ok(Number::from(self.parse_long_integer(
                    radix, pos, res, 1, // res * 10^1
                )?));
            }
            res = res * r + digit;
        }
    }

    // Parse an over-long integer as a f64.
    fn parse_long_integer(
        &mut self,
        radix: u8,
        pos: bool,
        significand: u64,
        mut exponent: i32,
    ) -> Result<f64> {
        loop {
            let digit = match self.peek_or_null()? {
                c @ b'0'..=b'9' => c - b'0',
                c @ b'a'..=b'f' if radix >= 10 => 10 + (c - b'a'),
                c @ b'A'..=b'F' if radix >= 10 => 10 + (c - b'A'),
                b'.' => {
                    if radix != 10 {
                        return Err(self.peek_error(ErrorCode::InvalidNumber));
                    }
                    return self.parse_decimal(pos, significand, exponent);
                }
                b'e' | b'E' => {
                    if radix != 10 {
                        return Err(self.peek_error(ErrorCode::InvalidNumber));
                    }
                    return self.parse_exponent(pos, significand, exponent);
                }
                _ => {
                    return self.f64_from_parts(pos, significand, exponent);
                }
            };
            if digit >= radix {
                return Err(self.peek_error(ErrorCode::InvalidNumber));
            }
            self.eat_char();
            // This could overflow... if your integer is gigabytes long.
            // Ignore that possibility.
            exponent += 1;
        }
    }

    fn parse_num_tail(&mut self, radix: u8, pos: bool, significand: u64) -> Result<Number> {
        Ok(match self.peek_or_null()? {
            b'.' => {
                if radix != 10 {
                    return Err(self.peek_error(ErrorCode::InvalidNumber));
                }
                Number::from(self.parse_decimal(pos, significand, 0)?)
            }
            b'e' | b'E' => {
                if radix != 10 {
                    return Err(self.peek_error(ErrorCode::InvalidNumber));
                }
                Number::from(self.parse_exponent(pos, significand, 0)?)
            }
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
        while let c @ b'0'..=b'9' = self.peek_or_null()? {
            self.eat_char();
            let digit = u64::from(c - b'0');
            at_least_one_digit = true;

            if overflow!(significand * 10 + digit, u64::MAX) {
                // The next multiply/add would overflow, so just ignore all
                // further digits.
                while let b'0'..=b'9' = self.peek_or_null()? {
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
            b'e' | b'E' => self.parse_exponent(pos, significand, exponent),
            _ => self.f64_from_parts(pos, significand, exponent),
        }
    }

    fn parse_exponent(
        &mut self,
        positive: bool,
        significand: u64,
        starting_exp: i32,
    ) -> Result<f64> {
        self.eat_char();

        let positive_exp = match self.peek_or_null()? {
            b'+' => {
                self.eat_char();
                true
            }
            b'-' => {
                self.eat_char();
                false
            }
            _ => true,
        };

        // Make sure a digit follows the exponent place.
        let mut exp = match self.next_char_or_null()? {
            c @ b'0'..=b'9' => i32::from(c - b'0'),
            _ => {
                return Err(self.error(ErrorCode::InvalidNumber));
            }
        };

        while let c @ b'0'..=b'9' = self.peek_or_null()? {
            self.eat_char();
            let digit = i32::from(c - b'0');

            if overflow!(exp * 10 + digit, i32::max_value()) {
                return self.parse_exponent_overflow(positive, significand, positive_exp);
            }

            exp = exp * 10 + digit;
        }

        let final_exp = if positive_exp {
            starting_exp.saturating_add(exp)
        } else {
            starting_exp.saturating_sub(exp)
        };

        self.f64_from_parts(positive, significand, final_exp)
    }

    // This cold code should not be inlined into the middle of the hot
    // exponent-parsing loop above.
    #[cold]
    #[inline(never)]
    fn parse_exponent_overflow(
        &mut self,
        positive: bool,
        significand: u64,
        positive_exp: bool,
    ) -> Result<f64> {
        // Error instead of +/- infinity.
        if significand != 0 && positive_exp {
            return Err(self.error(ErrorCode::NumberOutOfRange));
        }

        while let b'0'..=b'9' = self.peek_or_null()? {
            self.eat_char();
        }
        Ok(if positive { 0.0 } else { -0.0 })
    }

    #[cfg(not(feature = "fast-float-parsing"))]
    fn f64_from_parts(&mut self, pos: bool, significand: u64, exponent: i32) -> Result<f64> {
        // Slow path -- in the `fast-float-parsing` variant, we
        // potentially lose digits by casting `significand` (which is
        // may exceeds 52 bits) to `f64`, as well as by the divisions
        // in there, so we when `fast-float-parsing` is disabled, we
        // delegate floating number parsing to the standard
        // library. Note that this code path is more than 3 orders of
        // magnitude slower.
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

    #[cfg(feature = "fast-float-parsing")]
    fn f64_from_parts(&mut self, pos: bool, significand: u64, mut exponent: i32) -> Result<f64> {
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

    fn end_seq(&mut self, close: u8) -> Result<()> {
        match self.parse_whitespace()? {
            Some(b) if b == close => {
                self.eat_char();
                Ok(())
            }
            Some(_) => Err(self.peek_error(ErrorCode::TrailingCharacters)),
            // TODO: path is taken for vectors as well
            None => Err(self.peek_error(ErrorCode::EofWhileParsingList)),
        }
    }
}

// This could probably profit from being a `u8 -> bool` LUT instead.
static SYMBOL_EXTENDED: [u8; 16] = [
    b'!', b'$', b'%', b'&', b'*', b'.', b'/', b':', b'<', b'=', b'>', b'?', b'@', b'^', b'_', b'~',
];

fn is_delimiter(c: u8) -> bool {
    c.is_ascii_whitespace() || b"|()\"".contains(&c)
}

// This implements the <sign subsequent> nonterminal of R7RS 7.1.1
fn is_sign_subsequent(c: u8) -> bool {
    c.is_ascii_alphabetic() || b"!$%&*/:<=>?@^_~-+@".contains(&c)
}

#[cfg(feature = "fast-float-parsing")]
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

fn from_trait<'de, R>(read: R, options: Options) -> Result<Value>
where
    R: Read<'de>,
{
    let mut parser = Parser::with_options(read, options);
    let value = parser.expect_value()?;
    parser.expect_end()?;

    Ok(value)
}

/// This implementation is deprecated in favor of `value_iter` since version
/// "0.2.5", but cannot be marked as such due to
/// <https://github.com/rust-lang/rust/issues/39935>.
impl<'de, R: Read<'de>> Iterator for Parser<R> {
    type Item = Result<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        self.value_iter().next()
    }
}

/// Parse a value from an IO stream containing a single S-expression.
///
/// The content of the IO stream is parsed directly from the stream
/// without being buffered in memory.
///
/// When reading from a source against which short reads are not efficient, such
/// as a [`File`], you will want to apply your own buffering, e.g. using
/// [`std::io::BufReader`].
///
/// ```no_run
/// use std::error::Error;
/// use std::fs::File;
/// use std::io::BufReader;
/// use std::path::Path;
///
/// fn read_value_from_file<P: AsRef<Path>>(path: P) -> Result<lexpr::Value, Box<dyn Error>> {
///     // Open the file in read-only mode with buffer.
///     let file = File::open(path)?;
///     let reader = BufReader::new(file);
///
///     // Read an arbitrary S-expression, using parser options suitable for Emacs Lisp.
///     let value = lexpr::from_reader_custom(reader, lexpr::parse::Options::elisp())?;
///
///     // Return the value.
///     Ok(value)
/// }
///
/// let value = read_value_from_file("test.el").unwrap();
/// println!("{:?}", value);
/// ```
///
/// [`File`]: https://doc.rust-lang.org/std/fs/struct.File.html
/// [`BufReader`]: https://doc.rust-lang.org/std/io/struct.BufReader.html
pub fn from_reader_custom(rdr: impl io::Read, options: Options) -> Result<Value> {
    from_trait(read::IoRead::new(rdr), options)
}

/// Parse a value from an IO stream of S-expressions, using the default parser
/// options.
///
/// See [`from_reader_custom`] for more information.
///
/// [`from_reader_custom`]: fn.from_reader_custom.html
pub fn from_reader(rdr: impl io::Read) -> Result<Value> {
    from_reader_custom(rdr, Options::default())
}

/// Parse a value from an IO stream of S-expressions, using the parser
/// options suitable for parsing Emacs Lisp.
///
/// See [`from_reader_custom`] for more information.
///
/// [`from_reader_custom`]: fn.from_reader_custom.html
pub fn from_reader_elisp(rdr: impl io::Read) -> Result<Value> {
    from_reader_custom(rdr, Options::elisp())
}

/// Parse a value from bytes representing a single S-expression.
///
/// ```
/// let value = lexpr::from_slice_custom(b"(a (nested) list)", lexpr::parse::Options::new());
/// println!("{:?}", value);
/// ```
pub fn from_slice_custom(bytes: &[u8], options: Options) -> Result<Value> {
    from_trait(read::SliceRead::new(bytes), options)
}

/// Parse a value from bytes representing a single S-expressions, using the
/// default parser options.
///
/// See [`from_slice_custom`] for more information.
///
/// [`from_slice_custom`]: fn.from_slice_custom.html
pub fn from_slice(bytes: &[u8]) -> Result<Value> {
    from_slice_custom(bytes, Options::default())
}

/// Parse a value from bytes representing a single S-expressions, using parser
/// options suitable for Emacs Lisp.
///
/// See [`from_slice_custom`] for more information.
///
/// [`from_slice_custom`]: fn.from_slice_custom.html
pub fn from_slice_elisp(bytes: &[u8]) -> Result<Value> {
    from_slice_custom(bytes, Options::elisp())
}

/// Parse a value from a string slice representing a single S-expression.
///
/// ```
/// let value = lexpr::from_str_custom("(a (nested) list)", lexpr::parse::Options::new());
/// println!("{:?}", value);
/// ```
pub fn from_str_custom(s: &str, options: Options) -> Result<Value> {
    from_trait(read::StrRead::new(s), options)
}

/// Parse a value from a string slice representing a single S-expressions, using
/// the default parser options.
///
/// See [`from_str_custom`] for more information.
///
/// [`from_str_custom`]: fn.from_str_custom.html
pub fn from_str(s: &str) -> Result<Value> {
    from_str_custom(s, Options::default())
}

/// Parse a value from a string slice representing a single S-expression, using
/// parser options suitable for Emacs Lisp.
///
/// See [`from_str_custom`] for more information.
///
/// [`from_str_custom`]: fn.from_str_custom.html
pub fn from_str_elisp(s: &str) -> Result<Value> {
    from_str_custom(s, Options::elisp())
}

/// Iterator over the values producedd by a parser.
pub struct ValueIter<'a, R>(&'a mut Parser<R>);

impl<'a, 'b, R> Iterator for ValueIter<'a, R>
where
    R: read::Read<'b>,
{
    type Item = Result<Value>;

    fn next(&mut self) -> Option<Self::Item> {
        // TODO: This is just `Result::transpose`, which go introduced in 1.33,
        // so update this when bumping MSRV
        match self.0.next_value() {
            Ok(Some(item)) => Some(Ok(item)),
            Ok(None) => None,
            Err(e) => Some(Err(e)),
        }
    }
}

/// Iterator over the datums producedd by a parser.
pub struct DatumIter<'a, R>(&'a mut Parser<R>);

impl<'a, 'b, R> Iterator for DatumIter<'a, R>
where
    R: read::Read<'b>,
{
    type Item = Result<Datum>;

    fn next(&mut self) -> Option<Self::Item> {
        // TODO: This is just `Result::transpose`, which got introduced in 1.33,
        // so update this when bumping MSRV
        match self.0.next_datum() {
            Ok(Some(item)) => Some(Ok(item)),
            Ok(None) => None,
            Err(e) => Some(Err(e)),
        }
    }
}

pub mod error;
mod iter;
pub(crate) mod read;

#[cfg(test)]
mod tests;
