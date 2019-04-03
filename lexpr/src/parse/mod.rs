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
use read::Reference;

use crate::{Cons, Number, Value};

pub use crate::style::KeywordStyle;

pub use read::{IoRead, Read, SliceRead, StrRead};

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
#[derive(Debug, Clone)]
pub struct Options {
    keyword_styles: u8,
    nil_symbol: NilSymbol,
    t_symbol: TSymbol,
    brackets: Brackets,
    string_syntax: StringSyntax,
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

/// Defines the accepted syntax for strings.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum StringSyntax {
    /// Syntax as specified the R6RS.
    ///
    /// Note that there is no R7RS variant, because R6RS specifies a superset of
    /// R7RS syntax.
    R6RS,
    /// Emacs Lisp syntax.
    ///
    /// Note that unibyte strings will be parsed as byte vectors.
    Elisp,
}

impl Options {
    /// Construct an empty set of options.
    ///
    /// This will recognize no keywords, and parse `nil` and `t` as
    /// regular symbols.
    pub fn new() -> Self {
        Options {
            keyword_styles: 0,
            nil_symbol: NilSymbol::Default,
            t_symbol: TSymbol::Default,
            brackets: Brackets::List,
            string_syntax: StringSyntax::R6RS,
        }
    }

    /// Construct a set of options suitable for parsing Emacs Lisp.
    pub fn elisp() -> Self {
        Self::new()
            .with_keyword_style(KeywordStyle::ColonPrefix)
            .with_nil_symbol(NilSymbol::EmptyList)
            .with_brackets(Brackets::Vector)
            .with_string_syntax(StringSyntax::Elisp)
    }

    /// Add `style` to the recognized keyword styles.
    pub fn with_keyword_style(mut self, style: KeywordStyle) -> Self {
        self.keyword_styles |= style.to_flag();
        self
    }

    /// Set the recognized keyword styles.
    pub fn with_keyword_styles<I, T>(mut self, styles: I) -> Self
    where
        I: IntoIterator<Item = T>,
        T: Borrow<KeywordStyle>,
    {
        self.keyword_styles = styles
            .into_iter()
            .fold(0, |flags, style| flags | style.borrow().to_flag());
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

    /// Check wether a keyword style is enabled.
    #[inline]
    pub fn keyword_style(&self, style: KeywordStyle) -> bool {
        (self.keyword_styles & style.to_flag()) != 0
    }

    /// Query the way the `nil` symbol is handled.
    pub fn nil_symbol(&self) -> NilSymbol {
        self.nil_symbol
    }

    /// Query the way the `t` symbol is handled.
    pub fn t_symbol(&self) -> TSymbol {
        self.t_symbol
    }

    /// Query the way brackets are handled.
    pub fn brackets(&self) -> Brackets {
        self.brackets
    }

    /// Query the accepted string syntax.
    pub fn string_syntax(&self) -> StringSyntax {
        self.string_syntax
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
            keyword_styles: KeywordStyle::Octothorpe.to_flag(),
            nil_symbol: NilSymbol::Default,
            t_symbol: TSymbol::Default,
            brackets: Brackets::List,
            string_syntax: StringSyntax::R6RS,
        }
    }
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
    ($a:ident * 10 + $b:ident, $c:expr) => {
        $a >= $c / 10 && ($a > $c / 10 || $b > $c % 10)
    };
}

impl<'de, R: Read<'de>> Parser<R> {
    /// The `Parser::end` method should be called after a value has been fully
    /// parsed.  This allows the `Parser` to validate that the input stream is
    /// at the end or that it only has trailing whitespace.
    pub fn end(&mut self) -> Result<()> {
        match self.parse_whitespace()? {
            Some(_) => Err(self.peek_error(ErrorCode::TrailingCharacters)),
            None => Ok(()),
        }
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
    /// This expects an S-expression value to be actually present, and
    /// returns an `Err` when called at the end of input. Use
    /// `Parser::parse` if you need to handle end of input gracefully.
    ///
    /// ```
    /// # use lexpr::{sexp, parse, Parser};
    /// let mut parser = Parser::from_str(r#"foo ("bar" . 3.14) #:baz (1 2 3)"#);
    /// assert_eq!(parser.parse_value().unwrap(), sexp!(foo));
    /// assert_eq!(parser.parse_value().unwrap(), sexp!(("bar" . 3.14)));
    /// assert_eq!(parser.parse_value().unwrap(), sexp!(#:baz));
    /// assert_eq!(parser.parse_value().unwrap(), sexp!((1 2 3)));
    /// assert!(parser.end().is_ok());
    /// ```
    pub fn parse_value(&mut self) -> Result<Value> {
        self.parse()
            .and_then(|o| o.ok_or_else(|| self.peek_error(ErrorCode::EofWhileParsingValue)))
    }

    /// Parse a single S-expression from the input source.
    ///
    /// If the end of input is ecountered, this will return
    /// `Ok(None)`, otherwise, if parsing suceeded, `Ok(Some(Value))`.
    ///
    /// ```
    /// # use lexpr::{sexp, parse, Parser};
    /// let mut parser = Parser::from_str(r#"foo ("bar" . 3.14) #:baz (1 2 3)"#);
    /// assert_eq!(parser.parse().unwrap(), Some(sexp!(foo)));
    /// assert_eq!(parser.parse().unwrap(), Some(sexp!(("bar" . 3.14))));
    /// assert_eq!(parser.parse().unwrap(), Some(sexp!(#:baz)));
    /// assert_eq!(parser.parse().unwrap(), Some(sexp!((1 2 3))));
    /// assert_eq!(parser.parse().unwrap(), None);
    /// ```
    pub fn parse(&mut self) -> Result<Option<Value>> {
        let peek = match self.parse_whitespace()? {
            Some(b) => b,
            None => return Ok(None),
        };

        let value = match peek {
            b'#' => {
                self.eat_char();
                match self.next_char()? {
                    Some(b't') => Ok(Value::from(true)),
                    Some(b'f') => Ok(Value::from(false)),
                    Some(b'n') => {
                        self.expect_ident(b"il")?;
                        Ok(Value::Nil)
                    }
                    Some(b'(') => {
                        self.remaining_depth -= 1;
                        if self.remaining_depth == 0 {
                            return Err(self.peek_error(ErrorCode::RecursionLimitExceeded));
                        }

                        let ret = self.parse_vector_elements(b')');

                        self.remaining_depth += 1;

                        match (ret, self.end_seq()) {
                            (Ok(elements), Ok(())) => Ok(Value::Vector(elements.into())),
                            (Err(err), _) | (_, Err(err)) => Err(err),
                        }
                    }
                    Some(b':') if self.options.keyword_style(KeywordStyle::Octothorpe) => {
                        Ok(Value::keyword(self.parse_symbol()?))
                    }
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
                let ret = self.parse_list_elements(b')');

                self.remaining_depth += 1;

                match (ret, self.end_seq()) {
                    (Ok(ret), Ok(())) => Ok(ret),
                    (Err(err), _) | (_, Err(err)) => Err(err),
                }
            }
            b'[' => match self.options.brackets {
                Brackets::Vector => {
                    self.remaining_depth -= 1;
                    if self.remaining_depth == 0 {
                        return Err(self.peek_error(ErrorCode::RecursionLimitExceeded));
                    }

                    self.eat_char();
                    let ret = self.parse_vector_elements(b']');

                    self.remaining_depth += 1;

                    match (ret, self.end_seq()) {
                        (Ok(elements), Ok(())) => Ok(Value::Vector(elements.into())),
                        (Err(err), _) | (_, Err(err)) => Err(err),
                    }
                }
                Brackets::List => {
                    self.remaining_depth -= 1;
                    if self.remaining_depth == 0 {
                        return Err(self.peek_error(ErrorCode::RecursionLimitExceeded));
                    }

                    self.eat_char();
                    let ret = self.parse_list_elements(b']');

                    self.remaining_depth += 1;

                    match (ret, self.end_seq()) {
                        (Ok(ret), Ok(())) => Ok(ret),
                        (Err(err), _) | (_, Err(err)) => Err(err),
                    }
                }
            },
            b':' => {
                if self.options.keyword_style(KeywordStyle::ColonPrefix) {
                    self.eat_char();
                    Ok(Value::keyword(self.parse_symbol()?))
                } else {
                    Ok(Value::symbol(self.parse_symbol()?))
                }
            }
            b'a'...b'z' | b'A'...b'Z' => {
                let mut name = self.parse_symbol()?;
                if self.options.keyword_style(KeywordStyle::ColonPostfix) && name.ends_with(':') {
                    name.pop();
                    Ok(Value::keyword(name))
                } else if self.options.nil_symbol() != NilSymbol::Default && name == "nil" {
                    match self.options.nil_symbol() {
                        NilSymbol::EmptyList => Ok(Value::Null),
                        NilSymbol::Special => Ok(Value::Nil),
                        NilSymbol::Default => unreachable!(),
                    }
                } else if self.options.t_symbol() != TSymbol::Default && name == "t" {
                    match self.options.t_symbol() {
                        TSymbol::True => Ok(Value::from(true)),
                        TSymbol::Default => unreachable!(),
                    }
                } else {
                    Ok(Value::symbol(name))
                }
            }
            _ => {
                if SYMBOL_EXTENDED.contains(&peek) {
                    Ok(Value::symbol(self.parse_symbol()?))
                } else {
                    Err(self.peek_error(ErrorCode::ExpectedSomeValue))
                }
            }
        };

        match value {
            Ok(value) => Ok(Some(value)),
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

    fn parse_list_elements(&mut self, terminator: u8) -> Result<Value> {
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
                        pair.set_cdr(self.parse_value()?);
                        match self.parse_whitespace()? {
                            Some(b')') => return Ok(Value::Cons(list)),
                            Some(_) => return Err(self.peek_error(ErrorCode::TrailingCharacters)),
                            None => return Err(self.peek_error(ErrorCode::EofWhileParsingList)),
                        }
                    }
                    _ => {
                        if have_value {
                            pair.set_cdr(Value::from((Value::Nil, Value::Null)));
                            pair = pair.cdr_mut().as_cons_mut().unwrap();
                        }
                        pair.set_car(self.parse_value()?);
                        have_value = true;
                    }
                },
                Ok(None) => return Err(self.peek_error(ErrorCode::EofWhileParsingList)),
            }
        }
    }

    fn parse_vector_elements(&mut self, terminator: u8) -> Result<Vec<Value>> {
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
                    _ => elements.push(self.parse_value()?),
                },
                Ok(None) => return Err(self.peek_error(ErrorCode::EofWhileParsingVector)),
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
                b'e' | b'E' => {
                    return self.parse_exponent(pos, significand, exponent);
                }
                _ => {
                    return self.f64_from_parts(pos, significand, exponent);
                }
            }
        }
    }

    fn parse_number(&mut self, pos: bool, significand: u64) -> Result<Number> {
        Ok(match self.peek_or_null()? {
            b'.' => Number::from(self.parse_decimal(pos, significand, 0)?),
            b'e' | b'E' => Number::from(self.parse_exponent(pos, significand, 0)?),
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
            c @ b'0'...b'9' => i32::from(c - b'0'),
            _ => {
                return Err(self.error(ErrorCode::InvalidNumber));
            }
        };

        while let c @ b'0'...b'9' = self.peek_or_null()? {
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

        while let b'0'...b'9' = self.peek_or_null()? {
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

    fn end_seq(&mut self) -> Result<()> {
        match self.parse_whitespace()? {
            Some(b')') | Some(b']') => {
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
    let value = parser.parse_value()?;
    parser.end()?;

    Ok(value)
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
/// ```
/// use std::error::Error;
/// use std::fs::File;
/// use std::io::BufReader;
/// use std::path::Path;
///
/// fn read_value_from_file<P: AsRef<Path>>(path: P) -> Result<lexpr::Value, Box<Error>> {
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
/// fn main() {
/// # }
/// # fn fake_main() {
///     let value = read_value_from_file("test.el").unwrap();
///     println!("{:?}", value);
/// }
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

mod error;
mod iter;
mod read;

#[cfg(test)]
mod tests;
