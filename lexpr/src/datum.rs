//! S-expression values including source location.

use std::{io, iter, slice};

use crate::{
    parse::{read, Options, Parser, Position, Result},
    Cons, Value,
};

/// Combines an S-expression value with location information.
///
/// A `Datum` keeps, along with a plain `Value`, information about the text
/// location the value was parsed from. For compound values, such as lists and
/// vectors, that includes information for all contained values, recursively.
///
/// A `Datum` can be obtained by using the [`next_datum`] and [`expect_datum`]
/// methods on `Parser`, or via the iterator obtained with [`datum_iter`].
///
/// [`next_datum`]: Parser::next_datum
/// [`expect_datum`]: Parser::expect_datum
/// [`datum_iter`]: Parser::datum_iter
#[derive(Debug, Clone, PartialEq)]
pub struct Datum {
    value: Value,
    info: SpanInfo,
}

impl Datum {
    pub(crate) fn into_inner(self) -> (Value, SpanInfo) {
        (self.value, self.info)
    }

    /// Returns a reference to the contained value.
    pub fn value(&self) -> &Value {
        &self.value
    }

    /// Returns the span for the compelete value.
    pub fn span(&self) -> Span {
        self.info.span()
    }

    /// Returns a reference to the datum.
    pub fn as_ref(&self) -> Ref<'_> {
        Ref {
            value: &self.value,
            info: &self.info,
        }
    }

    /// Returns an iterator over the elements of a list.
    ///
    /// If the value contained in the datum is not either a cons cell or `Null`, `None` is
    /// returned.
    ///
    /// Note that the returned iterator has special behavior for improper lists, yielding the
    /// element after the dot after returning `None` the first time.
    ///
    /// ```
    /// # use lexpr_macros::sexp;
    ///
    /// let datum = lexpr::datum::from_str("(1 2 . 3)").unwrap();
    /// let mut iter = datum.list_iter().unwrap();
    /// let one = iter.next().unwrap();
    /// assert_eq!(one.value(), &sexp!(1));
    /// let two = iter.next().unwrap();
    /// assert_eq!(two.value(), &sexp!(2));
    /// assert_eq!(iter.next(), None);
    /// let three = iter.next().unwrap();
    /// assert_eq!(three.value(), &sexp!(3));
    /// assert_eq!(iter.next(), None);
    /// ```
    pub fn list_iter(&self) -> Option<ListIter<'_>> {
        self.as_ref().list_iter()
    }

    /// Returns an iterator over the elements of a vector.
    ///
    /// If the value contained in the datum is not a vector, `None` is returned.
    pub fn vector_iter(&self) -> Option<VectorIter<'_>> {
        self.as_ref().vector_iter()
    }

    pub(crate) fn primitive(value: Value, start: Position, end: Position) -> Self {
        Datum {
            value,
            info: SpanInfo::Prim(Span { start, end }),
        }
    }

    pub(crate) fn vec(
        elements: Vec<Value>,
        element_info: Vec<SpanInfo>,
        start: Position,
        end: Position,
    ) -> Self {
        Datum {
            value: Value::Vector(elements),
            info: SpanInfo::Vec(Span { start, end }, element_info),
        }
    }

    pub(crate) fn cons(cell: Cons, meta: [SpanInfo; 2], start: Position, end: Position) -> Self {
        Datum {
            value: Value::Cons(cell),
            info: SpanInfo::Cons(Span::new(start, end), Box::new(meta)),
        }
    }

    pub(crate) fn quotation(name: &str, quoted: Datum, quote_span: Span) -> Self {
        let (quoted_value, quoted_info) = quoted.into_inner();
        let quoted_end = quoted_info.span().end();
        let null_span = Span::new(quoted_end, quoted_end);
        Datum {
            value: Value::list(vec![Value::symbol(name), quoted_value]),
            info: SpanInfo::Cons(
                Span::new(quote_span.start(), quoted_end),
                Box::new([
                    SpanInfo::Prim(quote_span),
                    SpanInfo::Cons(
                        quoted_info.span(),
                        Box::new([quoted_info, SpanInfo::Prim(null_span)]),
                    ),
                ]),
            ),
        }
    }
}

impl From<Datum> for Value {
    fn from(datum: Datum) -> Self {
        datum.value
    }
}

/// A reference to a value and corresponding location information.
///
/// A `Ref` is the generalized version of `&Datum`; it can not only refer a top-level, owned `Datum`
/// value, but also to values recursively contained therein.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Ref<'a> {
    value: &'a Value,
    info: &'a SpanInfo,
}

impl<'a> AsRef<Value> for Ref<'a> {
    fn as_ref(&self) -> &Value {
        self.value
    }
}

impl<'a> From<Ref<'a>> for Datum {
    /// Turns a reference into an owned `Datum`, by cloning the referenced value and location
    /// information.
    fn from(r: Ref<'a>) -> Self {
        Datum {
            value: r.value.clone(),
            info: r.info.clone(),
        }
    }
}

impl<'a> Ref<'a> {
    fn new(value: &'a Value, info: &'a SpanInfo) -> Self {
        Ref { value, info }
    }

    /// Returns the span of the referenced value.
    pub fn span(&self) -> Span {
        self.info.span()
    }

    /// Returns a reference to the contained value.
    pub fn value(&self) -> &'a Value {
        self.value
    }

    /// If the value referenced is not either a cons cell or `Null`, `None` is returned.
    ///
    /// Note that the returned iterator has special behavior for improper lists, yielding the
    /// element after the dot after returning `None` the first time; see [`Datum::list_iter`] for an
    /// example.
    pub fn list_iter(&self) -> Option<ListIter<'a>> {
        match (self.value, self.info) {
            (Value::Cons(cell), SpanInfo::Cons(_, meta)) => Some(ListIter::cons(cell, meta)),
            (Value::Null, _) => Some(ListIter::empty()),
            _ => None,
        }
    }

    /// Returns an iterator over the elements of a vector.
    ///
    /// If the value referenced is not a vector, `None` is returned.
    pub fn vector_iter(&self) -> Option<VectorIter<'a>> {
        match (self.value, self.info) {
            (Value::Vector(elements), SpanInfo::Vec(_, element_meta)) => {
                Some(VectorIter(elements.iter().zip(element_meta)))
            }
            _ => None,
        }
    }

    /// Returns a pair of references to the fields of a cons cell.
    ///
    /// If the value referenced is not a cons cell, `None` is returned.
    pub fn as_pair(&self) -> Option<(Ref<'a>, Ref<'a>)> {
        let (car, cdr) = self.value.as_pair()?;
        match &self.info {
            SpanInfo::Cons(_, inner) if inner.len() == 2 => {
                Some((Ref::new(car, &inner[0]), Ref::new(cdr, &inner[1])))
            }
            _ => unreachable!("badly shaped pair span information"),
        }
    }
}

impl<'a> std::ops::Deref for Ref<'a> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        self.value
    }
}

/// The start and end for a span of text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    start: Position,
    end: Position,
}

impl Span {
    pub(crate) fn new(start: Position, end: Position) -> Self {
        Span { start, end }
    }

    pub(crate) fn empty() -> Self {
        Span {
            start: Position::new(0, 0),
            end: Position::new(0, 0),
        }
    }

    /// Get the starting line/column in the source file for this span.
    pub fn start(&self) -> Position {
        self.start
    }

    /// Get the ending line/column in the source file for this span.
    pub fn end(&self) -> Position {
        self.end
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum SpanInfo {
    Prim(Span),
    Cons(Span, Box<[SpanInfo; 2]>),
    Vec(Span, Vec<SpanInfo>),
}

impl SpanInfo {
    fn span(&self) -> Span {
        match self {
            SpanInfo::Prim(span) => *span,
            SpanInfo::Cons(span, _) => *span,
            SpanInfo::Vec(span, _) => *span,
        }
    }
    pub(crate) fn cons_mut(&mut self) -> Option<&mut [SpanInfo; 2]> {
        match self {
            SpanInfo::Cons(_, info) => Some(info),
            _ => None,
        }
    }
}

/// An iterator over the elements
#[derive(Debug, Clone)]
pub struct VectorIter<'a>(iter::Zip<slice::Iter<'a, Value>, slice::Iter<'a, SpanInfo>>);

impl<'a> Iterator for VectorIter<'a> {
    type Item = Ref<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(value, info)| Ref { value, info })
    }
}

/// An iterator yielding the `car` field of a chain of cons cells.
///
/// # Improper lists
///
/// Since in Lisp, lists can be "improper", i.e., terminated by a value other than `Null`, this
/// iterator type takes advantage of the fact that Rust's iterators can produce multiple sequences
/// of values, each terminated by `None`. For an improper list, the terminating value is produced
/// after the sequence of elements, as a singleton element, again followed by `None`.
///
/// For example, while the list `(1 2 3)` will produce the three expected `Some` values, followed by
/// `None`, the list `(1 2 . 3)` will produce `Some` values for `1` and `2`, then a `None`, followed
/// by a some value for `3`, and then the final `None`.
#[derive(Debug, Clone)]
pub struct ListIter<'a>(ListCursor<'a>);

impl<'a> ListIter<'a> {
    /// Returns true when the iterator is completely exhausted.
    ///
    /// For an improper list, true will only be returned after the terminating value has been
    /// consumed.
    pub fn is_empty(&self) -> bool {
        matches!(&self.0, ListCursor::Exhausted)
    }

    /// Returns a peek at the value that would be returned by a call to `next`.
    ///
    /// For improper lists, this implies that after the last regular element, `None` will be
    /// returned, while `is_empty` still returns false at that point.
    pub fn peek(&self) -> Option<Ref<'_>> {
        match &self.0 {
            ListCursor::Cons(cell, info) => Some(Ref {
                value: cell.car(),
                info: &info[0],
            }),
            ListCursor::Dot(_, _) => None,
            ListCursor::Rest(value, info) => Some(Ref { value, info }),
            ListCursor::Exhausted => None,
        }
    }

    fn empty() -> Self {
        ListIter(ListCursor::Exhausted)
    }

    fn cons(cell: &'a Cons, meta: &'a [SpanInfo; 2]) -> Self {
        ListIter(ListCursor::Cons(cell, meta))
    }
}

#[derive(Debug, Clone)]
enum ListCursor<'a> {
    Cons(&'a Cons, &'a [SpanInfo; 2]),
    Dot(&'a Value, &'a SpanInfo),
    Rest(&'a Value, &'a SpanInfo),
    Exhausted,
}

impl<'a> Iterator for ListIter<'a> {
    type Item = Ref<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0 {
            ListCursor::Cons(cell, [car_meta, cdr_meta]) => {
                let car = cell.car();
                match cdr_meta {
                    SpanInfo::Cons(_, next) => {
                        let cell = cell
                            .cdr()
                            .as_cons()
                            .expect("badly shaped list span information");
                        self.0 = ListCursor::Cons(cell, next);
                    }
                    SpanInfo::Prim(_) if cell.cdr().is_null() => {
                        self.0 = ListCursor::Exhausted;
                    }
                    _ => {
                        self.0 = ListCursor::Dot(cell.cdr(), cdr_meta);
                    }
                }
                Some(Ref {
                    value: car,
                    info: car_meta,
                })
            }
            ListCursor::Dot(value, info) => {
                self.0 = ListCursor::Rest(value, info);
                None
            }
            ListCursor::Rest(value, info) => {
                self.0 = ListCursor::Exhausted;
                Some(Ref { value, info })
            }
            ListCursor::Exhausted => None,
        }
    }
}

fn from_trait<'de, R>(read: R, options: Options) -> Result<Datum>
where
    R: read::Read<'de>,
{
    let mut parser = Parser::with_options(read, options);
    let datum = parser.expect_datum()?;
    parser.expect_end()?;

    Ok(datum)
}

/// Parse a datum from an IO stream containing a single S-expression.
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
/// fn read_datum_from_file<P: AsRef<Path>>(path: P) -> Result<lexpr::Datum, Box<dyn Error>> {
///     // Open the file in read-only mode with buffer.
///     let file = File::open(path)?;
///     let reader = BufReader::new(file);
///
///     // Read an arbitrary S-expression, using parser options suitable for Emacs Lisp.
///     let datum = lexpr::datum::from_reader_custom(reader, lexpr::parse::Options::elisp())?;
///
///     // Return the datum.
///     Ok(datum)
/// }
///
/// let datum = read_datum_from_file("test.el").unwrap();
/// println!("{:?}", datum);
/// ```
///
/// [`File`]: https://doc.rust-lang.org/std/fs/struct.File.html
/// [`BufReader`]: https://doc.rust-lang.org/std/io/struct.BufReader.html
pub fn from_reader_custom(rdr: impl io::Read, options: Options) -> Result<Datum> {
    from_trait(read::IoRead::new(rdr), options)
}

/// Parse a datum from an IO stream of S-expressions, using the default parser
/// options.
///
/// See [`from_reader_custom`] for more information.
///
/// [`from_reader_custom`]: fn.from_reader_custom.html
pub fn from_reader(rdr: impl io::Read) -> Result<Datum> {
    from_reader_custom(rdr, Options::default())
}

/// Parse a datum from an IO stream of S-expressions, using the parser
/// options suitable for parsing Emacs Lisp.
///
/// See [`from_reader_custom`] for more information.
///
/// [`from_reader_custom`]: fn.from_reader_custom.html
pub fn from_reader_elisp(rdr: impl io::Read) -> Result<Datum> {
    from_reader_custom(rdr, Options::elisp())
}

/// Parse a datum from bytes representing a single S-expression.
///
/// ```
/// let datum = lexpr::from_slice_custom(b"(a (nested) list)", lexpr::parse::Options::new());
/// println!("{:?}", datum);
/// ```
pub fn from_slice_custom(bytes: &[u8], options: Options) -> Result<Datum> {
    // TODO: the use of SliceRead is most probably not a good idea, since it calculates position
    // information on-demand, leading to O(n^2) complexity.
    from_trait(read::SliceRead::new(bytes), options)
}

/// Parse a datum from bytes representing a single S-expressions, using the
/// default parser options.
///
/// See [`from_slice_custom`] for more information.
///
/// [`from_slice_custom`]: fn.from_slice_custom.html
pub fn from_slice(bytes: &[u8]) -> Result<Datum> {
    from_slice_custom(bytes, Options::default())
}

/// Parse a datum from bytes representing a single S-expressions, using parser
/// options suitable for Emacs Lisp.
///
/// See [`from_slice_custom`] for more information.
///
/// [`from_slice_custom`]: fn.from_slice_custom.html
pub fn from_slice_elisp(bytes: &[u8]) -> Result<Datum> {
    from_slice_custom(bytes, Options::elisp())
}

/// Parse a datum from a string slice representing a single S-expression.
///
/// ```
/// let datum = lexpr::from_str_custom("(a (nested) list)", lexpr::parse::Options::new());
/// println!("{:?}", datum);
/// ```
pub fn from_str_custom(s: &str, options: Options) -> Result<Datum> {
    // TODO: the use of StrRead (which delegates to SliceRead) is most probably not a good idea,
    // since it calculates position information on-demand, leading to O(n^2) complexity.
    from_trait(read::StrRead::new(s), options)
}

/// Parse a datum from a string slice representing a single S-expressions, using
/// the default parser options.
///
/// See [`from_str_custom`] for more information.
///
/// [`from_str_custom`]: fn.from_str_custom.html
pub fn from_str(s: &str) -> Result<Datum> {
    from_str_custom(s, Options::default())
}

/// Parse a datum from a string slice representing a single S-expression, using
/// parser options suitable for Emacs Lisp.
///
/// See [`from_str_custom`] for more information.
///
/// [`from_str_custom`]: fn.from_str_custom.html
pub fn from_str_elisp(s: &str) -> Result<Datum> {
    from_str_custom(s, Options::elisp())
}
