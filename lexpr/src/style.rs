/// Indicates a style of keywords.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum KeywordStyle {
    /// Parse identifiers starting with a colon as keywords.
    ///
    /// In the absence of this option, such identifiers would be
    /// parsed as symbols.
    ColonPrefix,

    /// Parse identifiers ending with a colon as keywords.
    ///
    /// In the absence of this option, such identifiers would be
    /// parsed as symbols.
    ColonPostfix,

    /// Parse identifiers prefixed with `#:` as keywords.
    ///
    /// In the absence of this option, the sequence `#:` will result
    /// in a parser error.
    Octothorpe,
}

impl KeywordStyle {
    #[inline]
    pub(crate) fn to_flag(self) -> u8 {
        use KeywordStyle::*;
        match self {
            ColonPrefix => 1,
            ColonPostfix => 2,
            Octothorpe => 4,
        }
    }
}

/// Indicates the syntax for strings.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum StringSyntax {
    /// Syntax as specified the R6RS.
    ///
    /// Note that there is no R7RS variant, because R6RS specifies a superset of
    /// R7RS syntax. When printing however, the syntax used will be restricted
    /// to be understood by an R7RS parser.
    R6RS,

    /// Emacs Lisp syntax.
    ///
    /// Note that unibyte strings will be parsed as byte vectors.
    Elisp,
}
