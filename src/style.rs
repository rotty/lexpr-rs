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
