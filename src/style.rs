#[derive(Debug, Clone, Copy)]
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
    ColonSuffix,
    /// Parse identifiers prefixed with `#:` as keywords.
    ///
    /// In the absence of this option, the sequence `#:` will result
    /// in a parser error.
    Octothorpe,
}
