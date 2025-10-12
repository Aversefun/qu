//! Transformers for code.

use crate::ir::IRItem;

/// A transformer that acts upon IR.
pub trait Transformer<'a, T: IRItem> {
    /// Metadata needed to transform.
    type Meta: Default;
    /// The default meta value.
    #[must_use]
    fn default_meta() -> Self::Meta {
        Default::default()
    }
    /// Transform the items in some way.
    ///
    /// # Errors
    /// Returns an error if:
    /// 1. An error was detected with the IR.
    /// 2. An error was encountered while attempting to apply the transform.
    /// 3. An error in a sub-transformer occurred (propogation).
    fn transform(
        meta: Self::Meta,
        items: &'a mut [T],
    ) -> Result<(), crate::errors::TransformerError<'a>>;
}
