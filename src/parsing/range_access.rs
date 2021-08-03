use super::context::ContextReader;
use super::feel_value::FeelValue;

pub trait RangeAccess {
    /// Number of items in a FeelValue::Range over numbers, or zero if a different type of
    /// valus is given.
    fn range_length<C: ContextReader>(&self, contexts: &C) -> usize;

    /// Gets the item at the zero-based position within a FeelValue::Range over numbers. 
    /// Returns a FeelValue::Null if out of range.
    /// If the Range is defined as a reverse range, start from the end and work backwards.
    fn index<C: ContextReader>(&self, position: usize, contexts: &C) -> FeelValue;
}