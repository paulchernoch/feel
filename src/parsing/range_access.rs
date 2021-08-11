use super::context::ContextReader;
use super::feel_value::FeelValue;

pub trait RangeAccess {
    /// Number of items in a FeelValue::Range over numbers, or zero if a different type of
    /// valus is given.
    fn range_length<C: ContextReader>(&self, contexts: &C) -> usize;

    /// Obtain the loop bounds for a conforming numeric range or a list. 
    /// The tuple will hold these FeelValue::Numbers: 
    ///    - start position 
    ///    - stop position
    ///    - count
    ///    - step size
    /// 
    /// If not a lisrt or a Numeric Range where endpoints are integers, then return this: 
    ///    - zero
    ///    - zero
    ///    - zero
    ///    - one
    fn loop_bounds<C: ContextReader>(&self, contexts: &C) -> (FeelValue,FeelValue,FeelValue,FeelValue);

    /// Indexes a List or Range.
    ///   If a Range: Gets the item at the zero-based position within a FeelValue::Range over numbers. 
    ///               Returns a FeelValue::Null if out of range.
    ///               If the Range is defined as a reverse range, 
    ///               start from the end and work backwards.
    ///   If a List:  Gets a clone of the given item from the List, 
    ///               or FeelValue::Null if out of bounds.
    fn range_index<C: ContextReader>(&self, position: usize, contexts: &C) -> FeelValue;
}