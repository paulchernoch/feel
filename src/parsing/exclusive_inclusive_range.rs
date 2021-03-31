use std::ops::{RangeBounds,Bound};

/// Rust lacks a Range that has an Exclusive start and Inclusive end.
pub struct ExclusiveInclusiveRange<T: Clone> {
  pub start: T,
  pub end: T
}

impl<T> RangeBounds<T> for ExclusiveInclusiveRange<&T> {
  fn start_bound(&self) -> Bound<&T> {
    Bound::Excluded(self.start)
  }
  fn end_bound(&self) -> Bound<&T> {
    Bound::Included(self.end)
  }
}


