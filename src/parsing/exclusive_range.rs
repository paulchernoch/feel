use std::ops::{RangeBounds,Bound};

/// Rust lacks a Range that has an Exclusive start and Exclusive end.
#[derive(Clone)]
pub struct ExclusiveRange<T: Clone> {
  pub start: T,
  pub end: T
}

impl<C: Copy + Clone> Copy for ExclusiveRange<C> {}

impl<T> RangeBounds<T> for ExclusiveRange<&T> {
  fn start_bound(&self) -> Bound<&T> {
    Bound::Excluded(self.start)
  }
  fn end_bound(&self) -> Bound<&T> {
    Bound::Excluded(self.end)
  }
}
