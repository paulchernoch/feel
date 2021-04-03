use std::fmt::{Debug,Display,Formatter,Result};
use std::cmp::{Ord, PartialOrd, Ordering};
use super::feel_value::{FeelValue, FeelType};
use super::context::ContextReader;
use std::ops::{RangeBounds, Bound};

// Note: Many important functions involving Ranges are implemented in Builtins. 

/// A Feel Range object, that decides whether a value falls within a range of low and high values.
///   - The ends of the range may be inclusive or exclusive. 
///   - The low and high values may be FeelValue::Name values, indicating that they should be looked up
///     from a NestedContext.
///   - The low and high values may be Option::None, indicating an open range in that direction. 
#[derive(PartialEq, Eq, Clone)]
pub struct Range {
  low: Box<Option<FeelValue>>,
  high: Box<Option<FeelValue>>,
  low_inclusive: bool,
  high_inclusive: bool
}

impl Range {

  /// Create a new Range that has both a low and high value.
  /// Panics if the types of the values do not match, 
  /// except if one or the other value is a Name, which will be used
  /// to lookup the real value in a supplied context.
  pub fn new(low: &FeelValue, high: &FeelValue, low_inclusive: bool, high_inclusive: bool) -> Self {
    if low.get_type() != high.get_type() && low.get_type() != FeelType::Name && high.get_type() != FeelType::Name {
      panic!("Range low and high values must be of the same type");
    }
    Range {
      low: Box::new(Option::Some(low.clone())),
      high: Box::new(Option::Some(high.clone())),
      low_inclusive: low_inclusive,
      high_inclusive: high_inclusive
    }
  }


  /// Create a new open-ended Range that has a low but no high value. 
  pub fn new_with_low(low: &FeelValue, inclusive: bool) -> Self {
    Range {
      low: Box::new(Option::Some(low.clone())),
      high: Box::new(None),
      low_inclusive: inclusive,
      high_inclusive: true
    }
  }

  /// Create a new open-ended Range that has a high but no low value. 
  pub fn new_with_high(high: &FeelValue, inclusive: bool) -> Self {
    Range {
      low: Box::new(None),
      high: Box::new(Option::Some(high.clone())),
      low_inclusive: true,
      high_inclusive: inclusive
    }
  }

  /// Identifies which FeelTypes can be used to define the beginning and end of a Range or
  /// be a point tested against a range by one of the Builtin Range functions.
  pub fn is_suitable_as_point(ft: FeelType) -> bool {
    match ft {
      FeelType::Boolean => true, // Nonsensical, but allowed.
      FeelType::Date => true,
      FeelType::DateAndTime => true,
      FeelType::Name => true,
      FeelType::Number => true,
      FeelType::String => true,
      FeelType::Time => true,
      FeelType::YearMonthDuration => true,
      _ => false
    }
  }

  // Note on start_bound and end_bound:
  //   These methods are inspired by trait std::ops::RangeBounds,
  //   but we could not use that trait because we need an extra context 
  //   object passed as an argument.

  /// Gets the start bound of the range (which may be Unbounded).
  /// If it is a QName, look up its actual value from the supplied Context.
  pub fn start_bound<C: ContextReader>(&self, contexts: &C) -> Bound<FeelValue> {
    match &*(self.low) {
      None => Bound::Unbounded,
      Some(FeelValue::Name(qname)) => {
        let low_from_context_opt = contexts.get(qname.clone());
        match (low_from_context_opt, self.low_inclusive) {
          (None, _) => Bound::Included(FeelValue::Null), // TODO: Should log error
          (Some(low_value_from_ctx), true) => Bound::Included(low_value_from_ctx),
          (Some(low_value_from_ctx), false) => Bound::Excluded(low_value_from_ctx)
        }          
      },
      Some(low_value) => {
        if self.low_inclusive {
          Bound::Included(low_value.clone())
        }
        else {
          Bound::Excluded(low_value.clone())
        }
      }
    }
  }

  /// Gets the end bound of the range (which may be Unbounded).
  /// If it is a QName, look up its actual value from the supplied Context.
  pub fn end_bound<C: ContextReader>(&self, contexts: &C) -> Bound<FeelValue> {
    match &*(self.high) {
      None => Bound::Unbounded,
      Some(FeelValue::Name(qname)) => {
        let high_from_context_opt = contexts.get(qname.clone());
        match (high_from_context_opt, self.high_inclusive) {
          (None, _) => Bound::Included(FeelValue::Null), // TODO: Should log error
          (Some(high_value_from_ctx), true) => Bound::Included(high_value_from_ctx),
          (Some(high_value_from_ctx), false) => Bound::Excluded(high_value_from_ctx)
        }          
      },
      Some(high_value) => {
        if self.high_inclusive {
          Bound::Included(high_value.clone())
        }
        else {
          Bound::Excluded(high_value.clone())
        }
      }
    }
  }

  /// Identify the type of items that may be tested against this range.
  pub fn get_bounds_type<C: ContextReader>(&self, contexts: &C) -> FeelType {
    match (self.start_bound(contexts), self.end_bound(contexts)) {
      (Bound::Included(b), _) => b.get_type(),
      (Bound::Excluded(b), _) => b.get_type(),
      (_, Bound::Included(b)) => b.get_type(),
      (_, Bound::Excluded(b)) => b.get_type(),
      _ => FeelType::Null // Should never happen
    }
  }

  pub fn satisfies_low<C: ContextReader>(&self, value: &FeelValue, contexts: &C) -> bool {
    match &*(self.low) {
      None => true,
      Some(FeelValue::Name(qname)) => {
        let low_from_context_opt = contexts.get(qname.clone());
        match (low_from_context_opt, self.low_inclusive) {
          (None, _) => false,
          (Some(low_value_from_ctx), true) => low_value_from_ctx <= *value,
          (Some(low_value_from_ctx), false) => low_value_from_ctx < *value
        }          
      },
      Some(low_value) => {
        if self.low_inclusive {
          low_value <= value
        }
        else {
          low_value < value
        }
      }
    }
  }

  pub fn satisfies_high<C: ContextReader>(&self, value: &FeelValue, contexts: &C) -> bool {
    match &*(self.high) {
      None => true,
      Some(FeelValue::Name(qname)) => {
        let high_from_context_opt = contexts.get(qname.clone());
        match (high_from_context_opt, self.high_inclusive) {
          (None, _) => false,
          (Some(high_value_from_ctx), true) => high_value_from_ctx >= *value,
          (Some(high_value_from_ctx), false) => high_value_from_ctx > *value
        }          
      },
      Some(high_value) => {
        if self.high_inclusive {
          high_value >= value
        }
        else {
          high_value > value
        }
      }
    }
  }

  /// Tests if the value falls within the Range, possibly looking
  /// up the low or high value from a NestedContext.
  pub fn includes<C: ContextReader>(&self, value: &FeelValue, contexts: &C) -> bool {
    self.satisfies_low(value, contexts) && self.satisfies_high(value, contexts)
  }

  /// Compare the value to the range and decide if it falls before the range starts (Less),
  /// inside the range (Equal), or after the range ends (Greater).
  pub fn compare<C: ContextReader>(&self, value: &FeelValue, contexts: &C) -> Ordering {
    match(self.satisfies_low(value, contexts), self.satisfies_high(value, contexts)) {
      (false, _) => Ordering::Less,
      (true, true) => Ordering::Equal,
      _ => Ordering::Greater
    }
  }

}

impl Debug for Range {
  fn fmt(&self, f: &mut Formatter) -> Result {
    let left_bracket = if self.low_inclusive { "[" } else { "(" };
    let right_bracket = if self.high_inclusive { "]" } else { ")" };
    let lt_operator = if self.high_inclusive { "<=" } else { "<" };
    let gt_operator = if self.low_inclusive { ">=" } else { ">" };

    match (&*self.low, &*self.high) {
      (None, None) =>  write!(f, "{},{}", left_bracket, right_bracket),
      (None, Some(value)) =>  write!(f, "{} {:?}", lt_operator, value),
      (Some(value), None) =>  write!(f, "{} {:?}", gt_operator, value),
      (Some(low), Some(high)) =>  write!(f, "{}{:?},{:?}{}", left_bracket, low, high, right_bracket)
    }
  }
}

impl Display for Range {
  fn fmt(&self, f: &mut Formatter) -> Result {
      write!(f, "{:?}", self)
  }
}

impl PartialOrd for Range {
  /// Order the ranges first by the low, then by the high limit. 
  /// Assume low unbounded ranges come earliest.
  /// If two ranges have the same bound but one is included and the other excluded,
  /// for the low bound the included comes before the excluded,
  /// but for the high bound it is the reverse. 
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    let low_cmp = match (&*self.low, &*other.low) {
      (None, None) => Ordering::Equal, // Inclusivity is ignored for open-ended ranges
      (None, _) => Ordering::Less,
      (_, None) => Ordering::Greater,
      (Some(a), Some(b)) => { 
        match a.cmp(&b) {
          Ordering::Less => Ordering::Less,
          Ordering::Greater => Ordering::Greater,
          Ordering::Equal => {
            match (self.low_inclusive, other.low_inclusive) {
              (true, false) => Ordering::Less,
              (false, true) => Ordering::Greater,
              _ => Ordering::Equal
            }
          }
        }
      }
    };
    if low_cmp != Ordering::Equal {
      return Some(low_cmp);
    }

    let high_cmp = match (&*self.high, &*other.high) {
      (None, None) => match (self.high_inclusive, other.high_inclusive) {
          (true, false) => Ordering::Greater,
          (false,true) => Ordering::Less,
          _ => Ordering::Equal
        },
      (None, _) => Ordering::Greater,
      (_, None) => Ordering::Less,
      (Some(a), Some(b)) => { 
        match a.cmp(&b) {
          Ordering::Less => Ordering::Less,
          Ordering::Greater => Ordering::Greater,
          Ordering::Equal => {
            match (self.high_inclusive, other.high_inclusive) {
              (true, false) => Ordering::Greater,
              (false, true) => Ordering::Less,
              _ => Ordering::Equal
            }
          }
        }
      }
    };
    return Some(high_cmp);
  }
}

impl Ord for Range {
  fn cmp(&self, other: &Self) -> Ordering {
      self.partial_cmp(other).unwrap()
  }
}

impl<R: RangeBounds<f64>> From<R> for Range {
  fn from(r: R) -> Self {
    let f = |value: &f64| FeelValue::Number(*value);
    match (r.start_bound(), r.end_bound()) {
      (Bound::Included(low), Bound::Included(high)) 
        => Range::new(&f(low), &f(high), true, true),
      (Bound::Included(low), Bound::Excluded(high)) 
        => Range::new(&f(low), &f(high), true, false),
      (Bound::Excluded(low), Bound::Included(high)) 
        => Range::new(&f(low), &f(high), false, true),
      (Bound::Excluded(low), Bound::Excluded(high)) 
        => Range::new(&f(low), &f(high), false, false),
      (Bound::Included(low), Bound::Unbounded)
        => Range::new_with_low(&f(low), true),
      (Bound::Excluded(low), Bound::Unbounded) 
        => Range::new_with_low(&f(low), false),
      (Bound::Unbounded, Bound::Included(high)) 
        => Range::new_with_high(&f(high), true),
      (Bound::Unbounded, Bound::Excluded(high)) 
        => Range::new_with_high(&f(high), false),
      (Bound::Unbounded, Bound::Unbounded) 
        => panic!("Full range is not supported")
    }
  }
}

/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
  use std::cmp::{Ord, PartialOrd, Ordering};
  use std::ops::Range as OpsRange;
  use super::super::feel_value::{FeelValue};
  use super::super::context::{Context};
  use super::Range;

  fn make_test_data() -> Context {
    let ctx1 = Context::new();
    ctx1.insert("zero", 0.into());
    ctx1.insert("passing", 65.into());
    ctx1.insert("A-", 91.into());
    ctx1.insert("A", 94.into());
    ctx1.insert("A+", 97.into());
    ctx1.insert("B", 81.into());
    ctx1.insert("C", 71.into());
    ctx1.insert("perfect", 100.into());
    ctx1
  }

  #[test]
  fn test_equals() {
    let r1:Range = (5.0..10.0).into();
    let r2:Range = (5.0..10.0).into();
    let r3:Range = (5.0..=10.0).into();
    let r4:Range = (..10.0).into();
    let r5:Range = (..10.0).into();
    let r6:Range = (..=10.0).into();

    assert_eq!(&r1, &r2, "equal ranges");
    assert_ne!(&r1, &r3, "unequal ranges due to inclusive-exclusive mismatch");
    assert_eq!(&r4, &r5, "equal open-ended ranges");
    assert_ne!(&r4, &r6, "unequal open-ended ranges");
  }

  #[test]
  fn test_inclusive_range_using_context() {
    let ctx = make_test_data();
    let failing_range = Range::new(&FeelValue::Name("zero".into()), &FeelValue::Name("passing".into()), true, false);
    let grade_50: FeelValue = 50.into();
    let grade_92: FeelValue = 92.into();

    assert_eq!(true, failing_range.includes(&grade_50, &ctx), "failing grade in failing range");
    assert_eq!(false, failing_range.includes(&grade_92, &ctx), "passing grade not in failing range");
  }

  #[test]
  fn test_inclusive_range_ignoring_context() {
    let ctx = make_test_data();
    let failing_range = Range::new(&0.into(), &65.into(), true, false);
    let grade_50: FeelValue = 50.into();
    let grade_92: FeelValue = 92.into();

    assert_eq!(true, failing_range.includes(&grade_50, &ctx), "failing grade in failing range");
    assert_eq!(false, failing_range.includes(&grade_92, &ctx), "passing grade not in failing range");
  }


  #[test]
  fn test_to_string() {
    assert_eq!(
      "[0,65)".to_string(), 
      (&Range::new(&0.into(), &65.into(), true, false)).to_string(), 
      "inclusive exclusive range"
    );

    assert_eq!(
      "(0,65]".to_string(), 
      (&Range::new(&0.into(), &65.into(), false, true)).to_string(), 
      "exclusive inclusive range"
    );

    assert_eq!(
      "< 65".to_string(), 
      (&Range::new_with_high(&65.into(), false)).to_string(), 
      "open exclusive range"
    );

    assert_eq!(
      ">= 10".to_string(), 
      (&Range::new_with_low(&10.into(), true)).to_string(), 
      "inclusive open range"
    );
  }

  #[test]
  fn test_partial_cmp() {
    let r1 = Range::new(&5.into(), &65.into(), true, true);
    let r2 = Range::new(&6.into(), &65.into(), true, true);
    assert_eq!(Ordering::Less, r1.partial_cmp(&r2).unwrap(), "compare ranges differing by low");

    let r3 = Range::new(&5.into(), &64.into(), true, true);
    assert_eq!(Ordering::Greater, r1.partial_cmp(&r3).unwrap(), "compare ranges differing by high");
  }

  #[test]
  fn test_cmp() {
    let r1 = Range::new(&5.into(), &65.into(), true, true);
    let r2 = Range::new(&6.into(), &65.into(), true, true);
    assert_eq!(Ordering::Less, r1.cmp(&r2), "compare ranges differing by low");

    let r3 = Range::new(&5.into(), &64.into(), true, true);
    assert_eq!(Ordering::Greater, r1.cmp(&r3), "compare ranges differing by high");
  }

  #[test]
  fn test_range_conversion() {
    let ctx = Context::new();
    let rust_range: OpsRange<f64> = 10.0..20.0;
    let feel_range: Range = rust_range.into();
    assert_eq!(true, feel_range.includes(& (15.into()), &ctx), "included number");
    assert_eq!(true, feel_range.includes(& (10.into()), &ctx), "included number (inclusive lower bound)");
    assert_eq!(false, feel_range.includes(& (5.into()), &ctx), "excluded number (too low)");
    assert_eq!(false, feel_range.includes(& (25.into()), &ctx), "excluded number (too high)");
    assert_eq!(false, feel_range.includes(& (20.into()), &ctx), "excluded number (exclusive upper bound)");
  }
}

