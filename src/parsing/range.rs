use std::fmt::{Debug,Display,Formatter,Result};
use std::cmp::{Ord, PartialOrd, Ordering};
use super::feel_value::{FeelValue, FeelType};
use super::context::ContextReader;

/// A Feel Range object, that decides whether a value falls within a range of low and high values.
///   - The ends of the range may be inclusive or exclusive. 
///   - The low and high values may be FeelValue::Name values, indicating that they should be looked up
///     from a NestedContext.
///   - The low and high values may be Option::None, indicating an open range in that direction. 
#[derive(PartialEq, Eq, Clone)]
pub struct Range {
  low: Option<FeelValue>,
  high: Option<FeelValue>,
  low_inclusive: bool,
  high_inclusive: bool
}

impl Range {

  /// Create a new Range that has both a low and high value.
  /// Panics if the types of the values do not match, 
  /// except if one or the other value is a Name, which will be used
  /// to lookup the real value in a supplied context.
  pub fn new(low: FeelValue, high: FeelValue, low_inclusive: bool, high_inclusive: bool) -> Self {
    if low.get_type() != high.get_type() && low.get_type() != FeelType::Name && high.get_type() != FeelType::Name {
      panic!("Range low and high values must be of the same type");
    }
    Range {
      low: Option::Some(low),
      high: Option::Some(high),
      low_inclusive: low_inclusive,
      high_inclusive: high_inclusive
    }
  }


  /// Create a new open-ended Range that has a low but no high value. 
  pub fn new_with_low(low: FeelValue, inclusive: bool) -> Self {
    Range {
      low: Option::Some(low),
      high: None,
      low_inclusive: inclusive,
      high_inclusive: true
    }
  }

  /// Create a new open-ended Range that has a high but no low value. 
  pub fn new_with_high(high: FeelValue, inclusive: bool) -> Self {
    Range {
      low: None,
      high: Option::Some(high),
      low_inclusive: true,
      high_inclusive: inclusive
    }
  }

  fn satisfies_low<C: ContextReader>(&self, value: &FeelValue, contexts: &C) -> bool {
    match &self.low {
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

  fn satisfies_high<C: ContextReader>(&self, value: &FeelValue, contexts: &C) -> bool {
    match &self.high {
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
}

impl Debug for Range {
  fn fmt(&self, f: &mut Formatter) -> Result {
    let left_bracket = if self.low_inclusive { "[" } else { "(" };
    let right_bracket = if self.high_inclusive { "]" } else { ")" };

    match (&self.low, &self.high) {
      (None, None) =>  write!(f, "{},{}", left_bracket, right_bracket),
      (None, Some(value)) =>  write!(f, "{},{:?}{}", left_bracket, value, right_bracket),
      (Some(value), None) =>  write!(f, "{}{:?},{}", left_bracket, value, right_bracket),
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
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    let low_cmp = match (&self.low, &other.low) {
      (None, None) => match (self.low_inclusive, other.low_inclusive) {
          (true, false) => Ordering::Less,
          (false,true) => Ordering::Greater,
          _ => Ordering::Equal
        },
      (None, _) => Ordering::Less,
      (_, None) => Ordering::Greater,
      (Some(a), Some(b)) => a.cmp(&b)
    };
    if low_cmp != Ordering::Equal {
      return Some(low_cmp);
    }

    let high_cmp = match (&self.high, &other.high) {
      (None, None) => match (self.high_inclusive, other.high_inclusive) {
          (true, false) => Ordering::Greater,
          (false,true) => Ordering::Less,
          _ => Ordering::Equal
        },
      (None, _) => Ordering::Greater,
      (_, None) => Ordering::Less,
      (Some(a), Some(b)) => a.cmp(&b)
    };
    return Some(high_cmp);
  }
}

impl Ord for Range {
  fn cmp(&self, other: &Self) -> Ordering {
      self.partial_cmp(other).unwrap()
  }
}

/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
  use std::cmp::{Ord, PartialOrd, Ordering};
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
  fn test_inclusive_range_using_context() {
    let ctx = make_test_data();
    let failing_range = Range::new(FeelValue::Name("zero".into()), FeelValue::Name("passing".into()), true, false);
    let grade_50: FeelValue = 50.into();
    let grade_92: FeelValue = 92.into();

    assert_eq!(true, failing_range.includes(&grade_50, &ctx), "failing grade in failing range");
    assert_eq!(false, failing_range.includes(&grade_92, &ctx), "passing grade not in failing range");
  }

  #[test]
  fn test_inclusive_range_ignoring_context() {
    let ctx = make_test_data();
    let failing_range = Range::new(0.into(), 65.into(), true, false);
    let grade_50: FeelValue = 50.into();
    let grade_92: FeelValue = 92.into();

    assert_eq!(true, failing_range.includes(&grade_50, &ctx), "failing grade in failing range");
    assert_eq!(false, failing_range.includes(&grade_92, &ctx), "passing grade not in failing range");
  }


  #[test]
  fn test_to_string() {
    assert_eq!(
      "[0,65)".to_string(), 
      (&Range::new(0.into(), 65.into(), true, false)).to_string(), 
      "inclusive exclusive range"
    );

    assert_eq!(
      "(0,65]".to_string(), 
      (&Range::new(0.into(), 65.into(), false, true)).to_string(), 
      "exclusive inclusive range"
    );

    assert_eq!(
      "[,65)".to_string(), 
      (&Range::new_with_high(65.into(), false)).to_string(), 
      "open exclusive range"
    );
  }

  #[test]
  fn test_partial_cmp() {
    let r1 = Range::new(5.into(), 65.into(), true, true);
    let r2 = Range::new(6.into(), 65.into(), true, true);
    assert_eq!(Ordering::Less, r1.partial_cmp(&r2).unwrap(), "compare ranges differing by low");

    let r3 = Range::new(5.into(), 64.into(), true, true);
    assert_eq!(Ordering::Greater, r1.partial_cmp(&r3).unwrap(), "compare ranges differing by high");
  }

  #[test]
  fn test_cmp() {
    let r1 = Range::new(5.into(), 65.into(), true, true);
    let r2 = Range::new(6.into(), 65.into(), true, true);
    assert_eq!(Ordering::Less, r1.cmp(&r2), "compare ranges differing by low");

    let r3 = Range::new(5.into(), 64.into(), true, true);
    assert_eq!(Ordering::Greater, r1.cmp(&r3), "compare ranges differing by high");
  }
}

