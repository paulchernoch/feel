use std::rc::Rc;
use std::cmp::Ordering;
use std::ops::Bound;
use super::range::Range;
use super::context::{Context,ContextReader};
use super::feel_value::{FeelValue, FeelType};
// use super::execution_log::ExecutionLog;
use super::arguments::{Arguments,Validity};

pub enum RangeCase {
  PointRange,
  PointPoint,
  RangePoint,
  RangeRange
}

/// A Factory for creating a Context containing all the builtin functions needed by the Feel Language.
/// 
/// In Feel, builtins take one to four arguments (or more, for functions like sum, product, min, and max). 
/// These FeelFunctions will all take two arguments: 
///    - parameters: an Arguments object that holds the argument list
///      that conforms to the Feel specification. 
///      If named arguments are used, this may be a context,
///      which names the supplied parameter values.
///    - context: the currently in scope context, available in case the
///      function needs global context to perform its work. 
/// 
pub struct Builtins {}

impl Builtins {
  /// Create a Context containing all the builtin functions needed by the Feel Language.
  /// 
  /// The keys in the Context are the names of the functions (as QNames).
  /// All the values are FeelValue::Function variants.
  pub fn new_context() -> Context {
    let builtin_context = Context::new();

    builtin_context
  }

  //// ///////////// Range functions /////////////////

  // The Range Builtin functions are inspired by HL7 CQL 1.4 (Clinical Query Language).
  // Arguments may be points or Ranges, where a Point (normally a Number or a date type) 
  // may be any FeelType but Null, Range, Context, List or Function.

  /// All the range functions take two parameters in Feel 
  /// (packaged here into a single FeelValue::List), but
  /// the behavior varies based on the types of the parameters, which may be Ranges
  /// or points, where a point is a Number, Date, or one of several other permitted types.
  fn get_range_case(a: FeelType, b: FeelType) -> Option<RangeCase> {
    match(Range::is_suitable_as_point(a), Range::is_suitable_as_point(b), 
          a == FeelType::Range, b == FeelType::Range) {
      (true, true, _, _) => Some(RangeCase::PointPoint),
      (true, false, _, true) => Some(RangeCase::PointRange),
      (false, true, true, _) => Some(RangeCase::RangePoint),
      (false, false, true, true) => Some(RangeCase::RangeRange),
      _ => None
    }
  }

  fn make_validator(function_name: &str, parameters: FeelValue) -> Validity {
    let args: Rc<Arguments> = Rc::new(parameters.into());
    let name: Rc<String> = Rc::new(function_name.into());
    Validity::new(&name, &args)
  }


  fn before_helper<C: ContextReader>(function_name: &str, parameters: FeelValue, contexts: &C) -> FeelValue {
    match Builtins::make_validator(function_name, parameters)
      .arity(2..3)
      .no_nulls()
      .point_or_range(true, true, true, true)
      .validated() {
      Ok(arguments) => {
        match Builtins::get_range_case(arguments[0].get_type(), arguments[1].get_type()) {
          Some(RangeCase::PointPoint) => (arguments[0] < arguments[1]).into(),
          Some(RangeCase::PointRange) => {
            match arguments[1].try_range_compare(&arguments[0], contexts) {
              Some(Ordering::Less) => true.into(),
              _ => false.into()
            }
          },
          Some(RangeCase::RangePoint) => {
            match arguments[0].try_range_compare(&arguments[1], contexts) {
              Some(Ordering::Greater) => true.into(),
              _ => false.into()
            }
          },
          Some(RangeCase::RangeRange) => {
            match (&arguments[0], &arguments[1]) {
              (FeelValue::Range(a), FeelValue::Range(b)) => {
                match (a.end_bound(contexts), b.start_bound(contexts)) {
                  (Bound::Unbounded, _) => false.into(),
                  (_, Bound::Unbounded) => false.into(),
                  (Bound::Excluded(a_end), Bound::Excluded(b_start)) => (a_end <= b_start).into(),
                  (Bound::Included(a_end), Bound::Excluded(b_start)) => (a_end <= b_start).into(),
                  (Bound::Excluded(a_end), Bound::Included(b_start)) => (a_end <= b_start).into(),
                  (Bound::Included(a_end), Bound::Included(b_start)) => (a_end < b_start).into()
                }
              },
              _ => unreachable!() 
            }
          },
          None => FeelValue::Null
        }
      },
      Err(_) => FeelValue::Null
    }
  }

  fn reverse_parameters(parameters: FeelValue) -> FeelValue {
    match parameters {
      FeelValue::List(list) => {
        let reversed: Vec<FeelValue> = list.borrow().iter().cloned().rev().collect();
        return FeelValue::new_list(
          reversed
        );
      },
      _ => {
        return parameters;
      }
    };
  }

  /// before: Does first argument fall before the second with no overlap?
  pub fn before<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    Builtins::before_helper("before", parameters, contexts)
  }

  /// after: Does first argument fall after the second with no overlap?
  pub fn after<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    // before and after are symmetric; just reverse the argument order
    let parameters_reversed = Builtins::reverse_parameters(parameters);
    return Builtins::before_helper("after", parameters_reversed, contexts);
  }

  fn meets_helper<C: ContextReader>(function_name: &str, parameters: FeelValue, contexts: &C) -> FeelValue {
    match Builtins::make_validator(function_name, parameters)
      .arity(2..3)
      .no_nulls()
      .point_or_range(false, true, false, true)
      .validated() {
      Ok(arguments) => {
        match (&arguments[0], &arguments[1]) {
          (FeelValue::Range(a), FeelValue::Range(b)) => {
            match (a.end_bound(contexts), b.start_bound(contexts)) {
              (Bound::Included(a_end), Bound::Included(b_start)) => (a_end == b_start).into(),
              _ => false.into()
            }
          },
          _ => unreachable!() 
        }
      },
      Err(_) => FeelValue::Null
    }
  }

  /// first range meets the second, with its end included and equaling the start of the next (also included)
  pub fn meets<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    Builtins::meets_helper("meets", parameters, contexts)
  }
  
  /// first range is met by the second, with its start included and equaling the end of the next (also included)
  pub fn met_by<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    // meets and met_by are symmetric; just reverse the argument order
    let parameters_reversed = Builtins::reverse_parameters(parameters);
    Builtins::meets_helper("met by", parameters_reversed, contexts)
  }

  // overlaps_before

  // overlaps_after

  fn finishes_helper<C: ContextReader>(function_name: &str, parameters: FeelValue, contexts: &C) -> FeelValue {
    match Builtins::make_validator(function_name, parameters)
      .arity(2..3)
      .no_nulls()
      .point_or_range(true, true, false, true)
      .validated() {
      Ok(arguments) => {
        match (&arguments[0], &arguments[1]) {
          // Range-Range case
          (FeelValue::Range(a), FeelValue::Range(b)) => {
            let end_match = match (a.end_bound(contexts), b.end_bound(contexts)) {
              (Bound::Included(a_end), Bound::Included(b_end)) => a_end == b_end,
              (Bound::Excluded(a_end), Bound::Excluded(b_end)) => a_end == b_end,
              _ => false
            };
            if !end_match { return false.into(); }
            let start_match = match (a.start_bound(contexts), b.start_bound(contexts)) {
              (Bound::Included(a_start), Bound::Included(b_start)) => a_start >= b_start,
              (Bound::Excluded(a_start), Bound::Excluded(b_start)) => a_start >= b_start,
              (Bound::Included(a_start), Bound::Excluded(b_start)) => a_start > b_start,
              (Bound::Excluded(a_start), Bound::Included(b_start)) => a_start >= b_start,
              (Bound::Unbounded, _) => false,
              (_, Bound::Unbounded) => true
            };
            start_match.into()
          },
          // Point-Range Case
          (a, FeelValue::Range(b)) => {
            match b.end_bound(contexts) {
              Bound::Included(b_end) => (*a == b_end).into(),
              _ => false.into()
            }
          },
          _ => unreachable!() 
        }
      },
      Err(_) => FeelValue::Null
    }
  }

  /// One point or range finishes the other, being a subset which matches the 
  /// end bound of the second argument, which must be a range.
  pub fn finishes<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    Builtins::finishes_helper("finishes", parameters, contexts)
  }

  /// finished_by is symmetric with finishes; just swap parameters.
  pub fn finished_by<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    let parameters_reversed = Builtins::reverse_parameters(parameters);
    Builtins::finishes_helper("finished by", parameters_reversed, contexts)
  }

  fn includes_helper<C: ContextReader>(function_name: &str, parameters: FeelValue, contexts: &C) -> FeelValue {
    match Builtins::make_validator(function_name, parameters)
      .arity(2..3)
      .no_nulls()
      .point_or_range(false, true, true, true)
      .validated() {
      Ok(arguments) => {
        match (&arguments[0], &arguments[1]) {
          // Range-Range case
          (FeelValue::Range(a), FeelValue::Range(b)) => {
            let includes_start = match b.start_bound(contexts) {
              Bound::Included(b_start) => a.includes(&b_start, contexts),
              Bound::Excluded(b_start) => {
                match a.start_bound(contexts) {
                  Bound::Included(a_start) => a_start <= b_start,
                  Bound::Excluded(a_start) => a_start <= b_start,
                  Bound::Unbounded => true
                }
              },
              // Undefined behavior. 
              // If both ranges have no lower bound, the spec is unclear. 
              // We return false.
              Bound::Unbounded => false
            };
            if !includes_start { return false.into() }

            let includes_end = match b.end_bound(contexts) {
              Bound::Included(b_end) => a.includes(&b_end, contexts),
              Bound::Excluded(b_end) => {
                match a.end_bound(contexts) {
                  Bound::Included(a_end) => a_end >= b_end,
                  Bound::Excluded(a_end) => a_end >= b_end,
                  Bound::Unbounded => true
                }
              },
              // Undefined behavior. 
              // If both ranges have no upper bound, the spec is unclear. 
              // We return false.
              Bound::Unbounded => false
            };
            includes_end.into()
          },
          // Range-Point Case
          (FeelValue::Range(a), b) => a.includes(b, contexts).into(),
          _ => unreachable!() 
        }
      },
      Err(_) => FeelValue::Null
    }
  }

  /// The first range includes the second argument (a point or range).
  /// If the second is a range, all items in the second range are included by the first.
  pub fn includes<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    Builtins::includes_helper("includes", parameters, contexts)
  }

  /// First argument (a point or range) falls during the second range 
  /// (is completely included by it).
  /// This is symmetric with includes if you swap arguments.
  pub fn during<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    let parameters_reversed = Builtins::reverse_parameters(parameters);
    Builtins::includes_helper("during", parameters_reversed, contexts)
  }

  fn starts_helper<C: ContextReader>(function_name: &str, parameters: FeelValue, contexts: &C) -> FeelValue {
    match Builtins::make_validator(function_name, parameters)
      .arity(2..3)
      .no_nulls()
      .point_or_range(true, true, false, true)
      .validated() {
      Ok(arguments) => {
        match (&arguments[0], &arguments[1]) {
          // Range-Range case
          (FeelValue::Range(a), FeelValue::Range(b)) => {
            let start_match = match (a.start_bound(contexts), b.start_bound(contexts)) {
              (Bound::Included(a_start), Bound::Included(b_start)) => a_start == b_start,
              (Bound::Excluded(a_start), Bound::Excluded(b_start)) => a_start == b_start,
              _ => false
            };
            if !start_match { return false.into(); }
            let end_match = match (a.end_bound(contexts), b.end_bound(contexts)) {
              (Bound::Included(a_end), Bound::Included(b_end)) => a_end <= b_end,
              (Bound::Excluded(a_end), Bound::Excluded(b_end)) => a_end <= b_end,
              (Bound::Included(a_end), Bound::Excluded(b_end)) => a_end < b_end,
              (Bound::Excluded(a_end), Bound::Included(b_end)) => a_end <= b_end,
              (Bound::Unbounded, _) => false,
              (_, Bound::Unbounded) => true
            };
            end_match.into()
          },
          // Point-Range Case
          (a, FeelValue::Range(b)) => {
            match b.start_bound(contexts) {
              Bound::Included(b_start) => (*a == b_start).into(),
              _ => false.into()
            }
          },
          _ => unreachable!() 
        }
      },
      Err(_) => FeelValue::Null
    }
  }

  /// One point or range starts the other, being a subset which matches the 
  /// start bound of the second argument, which must be a range.
  pub fn starts<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    Builtins::starts_helper("starts", parameters, contexts)
  }

  /// started_by is symmetric with starts; just swap parameters.
  pub fn started_by<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    let parameters_reversed = Builtins::reverse_parameters(parameters);
    Builtins::starts_helper("started by", parameters_reversed, contexts)
  }

  /// The ranges coincide, hence are equal.
  pub fn coincides<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    match Builtins::make_validator("coincides", parameters)
      .arity(2..3)
      .no_nulls()
      .point_or_range(true, true, true, true)
      .same_types() // Only permit point-point or range-range
      .validated() {
      Ok(arguments) => {
        match (&arguments[0], &arguments[1]) {
          (FeelValue::Range(a), FeelValue::Range(b)) => {
            // We could use the equals operator except for the case when one or both
            // of the ranges refers to a context value for its bounds.
            (    a.start_bound(contexts) == b.start_bound(contexts)
              && a.end_bound(contexts)   == b.end_bound(contexts)).into()
          },
          _ => {
            (arguments[0] == arguments[1]).into()
          } 
        }
      },
      Err(_) => FeelValue::Null
    }
  }

  //// ///////////// END Range functions /////////////////

}



/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
  use super::super::feel_value::{FeelValue};
  use super::super::context::{Context};
  use std::ops::{RangeBounds};
  use super::super::range::Range;
  use super::Builtins;
  use super::super::exclusive_inclusive_range::ExclusiveInclusiveRange;
  use super::super::exclusive_range::ExclusiveRange;

  fn pt_rng<R: RangeBounds<f64>>(a: i32, b: R) -> FeelValue {
    let range: Range = b.into();
    FeelValue::new_list(vec![FeelValue::Number(a as f64), FeelValue::Range(range)])
  }

  fn rng_pt<R: RangeBounds<f64>>(a: R, b: i32) -> FeelValue {
    let range: Range = a.into();
    FeelValue::new_list(vec![FeelValue::Range(range), FeelValue::Number(b as f64)])
  }

  fn pt_pt(a: i32, b: i32) -> FeelValue {
    FeelValue::new_list(vec![FeelValue::Number(a as f64), FeelValue::Number(b as f64)])
  }

  fn rng_rng<R1: RangeBounds<f64>, R2: RangeBounds<f64>>(a: R1, b: R2) -> FeelValue {
    let range_a: Range = a.into();
    let range_b: Range = b.into();
    FeelValue::new_list(vec![FeelValue::Range(range_a), FeelValue::Range(range_b)])
  }

  /// Test the "before" examples given in Table 78 of the Spec.
  #[test]
  fn test_before() {
    let ctx = Context::new();
    assert!(Builtins::before(pt_pt(1, 10), &ctx).is_true(), "case 1");
    assert!(Builtins::before(pt_pt(10, 1), &ctx).is_false(), "case 2");
    assert!(Builtins::before(pt_rng(1, 1.0..=10.0), &ctx).is_false(), "case 3");
    let r_1_to_10 = ExclusiveInclusiveRange { start: &1.0_f64, end: &10.0_f64 };
    assert!(Builtins::before(pt_rng(1, r_1_to_10), &ctx).is_true(), "case 4"); 
    assert!(Builtins::before(pt_rng(1, 5.0..=10.0), &ctx).is_true(), "case 5");
    assert!(Builtins::before(rng_pt(1.0..=10.0, 10), &ctx).is_false(), "case 6");
    assert!(Builtins::before(rng_pt(1.0..10.0, 10), &ctx).is_true(), "case 7");
    assert!(Builtins::before(rng_pt(1.0..10.0, 15), &ctx).is_true(), "case 8");
    assert!(Builtins::before(rng_rng(1.0..=10.0, 15.0..=20.0), &ctx).is_true(), "case 9");
    assert!(Builtins::before(rng_rng(1.0..=10.0, 10.0..=20.0), &ctx).is_false(), "case 10");
    assert!(Builtins::before(rng_rng(1.0..10.0, 10.0..=20.0), &ctx).is_true(), "case 11");
    let r_10_to_20 = ExclusiveInclusiveRange { start: &10.0_f64, end: &20.0_f64 };
    assert!(Builtins::before(rng_rng(1.0..=10.0, r_10_to_20), &ctx).is_true(), "case 12"); 
  }

  /// Test the "after" examples given in Table 78 of the Spec.
  #[test]
  fn test_after() {
    let ctx = Context::new();
    let r_11_to_20 = ExclusiveInclusiveRange { start: &11.0_f64, end: &20.0_f64 };

    assert!(Builtins::after(pt_pt(10, 5), &ctx).is_true(), "case 1");
    assert!(Builtins::after(pt_pt(5, 10), &ctx).is_false(), "case 2");
    assert!(Builtins::after(pt_rng(12, 1.0..=10.0), &ctx).is_true(), "case 3");
    assert!(Builtins::after(pt_rng(10, 1.0..10.0), &ctx).is_true(), "case 4"); 
    assert!(Builtins::after(pt_rng(10, 1.0..=10.0), &ctx).is_false(), "case 5");
    assert!(Builtins::after(rng_pt(11.0..=20.0, 12), &ctx).is_false(), "case 6");
    assert!(Builtins::after(rng_pt(11.0..=20.0, 10), &ctx).is_true(), "case 7");
    assert!(Builtins::after(rng_pt(r_11_to_20, 11), &ctx).is_true(), "case 8");
    assert!(Builtins::after(rng_pt(11.0..=20.0, 11), &ctx).is_false(), "case 9");
    assert!(Builtins::after(rng_rng(11.0..=20.0, 1.0..=10.0), &ctx).is_true(), "case 10");
    assert!(Builtins::after(rng_rng(1.0..=10.0, 11.0..=20.0), &ctx).is_false(), "case 11");
    assert!(Builtins::after(rng_rng(11.0..=20.0, 1.0..11.0), &ctx).is_true(), "case 12"); 
    assert!(Builtins::after(rng_rng(r_11_to_20, 1.0..=11.0), &ctx).is_true(), "case 13"); 
  }

  /// Test the "meets" examples given in Table 78 of the Spec.
  #[test]
  fn test_meets() {
    let ctx = Context::new();
    let r_5_to_10 = ExclusiveInclusiveRange { start: &5.0_f64, end: &10.0_f64 };

    assert!(Builtins::meets(rng_rng(1.0..=5.0, 5.0..=10.0), &ctx).is_true(), "case 1");
    assert!(Builtins::meets(rng_rng(1.0..5.0, 5.0..=10.0), &ctx).is_false(), "case 2");
    assert!(Builtins::meets(rng_rng(1.0..=5.0, r_5_to_10), &ctx).is_false(), "case 3"); 
    assert!(Builtins::meets(rng_rng(1.0..=5.0, 6.0..=10.0), &ctx).is_false(), "case 4"); 
  }

  /// Test the "met by" examples given in Table 78 of the Spec.
  #[test]
  fn test_met_by() {
    let ctx = Context::new();
    let r_5_to_10 = ExclusiveInclusiveRange { start: &5.0_f64, end: &10.0_f64 };

    assert!(Builtins::met_by(rng_rng(5.0..=10.0, 1.0..=5.0), &ctx).is_true(), "case 1");
    assert!(Builtins::met_by(rng_rng(5.0..=10.0, 1.0..5.0), &ctx).is_false(), "case 2");
    assert!(Builtins::met_by(rng_rng(r_5_to_10, 1.0..=5.0), &ctx).is_false(), "case 3"); 
    assert!(Builtins::met_by(rng_rng(6.0..=10.0, 1.0..=5.0), &ctx).is_false(), "case 4"); 
  }

  /// Test the "finishes" examples given in Table 78 of the Spec.
  #[test]
  fn test_finishes() {
    let ctx = Context::new();
    let r_1_to_10xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &10.0_f64 };

    assert!(Builtins::finishes(pt_rng(10, 1.0..=10.0), &ctx).is_true(), "case 1");
    assert!(Builtins::finishes(pt_rng(10, 1.0..10.0), &ctx).is_false(), "case 2");
    assert!(Builtins::finishes(rng_rng(5.0..=10.0, 1.0..=10.0), &ctx).is_true(), "case 3");
    assert!(Builtins::finishes(rng_rng(5.0..10.0, 1.0..=10.0), &ctx).is_false(), "case 4"); 
    assert!(Builtins::finishes(rng_rng(5.0..10.0, 1.0..10.0), &ctx).is_true(), "case 5");
    assert!(Builtins::finishes(rng_rng(1.0..=10.0, 1.0..=10.0), &ctx).is_true(), "case 6");
    assert!(Builtins::finishes(rng_rng(r_1_to_10xi, 1.0..=10.0), &ctx).is_true(), "case 7"); 
  }

  /// Test the "finished by" examples given in Table 78 of the Spec.
  #[test]
  fn test_finished_by() {
    let ctx = Context::new();
    let r_1_to_10xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &10.0_f64 };

    assert!(Builtins::finished_by(rng_pt(1.0..=10.0, 10), &ctx).is_true(), "case 1");
    assert!(Builtins::finished_by(rng_pt(1.0..10.0, 10), &ctx).is_false(), "case 2");
    assert!(Builtins::finished_by(rng_rng(1.0..=10.0, 5.0..=10.0), &ctx).is_true(), "case 3");
    assert!(Builtins::finished_by(rng_rng(1.0..=10.0, 5.0..10.0), &ctx).is_false(), "case 4"); 
    assert!(Builtins::finished_by(rng_rng(1.0..10.0, 5.0..10.0), &ctx).is_true(), "case 5");
    assert!(Builtins::finished_by(rng_rng(1.0..=10.0, 1.0..=10.0), &ctx).is_true(), "case 6");
    assert!(Builtins::finished_by(rng_rng(1.0..=10.0, r_1_to_10xi), &ctx).is_true(), "case 7"); 
  }

  /// Test the "includes" examples given in Table 78 of the Spec.
  #[test]
  fn test_includes() {
    let ctx = Context::new();
    let r_1_to_5xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &5.0_f64 };
    let r_1_to_10xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &10.0_f64 };
    let r_1_to_10xx = ExclusiveRange { start: &1.0_f64, end: &10.0_f64 };

    assert!(Builtins::includes(rng_pt(1.0..=10.0, 5), &ctx).is_true(), "case 1");
    assert!(Builtins::includes(rng_pt(1.0..=10.0, 12), &ctx).is_false(), "case 2");
    assert!(Builtins::includes(rng_pt(1.0..=10.0, 1), &ctx).is_true(), "case 3");
    assert!(Builtins::includes(rng_pt(1.0..=10.0, 10), &ctx).is_true(), "case 4"); 
    assert!(Builtins::includes(rng_pt(r_1_to_10xi, 1), &ctx).is_false(), "case 5");
    assert!(Builtins::includes(rng_pt(1.0..10.0, 10), &ctx).is_false(), "case 6");
    assert!(Builtins::includes(rng_rng(1.0..=10.0, 4.0..=6.0), &ctx).is_true(), "case 7");
    assert!(Builtins::includes(rng_rng(1.0..=10.0, 1.0..=5.0), &ctx).is_true(), "case 8");
    assert!(Builtins::includes(rng_rng(r_1_to_10xi, r_1_to_5xi), &ctx).is_true(), "case 9");
    assert!(Builtins::includes(rng_rng(1.0..=10.0, r_1_to_10xx), &ctx).is_true(), "case 10"); 
    assert!(Builtins::includes(rng_rng(1.0..10.0, 5.0..10.0), &ctx).is_true(), "case 11"); 
    assert!(Builtins::includes(rng_rng(1.0..=10.0, 1.0..10.0), &ctx).is_true(), "case 12"); 
    assert!(Builtins::includes(rng_rng(1.0..=10.0, r_1_to_10xi), &ctx).is_true(), "case 13"); 
    assert!(Builtins::includes(rng_rng(1.0..=10.0, 1.0..=10.0), &ctx).is_true(), "case 14"); 
  }

  /// Test the "during" examples given in Table 78 of the Spec.
  #[test]
  fn test_during() {
    let ctx = Context::new();
    let r_1_to_5xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &5.0_f64 };
    let r_1_to_10xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &10.0_f64 };
    let r_1_to_10xx = ExclusiveRange { start: &1.0_f64, end: &10.0_f64 };

    assert!(Builtins::during(pt_rng(5, 1.0..=10.0), &ctx).is_true(), "case 1");
    assert!(Builtins::during(pt_rng(12, 1.0..=10.0), &ctx).is_false(), "case 2");
    assert!(Builtins::during(pt_rng(1, 1.0..=10.0), &ctx).is_true(), "case 3");
    assert!(Builtins::during(pt_rng(10, 1.0..=10.0), &ctx).is_true(), "case 4"); 
    assert!(Builtins::during(pt_rng(1, r_1_to_10xi), &ctx).is_false(), "case 5");
    assert!(Builtins::during(pt_rng(10, 1.0..10.0), &ctx).is_false(), "case 6");
    assert!(Builtins::during(rng_rng(4.0..=6.0, 1.0..=10.0), &ctx).is_true(), "case 7");
    assert!(Builtins::during(rng_rng(1.0..=5.0, 1.0..=10.0), &ctx).is_true(), "case 8");
    assert!(Builtins::during(rng_rng(r_1_to_5xi, r_1_to_10xi), &ctx).is_true(), "case 9");
    assert!(Builtins::during(rng_rng(r_1_to_10xx, 1.0..=10.0), &ctx).is_true(), "case 10"); 
    assert!(Builtins::during(rng_rng(5.0..10.0, 1.0..10.0), &ctx).is_true(), "case 11"); 
    assert!(Builtins::during(rng_rng(1.0..10.0, 1.0..=10.0), &ctx).is_true(), "case 12"); 
    assert!(Builtins::during(rng_rng(r_1_to_10xi, 1.0..=10.0), &ctx).is_true(), "case 13"); 
    assert!(Builtins::during(rng_rng(1.0..=10.0, 1.0..=10.0), &ctx).is_true(), "case 14"); 
  }

  /// Test the "starts" examples given in Table 78 of the Spec.
  #[test]
  fn test_starts() {
    let ctx = Context::new();
    let r_1_to_5xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &5.0_f64 };
    let r_1_to_10xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &10.0_f64 };
    let r_1_to_10xx = ExclusiveRange { start: &1.0_f64, end: &10.0_f64 };

    assert!(Builtins::starts(pt_rng(1, 1.0..=10.0), &ctx).is_true(), "case 1");
    assert!(Builtins::starts(pt_rng(1, r_1_to_10xi), &ctx).is_false(), "case 2");
    assert!(Builtins::starts(pt_rng(2, 1.0..=10.0), &ctx).is_false(), "case 3");
    assert!(Builtins::starts(rng_rng(1.0..=5.0, 1.0..10.0), &ctx).is_true(), "case 4"); 
    assert!(Builtins::starts(rng_rng(r_1_to_5xi, r_1_to_10xi), &ctx).is_true(), "case 5");
    assert!(Builtins::starts(rng_rng(r_1_to_5xi, 1.0..=10.0), &ctx).is_false(), "case 6");
    assert!(Builtins::starts(rng_rng(1.0..=5.0, r_1_to_10xi), &ctx).is_false(), "case 7");
    assert!(Builtins::starts(rng_rng(1.0..10.0, 1.0..=10.0), &ctx).is_true(), "case 8");
    assert!(Builtins::starts(rng_rng(1.0..10.0, 1.0..=10.0), &ctx).is_true(), "case 9");
    assert!(Builtins::starts(rng_rng(r_1_to_10xx, r_1_to_10xi), &ctx).is_true(), "case 10"); 
  }

    /// Test the "started by" examples given in Table 78 of the Spec.
    #[test]
    fn test_started_by() {
      let ctx = Context::new();
      let r_1_to_5xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &5.0_f64 };
      let r_1_to_10xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &10.0_f64 };
      let r_1_to_10xx = ExclusiveRange { start: &1.0_f64, end: &10.0_f64 };
  
      assert!(Builtins::started_by(rng_pt(1.0..=10.0, 1), &ctx).is_true(), "case 1");
      assert!(Builtins::started_by(rng_pt(r_1_to_10xi, 1), &ctx).is_false(), "case 2");
      assert!(Builtins::started_by(rng_pt(1.0..=10.0, 2), &ctx).is_false(), "case 3");
      assert!(Builtins::started_by(rng_rng(1.0..10.0, 1.0..=5.0), &ctx).is_true(), "case 4"); 
      assert!(Builtins::started_by(rng_rng(r_1_to_10xi, r_1_to_5xi), &ctx).is_true(), "case 5");
      assert!(Builtins::started_by(rng_rng(1.0..=10.0, r_1_to_5xi), &ctx).is_false(), "case 6");
      assert!(Builtins::started_by(rng_rng(r_1_to_10xi, 1.0..=5.0), &ctx).is_false(), "case 7");
      assert!(Builtins::started_by(rng_rng(1.0..=10.0, 1.0..10.0), &ctx).is_true(), "case 8");
      assert!(Builtins::started_by(rng_rng(1.0..=10.0, 1.0..10.0), &ctx).is_true(), "case 9");
      assert!(Builtins::started_by(rng_rng(r_1_to_10xi, r_1_to_10xx), &ctx).is_true(), "case 10"); 
    }
   
    /// Test the "coincides" examples given in Table 78 of the Spec.
    #[test]
    fn test_coincides() {
      let ctx = Context::new();
      let r_1_to_5 = ExclusiveRange { start: &1.0_f64, end: &5.0_f64 };
  
      assert!(Builtins::coincides(pt_pt(5, 5), &ctx).is_true(), "case 1");
      assert!(Builtins::coincides(pt_pt(3, 4), &ctx).is_false(), "case 2");
      assert!(Builtins::coincides(rng_rng(1.0..=5.0, 1.0..=5.0), &ctx).is_true(), "case 3"); 
      assert!(Builtins::coincides(rng_rng(r_1_to_5, 1.0..=5.0), &ctx).is_false(), "case 4"); 
      assert!(Builtins::coincides(rng_rng(1.0..=5.0, 2.0..=6.0), &ctx).is_false(), "case 5"); 
    }
}