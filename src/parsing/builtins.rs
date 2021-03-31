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


  pub fn before_helper<C: ContextReader>(function_name: &str, parameters: FeelValue, contexts: &C) -> FeelValue {
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

  // meets
  
  // met_by

  // overlaps_before

  // overlaps_after

  // finishes

  // finished_by

  // includes 

  // during

  // starts

  // started_by

  // coincides

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

  /// Test the "before" Examples given in Table 78 of the Spec.
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

  /// Test the "after" Examples given in Table 78 of the Spec.
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
   
}
