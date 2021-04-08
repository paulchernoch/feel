use std::rc::Rc;
use std::cmp::Ordering;
use std::ops::Bound;
use super::range::Range;
use super::context::{Context,ContextReader};
use super::feel_value::{FeelValue, FeelType};
use super::execution_log::ExecutionLog;
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

  //// ///////////// Numeric functions /////////////////
  
  fn validate_number<S: Into<String>>(n: f64, action: S) -> FeelValue {
    if !n.is_finite() {
      ExecutionLog::log(&format!("{:?} did not yield a finite result, substituting null", action.into()));
      FeelValue::Null
    }
    else {
      FeelValue::Number(n)
    }
  }
  
  // decimal(number, places)


  /// floor(number) returns the largest integer less than or equal to the number.
  pub fn floor<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "floor";
    match Builtins::make_validator(fname, parameters)
      .arity(1..2)
      .no_nulls()
      .expect_type(0_usize, FeelType::Number, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::Number(value) => Builtins::validate_number(value.floor(), format!("{:?}({:?})", fname, value)),
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// ceiling(number) returns the smallest integer greater than or equal to the number.
  pub fn ceiling<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "ceiling";
    match Builtins::make_validator(fname, parameters)
      .arity(1..2)
      .no_nulls()
      .expect_type(0_usize, FeelType::Number, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::Number(value) => Builtins::validate_number(value.ceil(), format!("{:?}({:?})", fname, value)),
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// abs(number) returns the absolute value of a number, a year month duration or a day time duration.
  pub fn abs<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "abs";
    match Builtins::make_validator(fname, parameters)
      .arity(1..2)
      .no_nulls()
      .expect_type(0_usize, FeelType::Number, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::Number(value) => Builtins::validate_number(value.abs(), format!("{:?}({:?})", fname, value)),
          // TODO: Implement abs for durations
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }


  // modulo(dividend,divisor)

  /// sqrt(number) returns the square root.
  /// Return Null on negative numbers.
  pub fn sqrt<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "sqrt";
    match Builtins::make_validator(fname, parameters)
      .arity(1..2)
      .no_nulls()
      .expect_type(0_usize, FeelType::Number, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::Number(value) => {
            if !value.is_finite() {
              ExecutionLog::log(&format!("Input to {}() is not finite, substituting null", fname));
              FeelValue::Null
            }
            else if *value < 0.0_f64 {
              ExecutionLog::log(&format!("Input to {}() is negative, substituting null", fname));
              FeelValue::Null
            }
            else {
              Builtins::validate_number(value.sqrt(), format!("{:?}({:?})", fname, value))
            }
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// log(number) returns the natural log.
  /// Return Null on non-positive numbers.
  pub fn log<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "log";
    match Builtins::make_validator(fname, parameters)
      .arity(1..2)
      .no_nulls()
      .expect_type(0_usize, FeelType::Number, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::Number(value) => {
            if !value.is_finite() {
              ExecutionLog::log(&format!("Input to {}() is not finite, substituting null", fname));
              FeelValue::Null
            }
            else if *value <= 0.0_f64 {
              ExecutionLog::log(&format!("Input to {}() is not positive, substituting null", fname));
              FeelValue::Null
            }
            else {
              Builtins::validate_number(value.ln(), format!("{:?}({:?})", fname, value))
            }
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// exp(number) returns Euler's number e raised to the given power.
  /// Return Null on Overflow.
  pub fn exp<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "exp";
    match Builtins::make_validator(fname, parameters)
      .arity(1..2)
      .no_nulls()
      .expect_type(0_usize, FeelType::Number, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::Number(value) => Builtins::validate_number(value.exp(), format!("{:?}({:?})", fname, value)),
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// even(number) returns true for integers that are even, false for
  /// odd integers or numbers with a fractional component, and Null for
  /// anything that is not a Number. 
  pub fn even<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    match Builtins::make_validator("even", parameters)
      .arity(1..2)
      .no_nulls()
      .expect_type(0_usize, FeelType::Number, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::Number(value) => {
            if value % 2.0 == 0.0 { true.into() }
            else { false.into() }
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// odd(number) returns true for integers that are odd, false for
  /// even integers or numbers with a fractional component, and Null for
  /// anything that is not a Number. 
  pub fn odd<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    match Builtins::make_validator("odd", parameters)
      .arity(1..2)
      .no_nulls()
      .expect_type(0_usize, FeelType::Number, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::Number(value) => {
            if value % 2.0 == 0.0 { false.into() }
            else if value % 1.0 == 0.0 { true.into() }
            else { false.into() }
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
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

  /// Compare two bounds to see which is less or greater than the other, 
  /// respecting whether they are a lower bound or an upper bound.
  pub fn compare_bounds<T: Ord>(a: &Bound<T>, a_is_lower_bound: bool, b: &Bound<T>, b_is_lower_bound: bool) -> Ordering {
    match (a, a_is_lower_bound, b, b_is_lower_bound) {
      (Bound::Unbounded, true, Bound::Unbounded, true) => Ordering::Equal,
      (Bound::Unbounded, true, _, _) => Ordering::Less,
      (_, _, Bound::Unbounded, true) => Ordering::Greater,
      (Bound::Unbounded, false, Bound::Unbounded, false) => Ordering::Equal,
      (Bound::Unbounded, false, _, _) => Ordering::Greater,
      (_, _, Bound::Unbounded, false) => Ordering::Less,
      (Bound::Included(a_value), _, Bound::Included(b_value), _) => a_value.cmp(b_value),
      (Bound::Excluded(a_value), _, Bound::Excluded(b_value), _) => a_value.cmp(b_value),
      (Bound::Included(a_value), true, Bound::Excluded(b_value), true) => {
        if a_value <= b_value { Ordering::Less } else { Ordering::Greater }
      },
      (Bound::Included(a_value), false, Bound::Excluded(b_value), false) => {
        if a_value >= b_value { Ordering::Greater } else { Ordering::Less }
      },
      (Bound::Included(a_value), true, Bound::Excluded(b_value), false) => {
        if a_value >= b_value { Ordering::Greater } else { Ordering::Less }
      },
      (Bound::Included(a_value), false, Bound::Excluded(b_value), true) => {
        if a_value <= b_value { Ordering::Less } else { Ordering::Greater }
      },

      (Bound::Excluded(a_value), true, Bound::Included(b_value), true) => {
        if a_value < b_value { Ordering::Less } else { Ordering::Greater }
      },
      (Bound::Excluded(a_value), false, Bound::Included(b_value), false) => {
        if a_value > b_value { Ordering::Greater } else { Ordering::Less }
      },
      (Bound::Excluded(a_value), true, Bound::Included(b_value), false) => {
        if a_value >= b_value { Ordering::Greater } else { Ordering::Less }
      },
      (Bound::Excluded(a_value), false, Bound::Included(b_value), true) => {
        if a_value <= b_value { Ordering::Less } else { Ordering::Greater }
      }
    }
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

  /// check if first range overlaps the seconds
  pub fn overlaps<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    match Builtins::make_validator("overlaps", parameters)
      .arity(2..3)
      .no_nulls()
      .point_or_range(false, true, false, true)
      .validated() {
      Ok(arguments) => {
        let (a, b) = (&arguments[0], &arguments[1]);
        match (a, b) {
          (FeelValue::Range(a_range), FeelValue::Range(b_range)) => {
            // Sorting the ranges into which starts the earliest or latest makes the rest of the 
            // comparisons much simpler.
            let range_cmp = a_range.cmp(b_range);
            if range_cmp == Ordering::Equal {
              return true.into();
            }
            let (low, high) = if range_cmp == Ordering::Less { (a_range, b_range) } else { (b_range, a_range) };

            // Had to work out these cases using a truth table - there are 81 cases! 
            // If there is an edge case with an error, this is the place to look first...
            match (low.start_bound(contexts), high.start_bound(contexts), low.end_bound(contexts), high.end_bound(contexts)) {
              (_, Bound::Unbounded, _, _) => true.into(),
              (Bound::Unbounded, _, Bound::Unbounded, _) => true.into(),
              (_, _, Bound::Unbounded, _) => true.into(),
              (_, _, Bound::Included(earliest_end), Bound::Unbounded) => high.includes(&earliest_end, contexts).into(),
              (_, Bound::Excluded(latest_start), Bound::Excluded(earliest_end), Bound::Unbounded) => (latest_start > earliest_end).into(),
              (_, Bound::Included(latest_start), _, _) => low.includes(&latest_start, contexts).into(),
              (_, _, Bound::Included(earliest_end), Bound::Included(latest_end) ) => {
                (high.includes(&earliest_end, contexts) || earliest_end >= latest_end).into()
              },
              (_, _, Bound::Included(earliest_end), Bound::Excluded(latest_end) ) => {
                (high.includes(&earliest_end, contexts) || earliest_end >= latest_end).into()
              },
              (_, Bound::Excluded(latest_start), Bound::Excluded(earliest_end), Bound::Included(_)) => (latest_start < earliest_end).into(),
              (_, Bound::Excluded(latest_start), Bound::Excluded(earliest_end), Bound::Excluded(_)) => (latest_start < earliest_end).into()
            }
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// Checks if first range "overlaps before" the second, meaning that the first range must start before
  /// the second and may not go past the end of the second.
  pub fn overlaps_before<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    match Builtins::make_validator("overlaps before", parameters)
      .arity(2..3)
      .no_nulls()
      .point_or_range(false, true, false, true)
      .validated() {
      Ok(arguments) => {
        let (a, b) = (&arguments[0], &arguments[1]);
        match (a, b) {
          (FeelValue::Range(a_range), FeelValue::Range(b_range)) => {
            let a_start = a_range.start_bound(contexts);
            let b_start = b_range.start_bound(contexts);
            let a_end = a_range.end_bound(contexts);
            let b_end = b_range.end_bound(contexts);
            // We must have a_start < b_start <= a_end <= b_end  
            if Builtins::compare_bounds(&a_start, true, &b_start, true) != Ordering::Less { return false.into(); }
            if Builtins::compare_bounds(&b_start, true, &a_end, false) == Ordering::Greater { return false.into(); }
            if Builtins::compare_bounds(&a_end, false, &b_end, false) == Ordering::Greater { return false.into(); }
            true.into()
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  // overlaps_after

  /// Checks if first range "overlaps after" the second, meaning that the first range must end after
  /// the second and may not go before the start of the second.
  pub fn overlaps_after<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    match Builtins::make_validator("overlaps after", parameters)
      .arity(2..3)
      .no_nulls()
      .point_or_range(false, true, false, true)
      .validated() {
      Ok(arguments) => {
        let (a, b) = (&arguments[0], &arguments[1]);
        match (a, b) {
          (FeelValue::Range(a_range), FeelValue::Range(b_range)) => {
            let a_start = a_range.start_bound(contexts);
            let b_start = b_range.start_bound(contexts);
            let a_end = a_range.end_bound(contexts);
            let b_end = b_range.end_bound(contexts);
            // We must have a_end > b_end >= a_start >= b_start 
            if Builtins::compare_bounds(&a_end, false, &b_end, false) != Ordering::Greater { return false.into(); }
            if Builtins::compare_bounds(&b_end, false, &a_start, true) == Ordering::Less { return false.into(); }
            if Builtins::compare_bounds(&a_start, true, &b_start, true) == Ordering::Less { return false.into(); }
            true.into()
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

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
  use std::ops::{RangeBounds, Bound};
  use std::cmp::Ordering;
  use super::super::range::Range;
  use super::Builtins;
  use super::super::exclusive_inclusive_range::ExclusiveInclusiveRange;
  use super::super::exclusive_range::ExclusiveRange;

  //// Numeric function tests
  
  /// Test if two Numbers are approximately equal, differing by no more than delta.
  fn are_near(a: &FeelValue, b: &FeelValue, delta: f64) -> bool {
    match (a, b) {
      (FeelValue::Number(x), FeelValue::Number(y)) => x.is_finite() && y.is_finite() && (x - y).abs() <= delta,
      _ => false
    }
  }

  /// Tests of floor builtin function from the spec
  #[test]
  fn test_floor() {
    let ctx = Context::new();
    assert!(are_near(&Builtins::floor(1.5.into(), &ctx), &1.0_f64.into(), 0.00000000005_f64), "case 1");
    assert!(are_near(&Builtins::floor((-1.5).into(), &ctx), &(-2.0_f64).into(), 0.00000000005_f64), "case 2");
  }

  /// Tests of ceiling builtin function from the spec
  #[test]
  fn test_ceiling() {
    let ctx = Context::new();
    assert!(are_near(&Builtins::ceiling(1.5.into(), &ctx), &2.0_f64.into(), 0.00000000005_f64), "case 1");
    assert!(are_near(&Builtins::ceiling((-1.5).into(), &ctx), &(-1.0_f64).into(), 0.00000000005_f64), "case 2");
  }

  /// Tests of abs builtin function from the spec
  #[test]
  fn test_abs() {
    let ctx = Context::new();
    assert!(are_near(&Builtins::abs(10.into(), &ctx), &10.0_f64.into(), 0.00000000005_f64), "case 1");
    assert!(are_near(&Builtins::abs((-10.0).into(), &ctx), &10.0_f64.into(), 0.00000000005_f64), "case 2");

    // TODO: Test abs(Duration) 
  }

  /// Tests of sqrt builtin function from the spec, plus more
  #[test]
  fn test_sqrt() {
    let ctx = Context::new();
    assert!(are_near(&Builtins::sqrt(16.into(), &ctx), &4.0_f64.into(), 0.00000000005_f64), "case 1");
    assert!(are_near(&Builtins::sqrt(1.into(), &ctx), &1.0_f64.into(), 0.000000000005_f64), "case 2");
    assert!(are_near(&Builtins::sqrt(0.into(), &ctx), &0.0_f64.into(), 0.000000000005_f64), "case 3");
    assert!(Builtins::sqrt((-1.0).into(), &ctx).is_null(), "case 4");
  }

  /// Tests of log builtin function from the spec, plus more
  #[test]
  fn test_log() {
    let ctx = Context::new();
    assert!(are_near(&Builtins::log(10.into(), &ctx), &2.30258509299_f64.into(), 0.00000000005_f64), "case 1");
    assert!(are_near(&Builtins::log(std::f64::consts::E.into(), &ctx), &1.0_f64.into(), 0.000000000005_f64), "case 2");
    assert!(are_near(&Builtins::log(1.into(), &ctx), &0.0_f64.into(), 0.000000000005_f64), "case 3");
    assert!(Builtins::log(0.0.into(), &ctx).is_null(), "case 4");
    assert!(Builtins::log((-1.0).into(), &ctx).is_null(), "case 5");
  }
  
  /// Tests of exp builtin function from the spec, plus more
  #[test]
  fn test_exp() {
    let ctx = Context::new();
    assert!(are_near(&Builtins::exp(5.into(), &ctx), &148.413159102577_f64.into(), 0.000000000005_f64), "case 1");
    assert!(are_near(&Builtins::exp(0.into(), &ctx), &1.0_f64.into(), 0.000000000005_f64), "case 2");
    assert!(Builtins::exp(99999999.into(), &ctx).is_null(), "case 3");
  }

  /// Tests of even builtin function from the spec, plus more
  #[test]
  fn test_even() {
    let ctx = Context::new();
    assert!(Builtins::even(5.into(), &ctx).is_false(), "case 1");
    assert!(Builtins::even(2.into(), &ctx).is_true(), "case 2");
    assert!(Builtins::even(8.5.into(), &ctx).is_false(), "case 3");
    assert!(Builtins::even("Bad".into(), &ctx).is_null(), "case 4");

    let list = FeelValue::new_list(vec![8.into()]);
    assert!(Builtins::even(list, &ctx).is_true(), "case 5");
  }

  /// Tests of odd builtin function from the spec, plus more
  #[test]
  fn test_odd() {
    let ctx = Context::new();
    assert!(Builtins::odd(5.into(), &ctx).is_true(), "case 1");
    assert!(Builtins::odd(2.into(), &ctx).is_false(), "case 2");
    assert!(Builtins::odd(8.5.into(), &ctx).is_false(), "case 3");
    assert!(Builtins::odd("Bad".into(), &ctx).is_null(), "case 4");
  }

  //// Helper functions for Range tests

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

  //// Range function Tests

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

  /// Test the "overlaps" examples  given in Table 78 of the Spec.
  #[test]
  fn test_overlaps() {
    // TODO: There are 81 different cases possible, but we only test 14.
    // This would be a good place to add more test cases than the spec has.
    let ctx = Context::new();
    let r_5_to_8 = ExclusiveInclusiveRange { start: &5.0_f64, end: &8.0_f64 };

    assert!(Builtins::overlaps(rng_rng(1.0..=5.0, 3.0..=8.0), &ctx).is_true(), "case 1");
    assert!(Builtins::overlaps(rng_rng(3.0..=8.0, 1.0..=5.0), &ctx).is_true(), "case 2");
    assert!(Builtins::overlaps(rng_rng(1.0..=8.0, 3.0..=5.0), &ctx).is_true(), "case 3"); 
    assert!(Builtins::overlaps(rng_rng(3.0..=5.0, 1.0..=8.0), &ctx).is_true(), "case 4"); 
    assert!(Builtins::overlaps(rng_rng(1.0..=5.0, 6.0..=8.0), &ctx).is_false(), "case 5"); 
    assert!(Builtins::overlaps(rng_rng(6.0..=8.0, 1.0..=5.0), &ctx).is_false(), "case 6"); 
    assert!(Builtins::overlaps(rng_rng(1.0..=5.0, 5.0..=8.0), &ctx).is_true(), "case 7"); 
    assert!(Builtins::overlaps(rng_rng(1.0..=5.0, r_5_to_8), &ctx).is_false(), "case 8"); 
    assert!(Builtins::overlaps(rng_rng(1.0..5.0, 5.0..=8.0), &ctx).is_false(), "case 9"); 
    assert!(Builtins::overlaps(rng_rng(1.0..5.0, r_5_to_8), &ctx).is_false(), "case 10"); 
    assert!(Builtins::overlaps(rng_rng(5.0..=8.0, 1.0..=5.0), &ctx).is_true(), "case 11"); 
    assert!(Builtins::overlaps(rng_rng(r_5_to_8, 1.0..=5.0), &ctx).is_false(), "case 12"); 
    assert!(Builtins::overlaps(rng_rng(5.0..=8.0, 1.0..5.0), &ctx).is_false(), "case 13"); 
    assert!(Builtins::overlaps(rng_rng(r_5_to_8, 1.0..5.0), &ctx).is_false(), "case 14"); 
  }

  /// Test the "overlaps before" examples  given in Table 78 of the Spec.
  #[test]
  fn test_overlaps_before() {
    // TODO: There are 81 different cases possible, but we only test 14.
    // This would be a good place to add more test cases than the spec has.
    let ctx = Context::new();
    let r_5_to_8 = ExclusiveInclusiveRange { start: &5.0_f64, end: &8.0_f64 };
    let r_1_to_5 = ExclusiveInclusiveRange { start: &1.0_f64, end: &5.0_f64 };

    assert!(Builtins::overlaps_before(rng_rng(1.0..=5.0, 3.0..=8.0), &ctx).is_true(), "case 1");
    assert!(Builtins::overlaps_before(rng_rng(1.0..=5.0, 6.0..=8.0), &ctx).is_false(), "case 2");
    assert!(Builtins::overlaps_before(rng_rng(1.0..=5.0, 5.0..=8.0), &ctx).is_true(), "case 3"); 
    assert!(Builtins::overlaps_before(rng_rng(1.0..=5.0, r_5_to_8), &ctx).is_false(), "case 4"); 
    assert!(Builtins::overlaps_before(rng_rng(1.0..5.0, 5.0..=8.0), &ctx).is_false(), "case 5"); 
    assert!(Builtins::overlaps_before(rng_rng(1.0..5.0, r_1_to_5), &ctx).is_true(), "case 6"); 
    assert!(Builtins::overlaps_before(rng_rng(1.0..=5.0, r_1_to_5), &ctx).is_true(), "case 7"); 
    assert!(Builtins::overlaps_before(rng_rng(1.0..5.0, 1.0..=5.0), &ctx).is_false(), "case 8"); 
    assert!(Builtins::overlaps_before(rng_rng(1.0..=5.0, 1.0..=5.0), &ctx).is_false(), "case 9"); 
  }

  /// Test the "overlaps after" examples  given in Table 78 of the Spec.
  #[test]
  fn test_overlaps_after() {
    // TODO: There are 81 different cases possible, but we only test 14.
    // This would be a good place to add more test cases than the spec has.
    let ctx = Context::new();
    let r_5_to_8 = ExclusiveInclusiveRange { start: &5.0_f64, end: &8.0_f64 };
    let r_1_to_5 = ExclusiveInclusiveRange { start: &1.0_f64, end: &5.0_f64 };

    assert!(Builtins::overlaps_after(rng_rng(3.0..=8.0, 1.0..=5.0), &ctx).is_true(), "case 1");
    assert!(Builtins::overlaps_after(rng_rng(6.0..=8.0, 1.0..=5.0), &ctx).is_false(), "case 2");
    assert!(Builtins::overlaps_after(rng_rng(5.0..=8.0, 1.0..=5.0), &ctx).is_true(), "case 3"); 
    assert!(Builtins::overlaps_after(rng_rng(r_5_to_8, 1.0..=5.0), &ctx).is_false(), "case 4"); 
    assert!(Builtins::overlaps_after(rng_rng(5.0..=8.0, 1.0..5.0), &ctx).is_false(), "case 5"); 
    assert!(Builtins::overlaps_after(rng_rng(r_1_to_5, 1.0..5.0), &ctx).is_true(), "case 6"); 
    // The following case has a "true" expectation in the DMN Spec, but appears to be incorrect.
    // The first range must end after the second range, not be coterminous. 
    // The spec has a missing clause in its logic and does not explain the semantics in words.
    // Consulted the HL7 CQL documentation for a textual description of the intended semantics. 
    assert!(Builtins::overlaps_after(rng_rng(r_1_to_5, 1.0..=5.0), &ctx).is_false(), "case 7"); 
    // Again, the spec has a wrong answer for this test case as well. 
    assert!(Builtins::overlaps_after(rng_rng(1.0..=5.0, 1.0..5.0), &ctx).is_true(), "case 8"); 
    assert!(Builtins::overlaps_after(rng_rng(1.0..=5.0, 1.0..=5.0), &ctx).is_false(), "case 9"); 
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

    #[test]
    fn test_compare_bounds() {
      compare_bounds_test_case("[!", "[!", Ordering::Equal);
      compare_bounds_test_case("[3", "[3", Ordering::Equal);
      compare_bounds_test_case("(3", "(3", Ordering::Equal);
      compare_bounds_test_case("[3", "3]", Ordering::Equal);
      compare_bounds_test_case("3]", "[3", Ordering::Equal);
      compare_bounds_test_case("3]", "3]", Ordering::Equal);
      compare_bounds_test_case("3)", "3)", Ordering::Equal);
      compare_bounds_test_case("!]", "!]", Ordering::Equal);

      compare_bounds_test_case("[!", "!]", Ordering::Less);
      compare_bounds_test_case("[!", "[3", Ordering::Less);
      compare_bounds_test_case("[!", "(3", Ordering::Less);
      compare_bounds_test_case("[!", "3]", Ordering::Less);
      compare_bounds_test_case("[!", "3)", Ordering::Less);
      compare_bounds_test_case("[3", "[4", Ordering::Less);
      compare_bounds_test_case("[3", "(4", Ordering::Less);
      compare_bounds_test_case("[3", "(3", Ordering::Less);
      compare_bounds_test_case("(3", "[4", Ordering::Less);
      compare_bounds_test_case("(3", "(4", Ordering::Less);
      compare_bounds_test_case("[3", "4]", Ordering::Less);
      compare_bounds_test_case("[3", "4)", Ordering::Less);
      compare_bounds_test_case("(3", "!]", Ordering::Less);
      compare_bounds_test_case("[3", "!]", Ordering::Less);
      compare_bounds_test_case("3]", "4]", Ordering::Less);
      compare_bounds_test_case("3]", "4)", Ordering::Less);
      compare_bounds_test_case("3]", "(3", Ordering::Less);
      compare_bounds_test_case("3)", "[4", Ordering::Less);
      compare_bounds_test_case("3)", "(4", Ordering::Less);
      compare_bounds_test_case("3)", "[3", Ordering::Less); // Failing

      compare_bounds_test_case("!]", "[3", Ordering::Greater);
      compare_bounds_test_case("!]", "(3", Ordering::Greater);
      compare_bounds_test_case("!]", "[!", Ordering::Greater);
      compare_bounds_test_case("[4", "[3", Ordering::Greater);
      compare_bounds_test_case("[4", "(3", Ordering::Greater);
      compare_bounds_test_case("[4", "[!", Ordering::Greater);
      compare_bounds_test_case("(4", "[4", Ordering::Greater);
      compare_bounds_test_case("(4", "(3", Ordering::Greater);
      compare_bounds_test_case("(4", "[3", Ordering::Greater);
      compare_bounds_test_case("4]", "3]", Ordering::Greater);
      compare_bounds_test_case("4]", "4)", Ordering::Greater);
      compare_bounds_test_case("4]", "[!", Ordering::Greater);
      compare_bounds_test_case("4]", "[3", Ordering::Greater);
      compare_bounds_test_case("4]", "(3", Ordering::Greater);
      compare_bounds_test_case("4)", "3)", Ordering::Greater);
      compare_bounds_test_case("!]", "4)", Ordering::Greater);
      compare_bounds_test_case("!]", "4]", Ordering::Greater);
    }

    /// Parse a string into a Bound, with the following syntax (where # is an integer):
    /// 
    ///   [#  ... Lower bound, inclusive
    ///   (#  ... Lower bound, exclusive
    ///   [!  ... Lower bound, unbounded
    ///   #]  ... Upper bound, inclusive
    ///   #)  ... Upper bound, exclusive
    ///   !]  ... Upper bound, unbounded
    /// 
    /// Returns a tuple, where the first item is true for a lower bound, false for an upper bound.
    fn parse_bound(s: &str) -> (bool, Bound<FeelValue>) {
      match (s, s.chars().nth(0).unwrap(), s.chars().last().unwrap()) {
        ("[!", _, _) => (true, Bound::Unbounded),
        ("!]", _, _) => (false, Bound::Unbounded),
        (_, '[', _) => (true, Bound::Included(s[1..].parse::<i32>().unwrap().into())),
        (_, '(', _) => (true, Bound::Excluded(s[1..].parse::<i32>().unwrap().into())),
        (_, _, ']') => (false, Bound::Included(s[..s.len()-1].parse::<i32>().unwrap().into())),
        (_, _, ')') => (false, Bound::Excluded(s[..s.len()-1].parse::<i32>().unwrap().into())),
        _ => panic!("Bad format")
      }
    }

    fn compare_bounds_test_case(a_pattern: &str, b_pattern: &str, expect: Ordering) -> () {
      let (a_is_lower_bound, a) = parse_bound(a_pattern);
      let (b_is_lower_bound, b) = parse_bound(b_pattern);
      let actual = Builtins::compare_bounds(&a, a_is_lower_bound, &b, b_is_lower_bound);
      assert!(expect == actual, "Comparison {:?} vs {:?} of {:?} with {:?} got {:?} but expected {:?}", a_pattern, b_pattern, a, b, actual, expect);
    }
}
