use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use std::convert::{From,TryFrom};
use std::string::ToString;
use chrono::{NaiveDate, NaiveDateTime, NaiveTime};
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use super::qname::{QName, Stringlike};
use super::context::{Context, ContextReader};
use super::duration::Duration;
use super::range::Range;
use super::feel_function::FeelFunction;
use super::execution_log::ExecutionLog;

#[derive(PartialEq, Debug, Eq, Clone, Copy, ToString, IntoStaticStr)]
/// Indicates the Type of a Feel language value but does not contain the actual value.
pub enum FeelType {
  /// A Number
  Number,
  /// A String
  String,
  /// A Qualified Name, which is a list of strings, none of which should contain whitespace.
  Name, 
  /// A Boolean
  Boolean,
  /// A DateTime value with just the Date part filled in
  Date,
  /// A DateTime value with just the Time part filled in
  Time,
  /// A DateTime value with both Date and Time parts filled in
  DateAndTime,
  /// Duration with only years and months filled in
  YearMonthDuration, 
  /// Duration with only days, hours, minutes and seconds filled in
  DayTimeDuration,
  /// A Range defined as a pair of low and high values
  Range,
  /// A sequential collection of other values
  List,
  /// A collection of name-value pairs
  Context,
  /// A function taking a single argument (a FeelValue) and returning a single result (a FeelValue)
  Function,
  /// A missing value
  Null,
  /// A value of any type
  Any
}

#[derive(Clone)]
/// Any value permitted as input into a Feel language expression or as the result of such an expression.
pub enum FeelValue {
  /// A Number
  Number(f64),
  /// A String
  String(String),
  /// A Qualified Name, which is a list of strings
  Name(QName), 
  /// A Boolean
  Boolean(bool),
  /// A value with just a Date part
  Date(NaiveDate),
  /// A value with just a Time part
  Time(NaiveTime),
  /// A DateTime value with both Date and Time parts filled in
  DateAndTime(NaiveDateTime),
  /// Duration with only years and months filled in
  YearMonthDuration(Duration), 
  /// Duration with only days, hours, minutes and seconds filled in
  DayTimeDuration(Duration),
  /// A Range given as a pair of low and high values
  Range(Range),
  /// A sequential collection of other values
  List(Rc<RefCell<Vec<FeelValue>>>),
  /// A collection of name-value pairs
  Context(Rc<Context>),
  /// A function taking a single argument (a FeelValue) and returning a single result (a FeelValue)
  Function(FeelFunction),
  /// A missing value
  Null
}

impl FeelValue {
  /// Get the FeelType that corresponds to the given FeelValue.
  pub fn get_type(&self) -> FeelType {
    match self {
      FeelValue::Number(_) => FeelType::Number,
      FeelValue::String(_) => FeelType::String,
      FeelValue::Name(_) => FeelType::Name,
      FeelValue::Boolean(_) => FeelType::Boolean,
      FeelValue::Date(_) => FeelType::Date,
      FeelValue::Time(_) => FeelType::Time,
      FeelValue::DateAndTime(_) => FeelType::DateAndTime,
      FeelValue::YearMonthDuration(_) => FeelType::YearMonthDuration,
      FeelValue::DayTimeDuration(_) => FeelType::DayTimeDuration,
      FeelValue::Range(_) => FeelType::Range,
      FeelValue::List(_) => FeelType::List,
      FeelValue::Context(_) => FeelType::Context,
      FeelValue::Function(_) => FeelType::Function,
      FeelValue::Null => FeelType::Null
    }
  }

  pub fn new_list(items: Vec<FeelValue>) -> Self {
    FeelValue::List(Rc::new(RefCell::new(items)))
  }

  /// Used for creating parameter lists for builtins expecting a list. 
  pub fn new_list_of_list(items: Vec<FeelValue>) -> Self {
    FeelValue::new_list(vec![FeelValue::new_list(items)])
  }

  pub fn new_from_iterator<I>(iterable: I) -> Self 
  where I: IntoIterator, 
        I::Item: Into<FeelValue> {
    let values: Vec<FeelValue> = iterable.into_iter().map(|item| item.into()).collect();
    FeelValue::new_list(values)
  }

  pub fn negate(&self) -> Self {
    let type_error = || -> FeelValue {
      ExecutionLog::log(&format!("Cannot negate {:?}", self.get_type()));
      FeelValue::Null
    };
    match self {
      FeelValue::Number(n) => FeelValue::Number(-n),
      FeelValue::Boolean(b) => FeelValue::Boolean(!b),
      FeelValue::YearMonthDuration(ymd) => FeelValue::YearMonthDuration(- *ymd),
      FeelValue::DayTimeDuration(dtd) => FeelValue::DayTimeDuration(- *dtd),
      FeelValue::Null => type_error(),
      _ => type_error()
    }
  }

  /// Is the value something that can take a sign and is that sign negative?
  /// Consider Boolean False values to be negative.
  pub fn is_negative(&self) -> bool {
    match self {
      FeelValue::Number(n) => *n < 0.0_f64,
      FeelValue::Boolean(b) => !b,
      FeelValue::YearMonthDuration(ymd) => ymd.is_negative(),
      FeelValue::DayTimeDuration(dtd) => dtd.is_negative(),
      _ => false
    }
  }

  /// Is this value a FeelValue::Null?
  pub fn is_null(&self) -> bool {
    match self {
      FeelValue::Null => true,
      _ => false
    }
  }

  /// Is this value a FeelValue::Boolean(true)?
  pub fn is_true(&self) -> bool {
    match self {
      FeelValue::Boolean(b) => *b,
      _ => false
    }
  }
  
  /// Is this value a FeelValue::Boolean(false)?
  pub fn is_false(&self) -> bool {
    match self {
      FeelValue::Boolean(b) => !*b,
      _ => false
    }
  }

  /// Proper Feel semantics for equality, which returns Null for incomparable values.
  pub fn equal(&self, other: &Self) -> FeelValue {
    let type1 = self.get_type();
    let type2 = other.get_type();
    match (type1, type2) {
      (_, _) if type1 == type2 => FeelValue::Boolean(self == other),
      (FeelType::DayTimeDuration, FeelType::YearMonthDuration) => FeelValue::Boolean(self == other),
      (FeelType::YearMonthDuration, FeelType::DayTimeDuration) => FeelValue::Boolean(self == other),
      (_, _) => FeelValue::Null
    }
  }

  /// If the value is a Number that has no fractional part, return true,
  /// otherwise false.
  pub fn is_integer(&self) -> bool {
    match self {
      FeelValue::Number(n) => n.fract() == 0.0,
      _ => false
    }
  }

  pub fn is_nan(&self) -> bool {
    match self {
      FeelValue::Number(n) => n.is_nan(),
      _ => false
    }
  }

  /// Is the value a Number that represents positive or negative infinity?
  pub fn is_infinite(&self) -> bool {
    match self {
      FeelValue::Number(n) => n.is_infinite(),
      _ => false
    }
  }

  pub fn is_finite(&self) -> bool {
    match self {
      FeelValue::Number(n) => n.is_finite(),
      _ => false
    }
  }

  /// Gets the number of items in the FeelValue::List, or None if it is not a List. 
  pub fn list_length(&self) -> Option<usize> {
    match self {
      FeelValue::List(list) => Some(list.borrow().len()),
      _ => None
    }
  }

  /// Is this a FeelValue::List with a single item that is itself a List?
  pub fn is_list_of_list(&self) -> bool {
    match self {
      FeelValue::List(rr_list) => {
        if rr_list.borrow().len() != 1 {
          false
        }
        else {
          rr_list.borrow()[0].get_type() == FeelType::List
        }
      },
      _ => false
    }
  }

  /// True if the type has date information, i.e. is Date or DateAndTime.
  pub fn has_date(&self) -> bool {
    match self {
      FeelValue::Date(_) => true,
      FeelValue::DateAndTime(_) => true,
      _ => false
    }
  }

  /// Try to interpret self as a Range and test whether
  /// the point falls before (Less), in (Equals) or after (Greater) the range.
  /// If the point cannot be used with Ranges or self is not a Range, return None.
  pub fn try_range_compare<C: ContextReader>(&self, point: &FeelValue, contexts: &C) -> Option<Ordering> {
    match (Range::is_suitable_as_point(point.get_type()), self) {
      (false, _) => None,
      (true, FeelValue::Range(r)) => Some(r.compare(point, contexts)),
      (true, _) => None
    }
  }

  /// Flatten a hierarchy of FeelValues where most FeelValues are leaves, but FeelValue::List's are
  /// considered branches. 
  ///   - Use depth-first ordering during the tree walk. 
  ///   - Collect all values into the supplied list. 
  ///   - The output will have no FeelValue::Lists in it.
  pub fn flatten_into(&self, flat_list: &mut Vec<FeelValue>) {
    match self {
      FeelValue::List(source_list) => {
        for item in source_list.borrow().iter() {
          item.flatten_into(flat_list);
          ()
        }
      },
      _ => { 
        flat_list.push(self.clone()); 
        () 
      }
    }
  }

}

fn are_vecs_equal<T: PartialEq>(a: &Vec<T>, b: &Vec<T>) -> bool {
  if a.len() != b.len() { return false }
  let matching = a.iter().zip(b.iter()).filter(|&(a, b)| a == b).count();
  matching == a.len() && matching == b.len()
}

impl PartialEq for FeelValue {
  /// Implement representational equality, which may not be the same in the Feel language
  /// as value equality. For example, NaN should not equal NaN, and Null should not equal Null,
  /// but this function will say that they are equal.
  fn eq(&self, other: &Self) -> bool {
      if self.get_type() != other.get_type() { return false}
      match (self, other) {
        (FeelValue::Number(l_num), FeelValue::Number(r_num)) 
          => l_num == r_num || (l_num.is_nan() && r_num.is_nan()),
        (FeelValue::String(l_str), FeelValue::String(r_str)) => l_str == r_str,
        (FeelValue::Name(l_name), FeelValue::Name(r_name)) => l_name == r_name,
        (FeelValue::Boolean(l_bool), FeelValue::Boolean(r_bool)) => l_bool == r_bool,
        (FeelValue::Date(l_date), FeelValue::Date(r_date)) => l_date == r_date,
        (FeelValue::Time(l_time), FeelValue::Time(r_time)) => l_time == r_time,
        (FeelValue::DateAndTime(l_dt), FeelValue::DateAndTime(r_dt)) => l_dt == r_dt,
        (FeelValue::YearMonthDuration(l_ymd), FeelValue::YearMonthDuration(r_ymd)) => l_ymd == r_ymd,
        (FeelValue::DayTimeDuration(l_dtd), FeelValue::DayTimeDuration(r_dtd)) => l_dtd == r_dtd,
        // Special case in the DMN 1.2 Spec, section 9.4: 
        // The two types of duration can be considered equal if both are zero, but in no other case.
        (FeelValue::YearMonthDuration(l_ymd), FeelValue::DayTimeDuration(r_dtd)) => l_ymd.is_zero() && r_dtd.is_zero(),
        (FeelValue::DayTimeDuration(l_dtd), FeelValue::YearMonthDuration(r_ymd)) => r_ymd.is_zero() && l_dtd.is_zero(),
        (FeelValue::Range(l_range), FeelValue::Range(r_range)) => l_range == r_range,
        (FeelValue::List(l_list), FeelValue::List(r_list)) => are_vecs_equal(&*l_list.borrow(), &*r_list.borrow()),
        (FeelValue::Context(l_ctx), FeelValue::Context(r_ctx)) => l_ctx == r_ctx,
        // TODO: Implement Function compare.
        (FeelValue::Function(l_func), FeelValue::Function(r_func)) => l_func == r_func,
        // Normally Nulls are not equal, but for this function they are.
        (FeelValue::Null, FeelValue::Null) => true,
        _ => false
      }
  }

}

impl Eq for FeelValue {}

impl fmt::Debug for FeelValue {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      match self {
        FeelValue::Number(n) => write!(f, "{}", n),
        FeelValue::String(s) => write!(f, "'{}'", s),
        FeelValue::Name(q) => write!(f, "QName('{:?}')", q),
        FeelValue::Boolean(b) => write!(f, "{}", b),
        FeelValue::Date(d) => write!(f, "{:?}", d),
        FeelValue::Time(t) => write!(f, "{:?}", t),
        FeelValue::DateAndTime(dt) => write!(f, "{:?}", dt),
        FeelValue::YearMonthDuration(ymd) => write!(f, "{:?}", ymd),
        FeelValue::DayTimeDuration(dtd) => write!(f, "{:?}", dtd),
        FeelValue::Range(range) => write!(f, "{:?}", range),
        FeelValue::List(l) => {
          let mut combined = String::with_capacity(1000);
          let mut comma = "";
          for item in l.borrow().iter() {
            combined.push_str(comma);
            combined.push_str(&format!("{:?}", item));
            comma = ", ";
          }
          write!(f, "[{}]", combined)
        },
        FeelValue::Context(c) => write!(f, "{:?}", c),
        FeelValue::Function(func) => write!(f, "{:?}", func),
        FeelValue::Null => write!(f, "{}", "null")
      }
  }
}

impl fmt::Display for FeelValue {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      FeelValue::Number(n) => write!(f, "{}", n),
      FeelValue::String(s) => write!(f, "'{}'", s),
      FeelValue::Name(q) => write!(f, "'{}'", q),
      FeelValue::Boolean(b) => write!(f, "{}", b),
      FeelValue::Date(d) => write!(f, "{}", d),
      FeelValue::Time(t) => write!(f, "{}", t),
      FeelValue::DateAndTime(dt) => write!(f, "{}", dt),
      FeelValue::YearMonthDuration(ymd) => write!(f, "{}", ymd),
      FeelValue::DayTimeDuration(dtd) => write!(f, "{}", dtd),
      FeelValue::Range(range) => write!(f, "{}", range),
      FeelValue::List(l) => {
        let mut combined = String::with_capacity(1000);
        let mut comma = "";
        for item in l.borrow().iter() {
          combined.push_str(comma);
          combined.push_str(&format!("{}", item));
          comma = ", ";
        }
        write!(f, "[{}]", combined)
      },
      FeelValue::Context(c) => write!(f, "{}", c),
      FeelValue::Function(func) => write!(f, "{}", func),
      FeelValue::Null => write!(f, "{}", "null")
    }
  }
}

/////////////// Conversions from Basic Types /////////////////

/// Marker trait so we don't get conflicting trait errors due to the coherence rules.
pub trait Numeric {}
impl Numeric for f64 {}
impl Numeric for f32 {}
impl Numeric for i64 {}
impl Numeric for i32 {}
impl Numeric for i16 {}
impl Numeric for i8 {}
impl Numeric for isize {}
impl Numeric for u64 {}
impl Numeric for u32 {}
impl Numeric for u16 {}
impl Numeric for u8 {}
impl Numeric for usize {}


impl<N: Into<f64> + Numeric> From<N> for FeelValue {
  fn from(n: N) -> Self {
    FeelValue::Number(n.into())
  }
}

impl From<String> for FeelValue {
  fn from(s: String) -> Self {
    FeelValue::String(s)
  }
}

impl From<&str> for FeelValue {
  fn from(s: &str) -> Self {
    FeelValue::String(s.to_string())
  }
}

impl From<bool> for FeelValue {
  fn from(b: bool) -> Self {
    FeelValue::Boolean(b)
  }
}

impl<S: Into<String> + Clone + Stringlike> From<&Vec<S>> for FeelValue {
  fn from(v: &Vec<S>) -> Self {
    FeelValue::Name(v.into())
  }
}
      

impl Hash for FeelValue {
  fn hash<H: Hasher>(&self, state: &mut H) {
    match self {
      // TODO: Implement a proper hash function for f64. This is slow and may overflow!
      FeelValue::Number(v) => ((*v * 1000000.0) as i128).hash(state),
      FeelValue::String(v) => v.hash(state),
      FeelValue::Name(v) => v.hash(state),
      FeelValue::Boolean(v) => v.hash(state),
      FeelValue::Date(v) => v.hash(state),
      FeelValue::Time(v) => v.hash(state),
      FeelValue::DateAndTime(v) => v.hash(state),
      FeelValue::YearMonthDuration(v) => v.hash(state),
      FeelValue::DayTimeDuration(v) => v.hash(state),
      FeelValue::Range(v) => v.hash(state),
      FeelValue::List(v) => v.borrow().len().hash(state),
      // TODO: Implement proper Hash for Context
      FeelValue::Context(_) => "context".hash(state),
      FeelValue::Function(func) => func.get_name().hash(state),
      FeelValue::Null => "null".hash(state)
    }
  }
}

impl TryFrom<&FeelValue> for f64 {
  type Error = ();

  fn try_from(value: &FeelValue) -> Result<Self, Self::Error> {
      match value {
        FeelValue::Number(n) => Ok(*n),
        _ => Err(())
      }
  }
}

/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
  use super::{FeelValue};
  use super::super::qname::{QName};
  use std::assert_ne;
  use super::super::duration::Duration;
  use std::str::FromStr;

  #[test]
  fn test_equals() {
    assert_eq!(FeelValue::Number(10.0), FeelValue::Number(10.0), "Equal Numbers");
    assert_ne!(FeelValue::Number(10.0), FeelValue::Number(11.0), "Unequal Numbers");
    assert_ne!(FeelValue::String("10.0".to_string()), FeelValue::Number(10.0), "String and Number");

    assert_eq!(FeelValue::Boolean(true), FeelValue::Boolean(true), "Equal Booleans");
    assert_ne!(FeelValue::Boolean(false), FeelValue::Boolean(true), "Unequal Booleans");

    assert_eq!(FeelValue::String("ABC".to_string()), FeelValue::String("ABC".to_string()), "Equal Strings");
    assert_ne!(FeelValue::String("ABC".to_string()), FeelValue::String("DEF".to_string()), "Unequal Strings");

    assert_eq!(
      FeelValue::DayTimeDuration(Duration::from_str("PT1H").unwrap()), 
      FeelValue::DayTimeDuration(Duration::from_str("PT1H").unwrap()), "Equal durations");
    assert_ne!(
      FeelValue::DayTimeDuration(Duration::from_str("PT1H").unwrap()), 
      FeelValue::DayTimeDuration(Duration::from_str("PT2H").unwrap()), "Unequal durations");
  
  }

  #[test]
  fn test_from_number() {
    let from_i32: FeelValue = 42_i32.into();
    let from_f32: FeelValue = 42.0_f32.into();
    assert_eq!(&from_i32, &from_f32, "From i32 and f32");
  }

  #[test]
  fn test_from_string() {
    let a: FeelValue = "Hello".to_string().into();
    let b: FeelValue = "Hello".to_string().into();
    assert_eq!(&a, &b, "Compare equal Strings");
  }

  #[test]
  fn test_from_bool() {
    let t: FeelValue = true.into();
    let f: FeelValue = false.into();
    assert_eq!(&t, &FeelValue::Boolean(true), "Verify a true value");
    assert_eq!(&f, &FeelValue::Boolean(false), "Verify a false value");
  }

  #[test]
  fn test_from_str_vec() {
    let v1 = vec!["John", "Smith"];
    let qname: QName = (&v1).into();
    let q1: FeelValue = FeelValue::Name(qname);

    let v2 = vec!["John", "Smith"];
    let q2: FeelValue = (&v2).into();

    assert_eq!(q1, q2);
  }

  #[test]
  fn test_is_integer() {
    let x: FeelValue = 5.into();
    let y: FeelValue = 7.7.into();
    assert!(x.is_integer(), "Verify an integral value");
    assert!(!y.is_integer(), "Verify a non-integral value");
  }

}
