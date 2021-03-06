use std::{ops, fmt};
use std::rc::Rc;
use std::cell::RefCell;
use chrono::Duration as ChronoDuration;
use chrono::{NaiveDate, Date, NaiveDateTime, DateTime, NaiveTime, Datelike, offset::TimeZone};
use super::qname::QName;
use super::context::Context;
use super::duration::Duration;

#[derive(PartialEq, Eq, Clone)]
/// Indicates the Type of a Feel language value but does not contain the actual value.
enum FeelType {
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
  /// A sequential collection of other values
  List,
  /// A collection of name-value pairs
  Context,
  /// A function taking a single argument (a FeelValue) and returning a single result (a FeelValue)
  Function,
  /// A missing value
  Null,
  /// An execution error
  Error,
  /// A value of any type
  Any
}

#[derive(Clone)]
/// Any value permitted as input into a Feel language expression or be the result of such an expression.
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
  /// A sequential collection of other values
  List(Rc<RefCell<Vec<FeelValue>>>),
  /// A collection of name-value pairs
  Context(Rc<Context>),
  /// A function taking a single argument (a FeelValue) and returning a single result (a FeelValue)
  Function,
  /// A missing value
  Null,
  /// An execution error
  Error(String)
}

impl FeelValue {
  /// Get the FeelType that corresponds to the given FeelValue.
  fn get_type(&self) -> FeelType {
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
      FeelValue::List(_) => FeelType::List,
      FeelValue::Context(_) => FeelType::Context,
      FeelValue::Function => FeelType::Function,
      FeelValue::Null => FeelType::Null,
      FeelValue::Error(_) => FeelType::Error
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
        (FeelValue::List(l_list), FeelValue::List(r_list)) => are_vecs_equal(&*l_list.borrow(), &*r_list.borrow()),
        (FeelValue::Context(l_ctx), FeelValue::Context(r_ctx)) => l_ctx == r_ctx,
        // TODO: Implement Function compare.
        (FeelValue::Function, FeelValue::Function) => true,
        // Normally Nulls are not equal, but for this function they are.
        (FeelValue::Null, FeelValue::Null) => true,
        (FeelValue::Error(l_err), FeelValue::Error(r_err)) => l_err == r_err,
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
        FeelValue::Function => write!(f, "{}", "function"),
        FeelValue::Null => write!(f, "{}", "null"),
        FeelValue::Error(e) => write!(f, "Error: {}", e),
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
        FeelValue::Function => write!(f, "{}", "function"),
        FeelValue::Null => write!(f, "{}", "null"),
        FeelValue::Error(e) => write!(f, "Error: {}", e),
      }
  }
}

/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
  use super::FeelValue;
  use std::assert_ne;

  #[test]
  fn test_equals() {
    assert_eq!(FeelValue::Number(10.0), FeelValue::Number(10.0), "Equal Numbers");
    assert_ne!(FeelValue::Number(10.0), FeelValue::Number(11.0), "Unequal Numbers");
    assert_ne!(FeelValue::String("10.0".to_string()), FeelValue::Number(10.0), "String and Number");

    assert_eq!(FeelValue::Boolean(true), FeelValue::Boolean(true), "Equal Booleans");
    assert_ne!(FeelValue::Boolean(false), FeelValue::Boolean(true), "Unequal Booleans");

    assert_eq!(FeelValue::String("ABC".to_string()), FeelValue::String("ABC".to_string()), "Equal Strings");
    assert_ne!(FeelValue::String("ABC".to_string()), FeelValue::String("DEF".to_string()), "Unequal Strings");

  }

}
