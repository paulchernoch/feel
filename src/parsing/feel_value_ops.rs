use std::cmp;
use std::ops;
use std::rc::Rc;
use std::cell::RefCell;
use super::feel_value::FeelValue;
use super::duration::Duration;
use chrono::Duration as ChronoDuration;
use chrono::{DateTime, Timelike, Datelike, NaiveDate, NaiveDateTime, NaiveTime};

impl<'a, 'b> ops::Add<&'b FeelValue> for &'a FeelValue {
  type Output = FeelValue;

  fn add(self, rhs: &'b FeelValue) -> Self::Output {
    match (self, rhs) {
      (FeelValue::Number(l_num), FeelValue::Number(r_num)) => FeelValue::Number(l_num + r_num),
      (FeelValue::String(l_str), FeelValue::String(r_str)) => FeelValue::String(format!("{}{}", l_str, r_str)),
      (FeelValue::YearMonthDuration(l_ymd), FeelValue::YearMonthDuration(r_ymd)) => FeelValue::YearMonthDuration(*l_ymd + *r_ymd),
      (FeelValue::DayTimeDuration(l_dtd), FeelValue::DayTimeDuration(r_dtd)) => FeelValue::YearMonthDuration(*l_dtd + *r_dtd),
      (FeelValue::Date(l_d), FeelValue::YearMonthDuration(r_ymd)) => FeelValue::Date(*l_d + *r_ymd),
      (FeelValue::YearMonthDuration(l_ymd), FeelValue::Date(r_d)) => FeelValue::Date(*r_d + *l_ymd),
      (FeelValue::DateAndTime(l_dt), FeelValue::YearMonthDuration(r_ymd)) => FeelValue::DateAndTime(*l_dt + *r_ymd),
      (FeelValue::YearMonthDuration(l_ymd), FeelValue::DateAndTime(r_dt)) => FeelValue::DateAndTime(*r_dt + *l_ymd),
      (FeelValue::DateAndTime(l_dt), FeelValue::DayTimeDuration(r_dtd)) => FeelValue::DateAndTime(*l_dt + *r_dtd),
      (FeelValue::DayTimeDuration(l_dtd), FeelValue::DateAndTime(r_dt)) => FeelValue::DateAndTime(*r_dt + *l_dtd),
      (FeelValue::Time(l_t), FeelValue::DayTimeDuration(r_dtd)) => FeelValue::Time(*l_t + *r_dtd),
      (FeelValue::DayTimeDuration(l_dtd), FeelValue::Time(r_t)) => FeelValue::Time(*r_t + *l_dtd),
      (FeelValue::List(l_list), r) => {
        let mut new_list: Vec<FeelValue> = l_list.borrow().clone();
        new_list.push(r.clone());
        FeelValue::List(Rc::new(RefCell::new(new_list)))
      },
      (FeelValue::Error(_), _) => self.clone(),
      (_, FeelValue::Error(_)) => rhs.clone(),
      (FeelValue::Null, _) => FeelValue::Null,
      (_, FeelValue::Null) => FeelValue::Null,
      _ => FeelValue::Error(format!("Cannot add {} to {}", self.get_type().to_string(), rhs.get_type().to_string()))
    }
  }
  
}



/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
  use std::str::FromStr;
  use std::clone::Clone;
  use chrono::{ DateTime, Datelike, NaiveDate, NaiveDateTime, NaiveTime };
  use super::super::duration::Duration;
  use super::super::feel_value::{FeelValue, Numeric};
  use super::super::qname::{QName, Stringlike};
  use std::assert_ne;

  #[test]
  fn test_add_numbers() {
    let x: FeelValue = 1.into();
    let y: FeelValue = 2.into();
    let expected:FeelValue = 3.into();
    assert_eq!(expected, &x + &y, "add numbers");
  }

  #[test]
  fn test_add_incompatibles() {
    let x: FeelValue = 1.into();
    let y: FeelValue = true.into();
    let expected:FeelValue = FeelValue::Error("Cannot add Number to Boolean".to_string());
    assert_eq!(expected, &x + &y, "add Number to Boolean");
  }

  #[test]
  fn test_add_durations() {
    let x: FeelValue = FeelValue::YearMonthDuration(Duration::from_str("P1Y3M").unwrap());
    let y: FeelValue = FeelValue::YearMonthDuration(Duration::from_str("P2Y5M").unwrap());
    let expected:FeelValue = FeelValue::YearMonthDuration(Duration::from_str("P3Y8M").unwrap());
    assert_eq!(expected, &x + &y, "add year month durations");
  }

  #[test]
  fn test_add_time_and_duration() {
    let x: FeelValue = FeelValue::Time(NaiveTime::from_hms(14_u32, 30_u32, 0_u32));
    let y: FeelValue = FeelValue::DayTimeDuration(Duration::from_str("PT45M20S").unwrap()); 
    let expected:FeelValue = FeelValue::Time(NaiveTime::from_hms(15_u32, 15_u32, 20_u32));
    assert_eq!(expected, &x + &y, "add time and day time duration");
  }

}
