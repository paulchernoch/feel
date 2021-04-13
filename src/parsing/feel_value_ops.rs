//use std::cmp;
use std::ops;
use std::cmp::{Ord, PartialOrd, Ordering};
use std::rc::Rc;
use std::cell::RefCell;
use super::feel_value::{FeelValue, FeelType};
use super::duration::{Duration, DAYS_PER_MONTH};
use super::execution_log::ExecutionLog;

/////////////// Arithmetic Operations: Add, Neg, Sub, Mul, Div /////////////////

impl<'a, 'b> ops::Add<&'b FeelValue> for &'a FeelValue {
  type Output = FeelValue;

  fn add(self, rhs: &'b FeelValue) -> Self::Output {
    match (self, rhs) {
      (FeelValue::Number(l_num), FeelValue::Number(r_num)) => FeelValue::Number(l_num + r_num),
      (FeelValue::String(l_str), FeelValue::String(r_str)) => FeelValue::String(format!("{}{}", l_str, r_str)),
      (FeelValue::YearMonthDuration(l_ymd), FeelValue::YearMonthDuration(r_ymd)) => 
        FeelValue::YearMonthDuration((*l_ymd + *r_ymd).as_year_month(DAYS_PER_MONTH as f32)),
      (FeelValue::DayTimeDuration(l_dtd), FeelValue::DayTimeDuration(r_dtd)) => 
        FeelValue::DayTimeDuration((*l_dtd + *r_dtd).as_day_time(DAYS_PER_MONTH as f32)),
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
      (FeelValue::Null, _) => FeelValue::Null,
      (_, FeelValue::Null) => FeelValue::Null,
      _ => {
        ExecutionLog::log(&format!("Cannot add {} to {}", self.get_type().to_string(), rhs.get_type().to_string()));
        FeelValue::Null
      } 
    }
  }
  
}

impl ops::Neg for FeelValue {
  type Output = FeelValue;

  fn neg(self) -> Self::Output {
      self.negate()
  }
}

impl ops::Neg for &FeelValue {
  type Output = FeelValue;

  fn neg(self) -> Self::Output {
      (*self).negate()
  }
}

impl<'a, 'b> ops::Sub<&'b FeelValue> for &'a FeelValue {
  type Output = FeelValue;

  fn sub(self, rhs: &'b FeelValue) -> Self::Output {
    let type_error = || -> FeelValue {
      ExecutionLog::log(&format!("Cannot subtract {} minus {}", self.get_type().to_string(), rhs.get_type().to_string()));
      FeelValue::Null
    };
    match (self, rhs) {
      (FeelValue::Date(d1), FeelValue::Date(d2)) => {
        // TODO: Should we instead use date_difference_months?
        let delta = Duration::date_difference_days(*d1, *d2).as_year_month(DAYS_PER_MONTH as f32);
        FeelValue::YearMonthDuration(delta)
      },
      (FeelValue::DateAndTime(dt1), FeelValue::DateAndTime(dt2)) => {
        let delta = Duration::datetime_difference(*dt1, *dt2).as_day_time(DAYS_PER_MONTH as f32);
        FeelValue::YearMonthDuration(delta)
      },
      (FeelValue::Time(t1), FeelValue::Time(t2)) => {
        let delta = Duration::time_difference(*t1, *t2).as_day_time(DAYS_PER_MONTH as f32);
        FeelValue::DayTimeDuration(delta)
      },
      (_, _) => {
        let result = self + &(-rhs);
        match result {
          FeelValue::Null => type_error(), 
          _ => result
        }
      }
    }
  }
}

impl<'a, 'b> ops::Mul<&'b FeelValue> for &'a FeelValue {
  type Output = FeelValue;

  fn mul(self, rhs: &'b FeelValue) -> Self::Output {
    let type_error = || -> FeelValue {
      ExecutionLog::log(&format!("Cannot multiply {} by {}", self.get_type().to_string(), rhs.get_type().to_string()));
      FeelValue::Null
    };
    match (self, rhs) {
      (FeelValue::Number(l_num), FeelValue::Number(r_num)) => FeelValue::Number(l_num * r_num),
      (FeelValue::YearMonthDuration(l_ymd), FeelValue::Number(r_factor)) => 
        FeelValue::YearMonthDuration((*l_ymd * (*r_factor as f32)).as_year_month(DAYS_PER_MONTH as f32)),
      (FeelValue::Number(l_factor), FeelValue::YearMonthDuration(r_ymd)) => 
        FeelValue::YearMonthDuration((*r_ymd * (*l_factor as f32)).as_year_month(DAYS_PER_MONTH as f32)),
      (FeelValue::DayTimeDuration(l_dtd), FeelValue::Number(r_factor)) => 
        FeelValue::DayTimeDuration((*l_dtd * (*r_factor as f32)).as_day_time(DAYS_PER_MONTH as f32)),
      (FeelValue::Number(l_factor), FeelValue::DayTimeDuration(r_dtd)) => 
        FeelValue::DayTimeDuration((*r_dtd * (*l_factor as f32)).as_day_time(DAYS_PER_MONTH as f32)),
      (FeelValue::Null, _) => type_error(),
      (_, FeelValue::Null) => type_error(),
      _ => type_error()
    }
  }
}

impl<'a, 'b> ops::Div<&'b FeelValue> for &'a FeelValue {
  type Output = FeelValue;

  fn div(self, rhs: &'b FeelValue) -> Self::Output {
    let type_error = || -> FeelValue {
      ExecutionLog::log(&format!("Cannot divide {} by {}", self.get_type().to_string(), rhs.get_type().to_string()));
      FeelValue::Null
    };
    let div_zero_error = || -> FeelValue {
      ExecutionLog::log(&format!("Division by zero, infinity or NaN {:?}", rhs));
      FeelValue::Null
    };
    let infinite_ratio = || -> FeelValue {
      ExecutionLog::log(&format!("Ratio is infinite {:?} / {:?}", self, rhs));
      FeelValue::Null
    };
    match (self, rhs) {
      (FeelValue::Null, _) => type_error(),
      (_, FeelValue::Null) => type_error(),
      (_, FeelValue::Number(r_num)) if *r_num == 0.0 || !r_num.is_finite() => div_zero_error(), // Division by zero, Infinity or NaN
      (FeelValue::Number(l_num), FeelValue::Number(r_num)) => {
        let ratio = l_num / r_num;
        // Feel language dies not permit Infinities or NaN and uses Null instead.
        if ratio.is_finite() { FeelValue::Number(ratio) }
        else { infinite_ratio() }
      },
      (FeelValue::YearMonthDuration(l_ymd), FeelValue::Number(r_factor)) => 
        FeelValue::YearMonthDuration((*l_ymd / (*r_factor as f32)).as_year_month(DAYS_PER_MONTH as f32)),
      (FeelValue::Number(l_factor), FeelValue::YearMonthDuration(r_ymd)) => 
        FeelValue::YearMonthDuration((*r_ymd / (*l_factor as f32)).as_year_month(DAYS_PER_MONTH as f32)),
      (FeelValue::DayTimeDuration(l_dtd), FeelValue::Number(r_factor)) => 
        FeelValue::DayTimeDuration((*l_dtd / (*r_factor as f32)).as_day_time(DAYS_PER_MONTH as f32)),
        (FeelValue::YearMonthDuration(l_ymd), FeelValue::YearMonthDuration(r_ymd)) => {
          let ratio = *l_ymd / *r_ymd;
          return if ratio.is_finite() { FeelValue::Number(ratio as f64) }
          else { infinite_ratio() };
        },
        (FeelValue::DayTimeDuration(l_dtd), FeelValue::DayTimeDuration(r_dtd)) => {
          let ratio = *l_dtd / *r_dtd;
          return if ratio.is_finite() {
            FeelValue::Number(ratio as f64)
          }
          else { infinite_ratio() };
        },
        _ => type_error()
    }
  }
}

/////////////// Relational Operations /////////////////

impl FeelType {

  /// Get the sort order of the variants of FeelType.
  /// 
  /// Observe that DateAndTime values will be interleaved with Dates
  /// and YearMonthDurations will be interleaved with DayTimeDurations,
  /// as they are commensurate.
  pub fn get_sort_order(self) -> i32 {
    match self {
      FeelType::Number => 1_i32,
      FeelType::String => 2_i32,
      FeelType::Name => 3_i32,
      FeelType::Boolean => 4_i32,
      FeelType::Date => 5_i32,
      FeelType::Time => 6_i32,
      FeelType::DateAndTime => 5_i32,
      FeelType::YearMonthDuration => 7_i32,
      FeelType::DayTimeDuration => 7_i32,
      FeelType::Range => 8_i32,
      FeelType::List => 9_i32,
      FeelType::Context => 10_i32,
      FeelType::Function => 11_i32,
      FeelType::Null => 12_i32,
      FeelType::Any => 13_i32
    }
  }
}

/// Compare two f64 values, forcing NaNs to be sortable.
pub fn compare_f64(left: &f64, right: &f64) -> Ordering {
  match left.partial_cmp(right) {
    Some(ord) => ord,
    None => {
      match (left.is_nan(), right.is_nan()) {
        (true, true) => Ordering::Equal,
        (true, false) => Ordering::Less,
        (false, true) => Ordering::Greater,
        _ => unreachable!()
      }
    }
  }
}

pub fn compare_lists(left: &Vec<FeelValue>, right: &Vec<FeelValue>) -> Ordering {
  let mut it_left = left.iter();
  let mut it_right = right.iter();
  let mut comparison = Ordering::Equal;
  loop {
      match (it_left.next(), it_right.next()) {
          (Some(x), Some(y)) => {
            comparison = x.cmp(y);
            if comparison != Ordering::Equal {
              break;
            }
          },
          (Some(_), None) => {
            comparison = Ordering::Greater;
            break
          } , 
          (None, Some(_)) => { 
            comparison = Ordering::Less;
            break
          }, 
          (None, None) =>  break
      }
  }
  comparison
}

impl PartialOrd for FeelValue {

  /// This will force full ordering on values, including floats.
  /// The rules are more relaxed than the Feel specification dictates. 
  /// In most cases, the spec only permits comparison of items of the same type, 
  /// with the exception of DateTime and Date or YearMonthDuration and DayTimeDuration.
  /// 
  ///   - Less:         self is less than value.    -or- self is not a number (NaN) and value is a number.
  ///   - Equal:        self is equal to value.     -or- Both self and value are not a number (NaN), PositiveInfinity, or NegativeInfinity.
  ///   - Greater than: self is greater than value. -or- self is a number and value is not a number (NaN).
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    let left_type_order = FeelType::get_sort_order(self.get_type());
    let right_type_order = FeelType::get_sort_order(other.get_type());
    match left_type_order.cmp(&right_type_order) {
      Ordering::Less => Some(Ordering::Less),
      Ordering::Greater => Some(Ordering::Greater),
      Ordering::Equal => {
        match (self, other) {
          (FeelValue::Number(l_num), FeelValue::Number(r_num)) => Some(compare_f64(l_num, r_num)),
          (FeelValue::String(l_str), FeelValue::String(r_str)) => Some(l_str.cmp(r_str)),
          (FeelValue::Name(l_name), FeelValue::Name(r_name)) => Some(l_name.cmp(r_name)),
          (FeelValue::Boolean(l_bool), FeelValue::Boolean(r_bool)) => Some(l_bool.cmp(&r_bool)),
          (FeelValue::Date(l_date), FeelValue::Date(r_date)) => Some(l_date.cmp(&r_date)),
          (FeelValue::Time(l_time), FeelValue::Time(r_time)) => Some(l_time.cmp(&r_time)),
          (FeelValue::DateAndTime(l_dt), FeelValue::DateAndTime(r_dt)) => Some(l_dt.cmp(&r_dt)),
          // Date and DateTime may be compared by extracting the Date from the DateTime.
          (FeelValue::DateAndTime(l_dt), FeelValue::Date(r_date)) => Some(l_dt.date().cmp(&r_date)),
          (FeelValue::Date(l_date), FeelValue::DateAndTime(r_dt)) => Some(l_date.cmp(&r_dt.date())),
          (FeelValue::YearMonthDuration(l_ymd), FeelValue::YearMonthDuration(r_ymd)) => Some(l_ymd.cmp(&r_ymd)),
          (FeelValue::DayTimeDuration(l_dtd), FeelValue::DayTimeDuration(r_dtd)) => Some(l_dtd.cmp(&r_dtd)),
          // Special case in the DMN 1.2 Spec, section 9.4: 
          // The two types of duration can be considered equal if both are zero, but in no other case.
          // We relax this here, and must tighten it up elsewhere.
          (FeelValue::YearMonthDuration(l_ymd), FeelValue::DayTimeDuration(r_dtd)) => Some(l_ymd.cmp(&r_dtd)),
          (FeelValue::DayTimeDuration(l_dtd), FeelValue::YearMonthDuration(r_ymd)) => Some(l_dtd.cmp(&r_ymd)),
          (FeelValue::Range(l_range), FeelValue::Range(r_range)) => Some(l_range.cmp(&r_range)),
          (FeelValue::List(left), FeelValue::List(right)) => Some(compare_lists(&left.borrow(), &right.borrow())),
          // TODO: Decide what to do with Contexts
          (FeelValue::Context(_), FeelValue::Context(_)) => None,
          // TODO: Implement Function compare.
          (FeelValue::Function(l_func), FeelValue::Function(r_func)) => Some(l_func.cmp(&r_func)),
          // Normally Nulls are not equal, but for this function they are.
          (FeelValue::Null, FeelValue::Null) => Some(Ordering::Equal),
          _ => None
        }
      }
    }
  }
}

impl Ord for FeelValue {
  fn cmp(&self, other: &Self) -> Ordering {
      self.partial_cmp(other).unwrap()
  }
}

/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
  use std::str::FromStr;
  use chrono::{ NaiveDate, NaiveTime };
  use super::super::duration::Duration;
  // use super::super::feel_value::{Numeric};
  use super::super::feel_value::{FeelValue};
  use super::super::execution_log::ExecutionLog;
  // use std::assert_ne;

  #[test]
  fn test_add_numbers() {
    let x: FeelValue = 1.into();
    let y: FeelValue = 2.into();
    let expected:FeelValue = 3.into();
    assert_eq!(expected, &x + &y, "add numbers");
  }

  #[test]
  fn test_add_incompatibles() {
    ExecutionLog::clear();
    let x: FeelValue = 1.into();
    let y: FeelValue = true.into();
    let expected:FeelValue = FeelValue::Null;
    assert_eq!(expected, &x + &y, "add Number to Boolean");
    assert_eq!(1, ExecutionLog::count(), "check size of log");
    ExecutionLog::clear();
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

  #[test]
  fn test_negate_number() {
    let x: FeelValue = 1.into();
    let expected:FeelValue = (-1).into();
    assert_eq!(expected, -x, "negate number");
  }

  #[test]
  fn test_sub_numbers() {
    let x: FeelValue = 10.into();
    let y: FeelValue = 12.into();
    let expected:FeelValue = (-2).into();
    assert_eq!(expected, &x - &y, "sub numbers");
  }

  #[test]
  fn test_div_numbers() {
    let x: FeelValue = 100.into();
    let y: FeelValue = 10.into();
    let expected:FeelValue = 10.into();
    assert_eq!(expected, &x / &y, "div numbers");

    let z: FeelValue = 0.0.into();
    assert_eq!(FeelValue::Null, &x / &z, "div by zero");
  }

  #[test]
  fn test_div_durations() {
    let x = FeelValue::DayTimeDuration(Duration::new_day_time(true, 1_u32, 0_u32, 0_u32, 0.0));
    let y = FeelValue::DayTimeDuration(Duration::new_day_time(true, 0_u32, 2_u32, 0_u32, 0.0));
    let expected:FeelValue = 12.into();
    assert_eq!(expected, &x / &y, "div durations");
  }

  #[test]
  fn test_sub_dates() {
    let x = FeelValue::Date(NaiveDate::from_ymd(2020, 3, 15));
    let y = FeelValue::Date(NaiveDate::from_ymd(2020, 4, 28));
    let expected = FeelValue::YearMonthDuration(Duration::new_year_month(true, 0_u32, 1_u32));
    assert_eq!(expected, &y - &x, "sub dates");
  }

  #[test]
  fn test_sub_times() {
    let x = FeelValue::Time(NaiveTime::from_hms(18_u32, 0_u32, 0_u32));
    let y = FeelValue::Time(NaiveTime::from_hms(16_u32, 30_u32, 0_u32));
    let expected = FeelValue::DayTimeDuration(Duration::new_day_time(false, 0_u32, 1_u32, 30_u32, 0.0));
    assert_eq!(expected, &y - &x, "sub times");
  }

  #[test]
  fn test_mul_numbers() {
    let x: FeelValue = 6.into();
    let y: FeelValue = 7.into();
    let expected:FeelValue = 42.into();
    assert_eq!(expected, &x * &y, "multiply numbers");
  }

  #[test]
  fn test_mul_duration_by_number() {
    let x: FeelValue = FeelValue::DayTimeDuration(Duration::from_str("PT1H10M20S").unwrap());
    let y: FeelValue = 3.into();
    let expected:FeelValue = FeelValue::DayTimeDuration(Duration::from_str("PT3H31M0S").unwrap());
    let product = &x * &y;
    assert_eq!(expected, product, "multiply duration by number");
  }

  #[test]
  fn test_cmp_numbers() {
    let x: FeelValue = 100.into();
    let y: FeelValue = 10.into();
    assert_eq!(true, x > y, "x > y");
    assert_eq!(false, x < y, "x < y");
    assert_eq!(false, x <= y, "x <= y");
  }

  #[test]
  fn test_cmp_nans() {
    let nan: FeelValue = f64::NAN.into();
    let y: FeelValue = 10.into();
    assert_eq!(false, nan > y, "nan > y");
    assert_eq!(true, nan < y, "nan < y");
    assert_eq!(true, nan >= nan, "nan >= nan");
  }

  #[test]
  fn test_cmp_strings() {
    let x: FeelValue = "hello".into();
    let y: FeelValue = "world".into();
    assert_eq!(false, x > y, "x > y");
    assert_eq!(true, x < y, "x < y");
    assert_eq!(true, x <= y, "x <= y");
    assert_eq!(false, x >= y, "x >= y");
  }
}
