use std::fmt;
use std::cmp;

#[derive(Debug, Copy, Clone)]
pub enum DurationVariety {
  /// Duration will only be permitted to have years and months set, and days, hours, minutes and seconds must be zero.
  YearMonth,
  /// Duration will only be permitted to have days, hours, minutes and seconds set, and years and months must be zero.
  DayTime,
  /// Duration may have any values set to a nonzero value.
  Full
}

#[derive(Debug, Copy, Clone)]
pub struct Duration {
  /// Restricts which properties may be set to a nonzero value and affects the string formatting.
  variety: DurationVariety,
  positive: bool,
  years: u32,
  months: u32,
  days: u32,
  hours: u32,
  minutes: u32,
  seconds: f32
}

impl Duration {

  /// Construct a Duration of zero length of the desired variety.
  pub fn new_zero(v: DurationVariety) -> Self {
    Duration {
      variety: v,
      positive: true,
      years: 0,
      months: 0,
      days: 0,
      hours: 0,
      minutes: 0,
      seconds: 0.0
    }
  }

  /// Construct a YearMonth variety Duration with the desired sign and magnitude.
  /// It may only specify values for years and months.
  pub fn new_year_month(positive: bool, years: u32, months: u32) -> Self {
    Duration {
      variety: DurationVariety::YearMonth,
      positive: positive,
      years: years,
      months: months,
      days: 0,
      hours: 0,
      minutes: 0,
      seconds: 0.0
    }
  }

  /// Construct a Full variety Duration with the desired sign and magnitude.
  /// It may specify values for all time components.
  pub fn new(positive: bool, years: u32, months: u32, days: u32, hours: u32, minutes: u32, seconds: f32) -> Self {
    Duration {
      variety: DurationVariety::Full,
      positive: positive,
      years: years,
      months: months,
      days: days,
      hours: hours,
      minutes: minutes,
      seconds: seconds
    }
  }

  /// Construct a YearMonth variety Duration with the desired sign and magnitude.
  /// It may only specify values for days, hours, minutes, and seconds.
  /// It may not specify values for years and months.
  pub fn new_day_time(positive: bool, days: u32, hours: u32, minutes: u32, seconds: f32) -> Self {
    Duration {
      variety: DurationVariety::DayTime,
      positive: positive,
      years: 0,
      months: 0,
      days: days,
      hours: hours,
      minutes: minutes,
      seconds: seconds
    }
  }

  /// Are all components of the duration zero or not?
  pub fn is_zero(&self) -> bool {
    self.years == 0 && self.months == 0 && self.days == 0 && self.hours == 0 && self.minutes == 0 && self.seconds == 0.0
  }

  pub fn are_hms_zero(&self) -> bool {
    self.hours == 0 && self.minutes == 0 && self.seconds == 0.0
  }

  /// Convert the duration into an approximate number of seconds, 
  /// treating a year as 365 days and a month as 1/12th of a year.
  pub fn total_seconds(&self) -> f32 {
    let sign:f32 = if self.positive { 1.0 } else { -1.0 };
    sign
    * ((((self.years as f32 * 365.0 
      + (self.months as f32 / 12.0) * 365.0 
      + self.days as f32) * 24.0 
      + self.hours as f32) * 60.0) 
      + self.minutes as f32) * 60.0 
      + self.seconds
  }
}

impl fmt::Display for Duration {
  /// Format a Duration using the XPath syntax.
  /// 
  ///   P2Y3M means 2 years, 3 months
  ///   P1D means one day
  ///   PT30M15S means 30 minutes and 15 seconds
  ///   P2DT5H means 2 days and 5 hours.
  /// 
  /// Components will be omitted if zero, unless all values are zero. 
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    if self.is_zero() {
      return match self.variety {
        DurationVariety::YearMonth => write!(f, "P0Y"),
        DurationVariety::DayTime => write!(f, "P0D"),
        DurationVariety::Full => write!(f, "P0D")
      }
    }

    let sign = if self.positive { String::new() } else { "-".to_string() }; 
    let y = if self.years > 0 { format!("{}Y", self.years) } else { String::new() };
    let mo = if self.months > 0 { format!("{}M", self.months) } else { String::new() };
    let d = if self.days > 0 { format!("{}D", self.days) } else { String::new() };
    let t = if self.are_hms_zero() { String::new() } else { "T".to_string() };
    let h = if self.hours > 0 { format!("{}H", self.hours) } else { String::new() };
    let mi = if self.minutes > 0 { format!("{}M", self.minutes) } else { String::new() };
    let s = if self.seconds > 0.0 { format!("{}S", self.seconds) } else { String::new() };
    write!(f, "{}P{}{}{}{}{}{}{}", sign, y, mo, d, t, h, mi, s)
  }
}

impl PartialEq for Duration {
  fn eq(&self, other: &Self) -> bool {
      self.total_seconds() == other.total_seconds()
  }
}
impl Eq for Duration {}

impl PartialOrd for Duration {
  fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
    self.total_seconds().partial_cmp(&other.total_seconds())
  }
}

#[cfg(test)]
mod tests {
  use super::Duration;

  #[test]
  fn test_year_month_to_string() {
    assert_eq!("P1Y3M", Duration::new_year_month(true, 1u32, 3u32).to_string());
    assert_eq!("P2Y", Duration::new_year_month(true, 2u32, 0u32).to_string());
    assert_eq!("P6M", Duration::new_year_month(true, 0u32, 6u32).to_string());
    assert_eq!("-P4Y20M", Duration::new_year_month(false, 4u32, 20u32).to_string());
  }
  #[test]
  fn test_day_time_to_string() {
    assert_eq!("P1DT2H3M4.5S", Duration::new_day_time(true, 1u32, 2u32, 3u32, 4.5).to_string());
    assert_eq!("-P5D", Duration::new_day_time(false, 5u32, 0u32, 0u32, 0.0).to_string());
    assert_eq!("-PT30M", Duration::new_day_time(false, 0u32, 0u32, 30u32, 0.0).to_string());
  }

  #[test]
  fn test_duration_eq() {
    assert_eq!(
      Duration::new_year_month(true, 1u32, 12u32), 
      Duration::new_year_month(true, 2u32, 0u32), 
    );
  }
}
