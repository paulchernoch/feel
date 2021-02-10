use std::fmt;
use std::cmp;
use std::ops;
use std::str::FromStr;
use super::duration_parser::parse_duration;

#[derive(Debug, Copy, Clone, PartialEq)]
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

  /// Construct a Duration from signed values, where some parts may be positive and others negative.
  /// The resulting Duration will have a single sign, possibly reformulating the values in canonical form.
  /// In canonical form, there are never more than 60 seconds, or more than 60 minutes, or more than 24 hours, etc.
  /// If the canonical transform is required, months will equal 1/12th of a 365 day year in seconds. 
  pub fn new_from_signed(variety: DurationVariety, years: i32, months: i32, days: i32, hours: i32, minutes: i32, seconds: f32) -> Duration {
    let mut nonnegative_count = 0_u32;
    let mut nonpositive_count = 0_u32;
    if years >= 0 { nonnegative_count += 1; }
    if years <= 0 { nonpositive_count += 1; }
    if months >= 0 { nonnegative_count += 1; }
    if months <= 0 { nonpositive_count += 1; }
    if days >= 0 { nonnegative_count += 1; }
    if days <= 0 { nonpositive_count += 1; }
    if hours >= 0 { nonnegative_count += 1; }
    if hours <= 0 { nonpositive_count += 1; }
    if minutes >= 0 { nonnegative_count += 1; }
    if minutes <= 0 { nonpositive_count += 1; }
    if seconds >= 0.0 { nonnegative_count += 1; }
    if seconds <= 0.0 { nonpositive_count += 1; }
    if nonnegative_count == 6 {
      // Make a positive duration
      match variety {
        DurationVariety::Full => return Duration::new(true, years as u32, months as u32, days as u32, hours as u32, minutes as u32, seconds),
        DurationVariety::YearMonth => return Duration::new_year_month(true, years as u32, months as u32),
        DurationVariety::DayTime => return Duration::new_day_time(true, days as u32, hours as u32, minutes as u32, seconds as f32)
      };
    };
    if nonpositive_count == 6 {
      // Make a negative duration
      match variety {
        DurationVariety::Full => return Duration::new(false, (-years) as u32, (-months) as u32, (-days) as u32, (-hours) as u32, (-minutes) as u32, -seconds),
        DurationVariety::YearMonth => return Duration::new_year_month(false, (-years) as u32, (-months) as u32),
        DurationVariety::DayTime => return Duration::new_day_time(false, (-days) as u32, (-hours) as u32, (-minutes) as u32, (-seconds) as f32)
      };
    };
    // Some parts are positive and some negative, so we need to create a canonical Duration.
    let mut as_seconds = ((
      ((years as f64 + months as f64 / 12.0) * 365.0 + days as f64) * 24.0 
      + hours as f64) * 60.0
      + minutes as f64) * 60.0 
      + seconds as f64;
    let positive = as_seconds >= 0.0;
    as_seconds = as_seconds.abs();
    const YEAR_SECONDS: f64 = 60.0 * 60.0 * 24.0 * 365.0;
    const MONTH_SECONDS: f64 = YEAR_SECONDS / 12.0;
    const DAY_SECONDS: f64 = 60.0 * 60.0 * 24.0;
    const HOUR_SECONDS: f64 = 60.0 * 60.0;
    let years_can = (as_seconds / YEAR_SECONDS).floor();
    as_seconds -= years_can * YEAR_SECONDS;
    let months_can = (as_seconds / MONTH_SECONDS).floor();
    as_seconds -= months_can * MONTH_SECONDS;
    let days_can = (as_seconds / DAY_SECONDS).floor();
    as_seconds -= days_can * DAY_SECONDS;
    let hours_can = (as_seconds / HOUR_SECONDS).floor();
    as_seconds -= hours_can * HOUR_SECONDS;
    let minutes_can = (as_seconds / 60.0).floor();
    as_seconds -= minutes_can * 60.0;
    let seconds_can: f64 = if as_seconds <= 0.0 { 0.0 } else { as_seconds };
    Duration::new(positive, years_can as u32, months_can as u32, days_can as u32, hours_can as u32, minutes_can as u32, seconds_can as f32)
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
    * (((
       (self.years as f32 + self.months as f32 / 12.0) * 365.0 
      + self.days as f32) * 24.0 
      + self.hours as f32) * 60.0
      + self.minutes as f32) * 60.0 
      + self.seconds
  }

  pub fn as_year_month(&self, days_in_month: f32) -> Duration {
    let days = (self.days as f32) + (self.hours as f32 + (self.minutes as f32 + self.seconds / 60.0) / 60.0) / 24.0;
    let months: u32 = (self.months as f32 + (days / days_in_month)) as u32;
    Duration::new_year_month(self.positive, self.years, months)
  }

  pub fn as_day_time(&self, days_in_month: f32) -> Duration {
    let days_float: f32 = ((self.years as f32) * 12.0 + (self.months as f32)) * days_in_month + self.days as f32;
    let days = days_float as u32;
    let hours_float: f32 = self.hours as f32 + (days_float - days as f32) * 24.0;
    let hours = hours_float as u32;
    let minutes_float: f32 = self.minutes as f32 + (hours_float - hours as f32) * 60.0;
    let minutes = minutes_float as u32;
    let seconds: f32 = self.seconds + (minutes_float - minutes as f32) * 60.0;
    Duration::new_day_time(self.positive, days, hours, minutes, seconds)
  }

  pub fn negate(&self) -> Duration {
    match self.variety {
      DurationVariety::YearMonth => Duration::new_year_month(!self.positive, self.years, self.months),
      DurationVariety::DayTime => Duration::new_day_time(!self.positive, self.days, self.hours, self.minutes, self.seconds),
      DurationVariety::Full => Duration::new(! self.positive, self.years, self.months, self.days, self.hours, self.minutes, self.seconds)
    }
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

impl FromStr for Duration {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
      parse_duration(s, DurationVariety::Full)
  }
}

impl ops::Add<Duration> for Duration {
  type Output = Duration;

  /// Add two Durations and return a new Duration, 
  /// using the same variety for the sum as the addends if they match,
  /// but changing the variety to Full if the addends have differing varieties.
  fn add(self, rhs: Duration) -> Duration {
    let variety = match (self.variety, rhs.variety) {
      (DurationVariety::DayTime, DurationVariety::DayTime) => DurationVariety::DayTime,
      (DurationVariety::YearMonth, DurationVariety::YearMonth) => DurationVariety::YearMonth,
      _ => DurationVariety::Full
    };
    let left_sign = if self.positive { 1 } else { -1 };
    let right_sign = if rhs.positive { 1 } else { -1 };
    let yr = (self.years as i32 * left_sign) + (rhs.years as i32 * right_sign);
    let mo = (self.months as i32 * left_sign) + (rhs.months as i32 * right_sign);
    let dy = (self.days as i32 * left_sign) + (rhs.days as i32 * right_sign);
    let hr = (self.hours as i32 * left_sign) + (rhs.hours as i32 * right_sign);
    let mi = (self.minutes as i32 * left_sign) + (rhs.minutes as i32 * right_sign);
    let sc = (self.seconds * (left_sign as f32)) + (rhs.seconds * (right_sign as f32));
    Duration::new_from_signed(variety, yr, mo, dy, hr, mi, sc)
  }
}

impl ops::Sub<Duration> for Duration {
  type Output = Duration;

  /// Add two Durations and return a new Duration, 
  /// using the same variety for the sum as the addends if they match,
  /// but changing the variety to Full if the addends have differing varieties.
  fn sub(self, rhs: Duration) -> Duration {
    self + rhs.negate()
  }
}


#[cfg(test)]
mod tests {
  use super::Duration;
  use std::str::FromStr;

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

  #[test]
  fn test_year_month_duration_add() {
    assert_eq!(
      Duration::new_year_month(true, 3u32, 5u32), 
      Duration::new_year_month(true, 1u32, 2u32) + Duration::new_year_month(true, 2u32, 3u32), 
    );
    assert_eq!(
      Duration::new_year_month(true, 1u32, 1u32), 
      Duration::new_year_month(false, 1u32, 2u32) + Duration::new_year_month(true, 2u32, 3u32), 
    );
  }

  #[test]
  fn test_day_time_duration_add_without_carry() {
    assert_eq!(
      Duration::new_day_time(true, 2_u32, 10_u32, 5_u32, 3.0), 
      Duration::new_day_time(true, 3_u32, 12_u32, 20_u32, 5.5) + Duration::new_day_time(false, 1_u32, 2_u32, 15_u32, 2.5), 
    );
  }

  /// Add a negative one second duration to a one day duration and expect 23 hrs, 59 minutes, 59 seconds
  #[test]
  fn test_day_time_duration_add_with_carry() {
    assert_eq!(
      Duration::new_day_time(true, 0_u32, 23_u32, 59_u32, 59.0), 
      Duration::new_day_time(true, 1_u32, 0_u32, 0_u32, 0.0) + Duration::new_day_time(false, 0_u32, 0_u32, 0_u32, 1.0), 
    );
  }

  /// Subtract a one month duration from a one year duration and expect 11 months
  #[test]
  fn test_year_month_duration_sub_with_carry() {
    assert_eq!(
      Duration::new_year_month(true, 0_u32, 11_u32), 
      Duration::new_year_month(true, 1_u32, 0_u32) - Duration::new_year_month(true, 0_u32, 1_u32), 
    );
  }

  #[test]
  fn test_as_year_month() {
    assert_eq!(
      Duration::new_year_month(true, 1u32, 0u32), 
      Duration::new(true, 0u32, 11u32, 29u32, 24u32, 0u32, 0.0).as_year_month(30.0), 
    );
  }

  #[test]
  fn test_as_day_time() {
    assert_eq!(
      Duration::new_day_time(true, 30_u32, 9_u32, 36_u32, 0.0), 
      Duration::new_year_month(true, 0_u32, 1_u32).as_day_time(30.4), 
    );
  }

  #[test]
  fn test_from_str() {
    assert_eq!(
      Duration::new_year_month(true, 1u32, 3u32), 
      Duration::from_str("P1Y3M").unwrap(), 
    );
  }
}
