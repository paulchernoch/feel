use std::fmt;
use std::cmp;
use std::ops;
use std::str::FromStr;
use std::hash::{Hash, Hasher};
use chrono::Duration as ChronoDuration;
use chrono::{NaiveDate, Date, NaiveDateTime, DateTime, NaiveTime, Datelike, Timelike, offset::TimeZone};
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

// NOTE: Cannot derive Hash for Duration because equivalent Durations may have
//       different representations.

/// Measures a duration of time in years, months, days, hours, minutes and seconds.
/// Supports arithmetic like adding, subtracting and dividing pairs of Durations,
/// or multiplying and dividing by scalars.
/// Durations may be parsed from strings conforming to the XPath syntax.
/// 
/// NOTE: The Duration class form Chrono would be adequate if not for the fact that it
/// does not support durations specified as months, which us understandable, 
/// as months vary in length and this gets messy.
#[derive(Debug, Copy, Clone)]
pub struct Duration {
  /// Restricts which properties may be set to a nonzero value and affects the string formatting.
  variety: DurationVariety,
  /// If true, the Duration is a positive value, otherwise a negative value.
  positive: bool,
  years: u32,
  months: u32,
  days: u32,
  hours: u32,
  minutes: u32,
  seconds: f32
}

pub const DAYS_PER_YEAR: f64 = 365.25;
pub const DAYS_PER_MONTH: f64 = 365.25 / 12.0;
const SECONDS_PER_DAY: f64 = 24.0 * 60.0 * 60.0;
const NANOSECONDS_PER_SECOND: f64 = 1000000000.0;

impl Duration {

  pub fn try_year_month(duration_string: &str) -> Result<Self, String> {
    match Duration::from_str(duration_string) {
      Ok(duration) => {
        Ok(duration.as_year_month(DAYS_PER_MONTH as f32))
      },
      Err(err_string) => Err(err_string)
    }
  }

  pub fn try_day_time(duration_string: &str) -> Result<Self, String> {
    match Duration::from_str(duration_string) {
      Ok(duration) => {
        Ok(duration.as_day_time(DAYS_PER_MONTH as f32))
      },
      Err(err_string) => Err(err_string)
    }
  }

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
  /// If the canonical transform is required, months will equal 1/12th of a 365.25 day year in seconds. 
  pub fn new_from_signed(variety: DurationVariety, years: i32, months: i32, days: i32, hours: i32, minutes: i32, seconds: f32) -> Self {
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
    let as_seconds: f64 = ((
      ((years as f64 + months as f64 / 12.0) * DAYS_PER_YEAR + days as f64) * 24.0 
      + hours as f64) * 60.0
      + minutes as f64) * 60.0 
      + seconds as f64;
      Duration::new_from_seconds(as_seconds)
  }

  /// Construct a canonical Duration given a number of seconds,
  /// such that if the number overflows 60 the extra is converted to minutes.
  /// If minutes overflows 60, the extra is converted to hours.
  /// etc. Months are taken as 365.25/12 days.
  pub fn new_from_seconds(seconds: f64) -> Self {
    let positive = seconds >= 0.0;
    let mut as_seconds = seconds.abs();
    const YEAR_SECONDS: f64 = 60.0 * 60.0 * 24.0 * DAYS_PER_YEAR;
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

  /// Normalized number of years
  pub fn get_years(&self) -> i32 { (self.years + (self.months / 12)) as i32 * if self.positive {1} else {-1} }

  /// Normalized number of months (in the range [-11,11])
  pub fn get_months(&self) -> i32 { (self.months % 12) as i32 * if self.positive {1} else {-1} }
  pub fn get_days(&self) -> i32 { self.total_days() }

  /// Normalized hours (in the range [-23,23])
  pub fn get_hours(&self) -> i32 { 
    let (h, m, s) = (self.hours as i32, self.minutes as i32, self.seconds as i32);
    let abs_hours = (h + ((m + s/60) / 60)) % 24;
    abs_hours * if self.positive {1} else {-1} 
  }

  /// Normalized minutes (in the range [-59,59])
  pub fn get_minutes(&self) -> i32 { 
    let (m, s) = (self.minutes as i32, self.seconds as i32);
    let abs_minutes = (m + s/60) % 60;
    abs_minutes * if self.positive {1} else {-1} 
  }

  /// Normalized seconds (in the range [-59,59])
  pub fn get_seconds(&self) -> i32 { self.seconds as i32 % 60 as i32 * if self.positive {1} else {-1} }

  pub fn new_from_nanoseconds(nano_seconds: i64) -> Self {
    let seconds = nano_seconds as f64 / 1.0e9;
    Self::new_from_seconds(seconds)
  }

  pub fn get_variety(&self) -> DurationVariety { self.variety }

  /// Compute minuend minus subtrahend, the date difference as a YearMonth Duration (rounded to the nearest month).
  pub fn date_difference_months(minuend: NaiveDate, subtrahend: NaiveDate) -> Duration {
    let days_difference = minuend.signed_duration_since(subtrahend).num_days();
    Duration::new_from_signed(DurationVariety::YearMonth, 0, 0, days_difference as i32, 0, 0, 0.0)
  }

  /// Compute minuend minus subtrahend, the date difference as a DayTime Duration.
  pub fn date_difference_days(minuend: NaiveDate, subtrahend: NaiveDate) -> Duration {
    let days_difference = minuend.signed_duration_since(subtrahend).num_days();
    Duration::new_from_signed(DurationVariety::DayTime, 0, 0, days_difference as i32, 0, 0, 0.0)
  }

  /// Compute minuend minus subtrahend, the date difference as a YearMonth Duration (rounded to the nearest month).
  pub fn datetime_difference(minuend: NaiveDateTime, subtrahend: NaiveDateTime) -> Duration {
    let difference: ChronoDuration = minuend.signed_duration_since(subtrahend);
    Duration::new_from_nanoseconds(difference.num_nanoseconds().unwrap())
  }

  /// Compute minuend minus subtrahend, the time difference as a Duration.
  pub fn time_difference(minuend: NaiveTime, subtrahend: NaiveTime) -> Duration {
    let difference: ChronoDuration = minuend.signed_duration_since(subtrahend);
    Duration::new_from_nanoseconds(difference.num_nanoseconds().unwrap())
  }

  /// Are all components of the duration zero or not?
  pub fn is_zero(&self) -> bool {
    self.years == 0 && self.months == 0 && self.days == 0 && self.hours == 0 && self.minutes == 0 && self.seconds == 0.0
  }

  /// Does the Duration represent a negative quantity?
  pub fn is_negative(&self) -> bool {
    !self.positive && !self.is_zero()
  }

  pub fn are_hms_zero(&self) -> bool {
    self.hours == 0 && self.minutes == 0 && self.seconds == 0.0
  }

  pub fn are_year_month_zero(&self) -> bool {
    self.years == 0 && self.months == 0
  }

  /// Convert the duration into an approximate and signed number of seconds, 
  /// treating a year as 365 days and a month as 1/12th of a year.
  pub fn total_seconds(&self) -> f32 {
    let sign:f32 = if self.positive { 1.0 } else { -1.0 };
    sign
    * (((
       (self.years as f32 + self.months as f32 / 12.0) * (DAYS_PER_YEAR as f32)
      + self.days as f32) * 24.0 
      + self.hours as f32) * 60.0
      + self.minutes as f32) * 60.0 
      + self.seconds
  }

  pub fn total_days(&self) -> i32 {
    let seconds = self.total_seconds() as i32;
    const SECONDS_PER_DAY: i32 = 86400;
    seconds / SECONDS_PER_DAY
  }

  pub fn as_year_month(&self, days_in_month: f32) -> Duration {
    let days = (self.days as f32) + (self.hours as f32 + (self.minutes as f32 + self.seconds / 60.0) / 60.0) / 24.0;
    let months: u32 = (self.months as f32 + (days / days_in_month)) as u32;
    Duration::new_year_month(self.positive, self.years, months)
  }

  /// Convert to a DayTime variety Duration by piling all the years, and months into the days field.
  /// For example, if the Duration was for one year, it would become a 365.25 day Duration.
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

impl Hash for Duration {
  fn hash<H: Hasher>(&self, state: &mut H) {
    ((self.total_seconds() * 1000000.0_f32) as i128).hash(state);
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

impl Ord for Duration {
  fn cmp(&self, other: &Self) -> cmp::Ordering {
    self.partial_cmp(other).unwrap()
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

/// Create a new Date in the same month as the given date
/// but with the day changed to the given day (one-based), 
/// unless that month has fewer days, in which case
/// use the last day of that month.
fn set_day_in_current_month<D>(date: &D, try_day: u32) -> D where D: Datelike {
  let mut day = try_day;
  while day >= 1_u32 {
    match date.with_day(day) {
      Some(d) => { return d },
      None => { day -= 1_u32; }
    };
  } 
  panic!("set_day_or_earlier cannot find valid date");
}

/// Create a new Date in the month following the given date (rolling the year if necessary)
/// but with the day changed to the given day (one-based), 
/// unless that month has fewer days, in which case
/// use the last day of that month.
fn set_day_in_next_month<D>(date: &D, try_day: u32) -> D where D: Datelike {
  let month = date.month();
  let mut first_of_month = date.with_day(1).unwrap();
  first_of_month = if month == 12_u32 {
    first_of_month.with_year(date.year() + 1).unwrap().with_month(1).unwrap()
  }
  else {
    first_of_month.with_month(month + 1_u32).unwrap()
  };
  set_day_in_current_month(&first_of_month, try_day)
}

/// Create a new Date in the month preceding the given date (rolling the year if necessary)
/// but with the day changed to the given day (one-based), 
/// unless that month has fewer days, in which case
/// use the last day of that month.
fn set_day_in_previous_month<D>(date: &D, try_day: u32) -> D where D: Datelike {
  let month = date.month();
  let mut first_of_month = date.with_day(1).unwrap();
  first_of_month = if month == 1_u32 {
    first_of_month.with_year(date.year() - 1).unwrap().with_month(12_u32).unwrap()
  }
  else {
    first_of_month.with_month(month - 1_u32).unwrap()
  };
  set_day_in_current_month(&first_of_month, try_day)
}

fn add_datelike_and_duration<Dt>(date: &Dt, rhs: Duration) -> Dt
where Dt: Datelike + Clone
      + ops::Add<ChronoDuration, Output = Dt>
{
  let days = rhs.total_days();
  let chrono_duration = ChronoDuration::days(days as i64);
  let approx_date = date.clone() + chrono_duration;
  let day = date.day();
  let days_0 = approx_date.num_days_from_ce();

  match (days.cmp(&0), rhs.variety) {
    (cmp::Ordering::Equal, _) => date.clone(),
    (_, DurationVariety::YearMonth) => {
      let d1 = set_day_in_current_month(&approx_date, day);
      let d2 = set_day_in_next_month(&approx_date, day);
      let d3 = set_day_in_previous_month(&approx_date, day);
      let delta_1 = (d1.num_days_from_ce() - days_0).abs();
      let delta_2 = (d2.num_days_from_ce() - days_0).abs();
      let delta_3 = (d3.num_days_from_ce() - days_0).abs();
      if delta_1 <= delta_2 && delta_1 <= delta_3 { d1 }
      else if delta_2 <= delta_3 { d2 }
      else { d3 }
    },
    _ => approx_date
  }
}

fn add_timelike_and_duration<Tm>(time: &Tm, rhs: Duration) -> Tm 
where Tm: Timelike + Clone
      + ops::Add<ChronoDuration, Output = Tm>
{
  let seconds = time.num_seconds_from_midnight() as f64 + (time.nanosecond() as f64) / NANOSECONDS_PER_SECOND;
  let delta_seconds = rhs.total_seconds() as f64;
  let mut new_seconds = seconds + delta_seconds;
  if new_seconds < 0.0 {
    new_seconds = SECONDS_PER_DAY + new_seconds;
  }

  if new_seconds > SECONDS_PER_DAY {
    new_seconds = new_seconds % SECONDS_PER_DAY;
  }
  let seconds_from_midnight = new_seconds.floor();
  let nanoseconds = (((new_seconds - seconds_from_midnight) * NANOSECONDS_PER_SECOND).round()) as u32;
  let s = (seconds_from_midnight % 60.0) as u32;
  let m = ((seconds_from_midnight % 3600.0) / 60.0).floor() as u32;
  let h = ((seconds_from_midnight % SECONDS_PER_DAY) / 3600.0).floor() as u32;
  let new_time = time
    .with_hour(h).unwrap()
    .with_minute(m).unwrap()
    .with_second(s).unwrap()
    .with_nanosecond(nanoseconds).unwrap();
  return new_time;
}

//            NaiveDate or Date and Duration arithmetic

/// &NaiveDate + Duration
impl ops::Add<Duration> for &NaiveDate {
  type Output = NaiveDate;
  fn add(self, rhs: Duration) -> Self::Output {
    add_datelike_and_duration(self, rhs)
  }
}

/// NaiveDate + Duration
impl ops::Add<Duration> for NaiveDate {
  type Output = NaiveDate;
  fn add(self, rhs: Duration) -> Self::Output {
    add_datelike_and_duration(&self, rhs)
  }
}

/// &Date<Tz> + Duration
impl<Tz: TimeZone> ops::Add<Duration> for &Date<Tz> {
  type Output = Date<Tz>;
  fn add(self, rhs: Duration) -> Self::Output {
    add_datelike_and_duration(self, rhs)
  }
}

/// Date<Tz> + Duration
impl<Tz: TimeZone> ops::Add<Duration> for Date<Tz> {
  type Output = Date<Tz>;
  fn add(self, rhs: Duration) -> Self::Output {
    add_datelike_and_duration(&self, rhs)
  }
}

/// &Date<Tz> - Duration
impl<Tz: TimeZone> ops::Sub<Duration> for &Date<Tz> {
  type Output = Date<Tz>;
  fn sub(self, rhs: Duration) -> Self::Output {
    add_datelike_and_duration(self, -rhs)
  }
}

/// Date<Tz> - Duration
impl<Tz: TimeZone> ops::Sub<Duration> for Date<Tz> {
  type Output = Date<Tz>;
  fn sub(self, rhs: Duration) -> Self::Output {
    add_datelike_and_duration(&self, -rhs)
  }
}

/// &NaiveDate - Duration
impl ops::Sub<Duration> for &NaiveDate {
  type Output = NaiveDate;
  fn sub(self, rhs: Duration) -> Self::Output {
    add_datelike_and_duration(self, -rhs)
  }
}

/// NaiveDate - Duration
impl ops::Sub<Duration> for NaiveDate {
  type Output = NaiveDate;
  fn sub(self, rhs: Duration) -> Self::Output {
    add_datelike_and_duration(&self, -rhs)
  }
}

//            End of Date and Duration arithmetic

//            NaiveDateTime or DateTime and Duration arithmetic

/// &NaiveDateTime + Duration
impl ops::Add<Duration> for &NaiveDateTime {
  type Output = NaiveDateTime;
  fn add(self, rhs: Duration) -> Self::Output {
    add_datelike_and_duration(self, rhs)
  }
}

/// NaiveDateTime + Duration
impl ops::Add<Duration> for NaiveDateTime {
  type Output = NaiveDateTime;
  fn add(self, rhs: Duration) -> Self::Output {
    add_datelike_and_duration(&self, rhs)
  }
}

/// &DateTime<Tz> + Duration
impl<Tz: TimeZone> ops::Add<Duration> for &DateTime<Tz> {
  type Output = DateTime<Tz>;
  fn add(self, rhs: Duration) -> Self::Output {
    add_datelike_and_duration(self, rhs)
  }
}

/// DateTime<Tz> + Duration
impl<Tz: TimeZone> ops::Add<Duration> for DateTime<Tz> {
  type Output = DateTime<Tz>;
  fn add(self, rhs: Duration) -> Self::Output {
    add_datelike_and_duration(&self, rhs)
  }
}

/// &NaiveDateTime - Duration
impl ops::Sub<Duration> for &NaiveDateTime {
  type Output = NaiveDateTime;
  fn sub(self, rhs: Duration) -> Self::Output {
    add_datelike_and_duration(self, -rhs)
  }
}

/// NaiveDateTime - Duration
impl ops::Sub<Duration> for NaiveDateTime {
  type Output = NaiveDateTime;
  fn sub(self, rhs: Duration) -> Self::Output {
    add_datelike_and_duration(&self, -rhs)
  }
}

/// &DateTime<Tz> - Duration
impl<Tz: TimeZone> ops::Sub<Duration> for &DateTime<Tz> {
  type Output = DateTime<Tz>;
  fn sub(self, rhs: Duration) -> Self::Output {
    add_datelike_and_duration(self, -rhs)
  }
}

/// DateTime<Tz> - Duration
impl<Tz: TimeZone> ops::Sub<Duration> for DateTime<Tz> {
  type Output = DateTime<Tz>;
  fn sub(self, rhs: Duration) -> Self::Output {
    add_datelike_and_duration(&self, -rhs)
  }
}

//      End of DateTime and Duration arithmetic  

//            NaiveTime and Duration arithmetic

/// &NaiveTime + Duration
impl ops::Add<Duration> for &NaiveTime {
  type Output = NaiveTime;
  fn add(self, rhs: Duration) -> Self::Output {
    add_timelike_and_duration(self, rhs)
  }
}

/// NaiveTime + Duration
impl ops::Add<Duration> for NaiveTime {
  type Output = NaiveTime;
  fn add(self, rhs: Duration) -> Self::Output {
    add_timelike_and_duration(&self, rhs)
  }
}

/// &NaiveDateTime - Duration
impl ops::Sub<Duration> for &NaiveTime {
  type Output = NaiveTime;
  fn sub(self, rhs: Duration) -> Self::Output {
    add_timelike_and_duration(self, -rhs)
  }
}

/// NaiveDateTime - Duration
impl ops::Sub<Duration> for NaiveTime {
  type Output = NaiveTime;
  fn sub(self, rhs: Duration) -> Self::Output {
    add_timelike_and_duration(&self, -rhs)
  }
}


//      End of NaiveTime and Duration arithmetic

/// Subtract two Durations to get a Duration
impl ops::Sub<Duration> for Duration {
  type Output = Duration;
  fn sub(self, rhs: Duration) -> Duration {
    self + rhs.negate()
  }
}

/// Divide two Durations to get a scalar.
impl ops::Div<Duration> for Duration {
  type Output = f32;

  fn div(self, rhs: Self) -> Self::Output {
      if rhs.is_zero() {
          return f32::NAN;
      };
      self.total_seconds()  / rhs.total_seconds()
  }
}

impl ops::Div<f32> for Duration {
  type Output = Duration;

  /// Divide a Duration by a scalar and derive a new Duration.
  /// A YearMonth type Duration will remain so, and round down to a whole month. 
  /// Months will be treated as one twelfth of a year.
  fn div(self, rhs: f32) -> Self::Output {
      if rhs == 0.0 {
          panic!("Cannot divide `Duration` by zero!");
      };
      let positive = self.positive == (rhs >= 0.0);
      let seconds: f64 = self.total_seconds() as f64 / rhs as f64;
      
      match self.variety {
        DurationVariety::YearMonth => {
          let total_months: u32 = ((self.years as f32 * 12.0 + self.months as f32 + self.days as f32 * 12.0 / DAYS_PER_YEAR as f32) / rhs.abs()).floor() as u32;
          let years = total_months / 12;
          let months = total_months % 12;
          Duration::new_year_month(positive, years, months)
        },
        DurationVariety::DayTime => Duration::new_from_seconds(seconds).as_day_time(DAYS_PER_YEAR as f32/12.0),
        DurationVariety::Full => Duration::new_from_seconds(seconds)
      }
  }
}

impl ops::Mul<f32> for Duration {
  type Output = Duration;

  /// Multiply a Duration by a scalar and derive a new Duration.
  /// A YearMonth type Duration will remain so, and round down to a whole month. 
  /// Months will be treated as one twelfth of a year.
  fn mul(self, rhs: f32) -> Self::Output {
      if rhs == 0.0 {
        return Duration::new_zero(self.variety);
      };
      if rhs == 1.0 {
        return self;
      }
      let positive = self.positive == (rhs >= 0.0);
      let seconds: f64 = self.total_seconds() as f64 * rhs as f64;
      
      match self.variety {
        DurationVariety::YearMonth => {
          let total_months: u32 = ((self.years as f32 * 12.0 + self.months as f32 + self.days as f32 * 12.0 / 365.0) * rhs.abs()).floor() as u32;
          let years = total_months / 12;
          let months = total_months % 12;
          Duration::new_year_month(positive, years, months)
        },
        DurationVariety::DayTime => Duration::new_from_seconds(seconds).as_day_time(DAYS_PER_YEAR as f32/12.0),
        DurationVariety::Full => Duration::new_from_seconds(seconds)
      }
  }
}

impl ops::Mul<Duration> for f32 {
  type Output = Duration;

  /// Multiply a scalar by a Duration and derive a new Duration.
  /// A YearMonth type Duration will remain so, and round down to a whole month. 
  /// Months will be treated as one twelfth of a year.
  fn mul(self, rhs: Duration) -> Self::Output {
      if self == 0.0 {
        return Duration::new_zero(rhs.variety);
      };
      if self == 1.0 {
        return rhs;
      }
      let positive = rhs.positive == (self >= 0.0);
      let seconds: f64 = rhs.total_seconds() as f64 * self as f64;
      
      match rhs.variety {
        DurationVariety::YearMonth => {
          let total_months: u32 = ((rhs.years as f32 * 12.0 + rhs.months as f32 + rhs.days as f32 * 12.0 / 365.0) * self.abs()).floor() as u32;
          let years = total_months / 12;
          let months = total_months % 12;
          Duration::new_year_month(positive, years, months)
        },
        DurationVariety::DayTime => Duration::new_from_seconds(seconds).as_day_time(DAYS_PER_YEAR as f32/12.0),
        DurationVariety::Full => Duration::new_from_seconds(seconds)
      }
  }
}

impl ops::Neg for Duration {
  type Output = Duration;

  fn neg(self) -> Duration {
      self.negate()
  }
}

impl From<ChronoDuration> for Duration {
  fn from(item: ChronoDuration) -> Self {
    let nano_seconds = item.num_nanoseconds().unwrap();
    Duration::new_from_nanoseconds(nano_seconds)
  }
}

impl From<Duration> for ChronoDuration {
  fn from(item: Duration) -> ChronoDuration {
    let nanoseconds = (item.total_seconds() as f64 * 1.0e9).round() as i64;
    ChronoDuration::nanoseconds(nanoseconds)
  }
}

/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
  use super::Duration;
  use super::{DAYS_PER_YEAR, DAYS_PER_MONTH};
  use std::str::FromStr;
  use chrono::Duration as ChronoDuration;
  use chrono::{Utc, TimeZone, DateTime, NaiveTime};

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
  fn test_duration_by_duration_div() {
    assert_eq!(
      DAYS_PER_YEAR as f32, 
      Duration::new_year_month(true, 1_u32, 0_u32) / Duration::new_day_time(true, 1_u32, 0_u32, 0_u32, 0.0), 
    );
  }

  #[test]
  fn test_duration_by_scalar_div() {
    assert_eq!(
      Duration::new_year_month(true, 0_u32, 9_u32), 
      Duration::new_year_month(true, 1_u32, 6_u32) / 2.0, 
    );
    // Divide a year down to an hour
    assert_eq!(
      Duration::from_str("PT1H").unwrap(), 
      Duration::from_str("P1Y").unwrap() / (DAYS_PER_YEAR as f32 * 24.0), 
    );

    // Divide by one twelfth, the same as multiplying by 12.
    assert_eq!(
      Duration::from_str("P1Y").unwrap(), 
      Duration::from_str("P1M").unwrap() / (1.0/12.0), 
    );
  }

  #[test]
  fn test_duration_by_scalar_mul() {
    assert_eq!(
      Duration::new_year_month(true, 3_u32, 0_u32), 
      Duration::new_year_month(true, 1_u32, 6_u32) * 2.0, 
    );
    // Reduce a year down to an hour
    assert_eq!(
      Duration::from_str("PT1H").unwrap(), 
      Duration::from_str("P1Y").unwrap() * (1.0 / (DAYS_PER_YEAR as f32 * 24.0)), 
    );

    // Multiply by 12.
    assert_eq!(
      Duration::from_str("P1Y").unwrap(), 
      Duration::from_str("P1M").unwrap() * 12.0, 
    );
  }

  #[test]
  fn test_scalar_by_duration_mul() {
    assert_eq!(
      Duration::new_year_month(true, 3_u32, 0_u32), 
      2.0 * Duration::new_year_month(true, 1_u32, 6_u32), 
    );
    // Reduce a year down to an hour
    assert_eq!(
      Duration::from_str("PT1H").unwrap(), 
      (1.0 / (DAYS_PER_YEAR as f32 * 24.0)) * Duration::from_str("P1Y").unwrap(), 
    );

    // Multiply by 12.
    assert_eq!(
      Duration::from_str("P1Y").unwrap(), 
      12.0 * Duration::from_str("P1M").unwrap(), 
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


  #[test]
  fn test_duration_round_trip() {
    let ten_seconds_a = Duration::from_str("PT10S").unwrap();
    let ten_seconds_b: ChronoDuration = ten_seconds_a.into();
    let ten_seconds_c: Duration = ten_seconds_b.into(); 
    assert_eq!(
      ten_seconds_a, 
      ten_seconds_c, 
    );
  }

  #[test]
  fn test_add_date_and_duration() {
    let duration = Duration::from_str("P2Y8M").unwrap().as_year_month(DAYS_PER_YEAR as f32/ 12.0);

    // Rollover into January
    assert_eq!(
      Utc.ymd(2018, 1, 15), 
      &Utc.ymd(2015, 5, 15) + duration, 
    );
    // Advance to month with fewer days
    assert_eq!(
      Utc.ymd(2017, 9, 30), 
      Utc.ymd(2015, 1, 31) + duration, 
    );
  }

  #[test]
  fn test_sub_date_and_duration() {
    let duration = Duration::from_str("P1Y3M").unwrap().as_year_month(DAYS_PER_YEAR as f32/ 12.0);
    assert_eq!(
      Utc.ymd(2018, 11, 12), 
      Utc.ymd(2020, 2, 12) - duration, 
    );
  }
  // pub fn parse_from_rfc3339(s: &str) -> ParseResult<DateTime<FixedOffset>>
  // Parses an RFC 3339 and ISO 8601 date and time string such as 1996-12-19T16:39:57-08:00, then returns a new DateTime with a parsed FixedOffset.
  #[test]
  fn test_add_datetime_and_duration() {
    let duration = Duration::from_str("P2Y8M").unwrap().as_year_month(DAYS_PER_YEAR as f32/ 12.0);

    // Rollover into January
    let dt_a = DateTime::parse_from_rfc3339("2015-05-15T16:39:57-05:00").unwrap();
    let dt_rollover = DateTime::parse_from_rfc3339("2018-01-15T16:39:57-05:00").unwrap();
    assert_eq!(
      dt_rollover, 
      dt_a + duration, 
    );
    // Advance to month with fewer days
    let dt_b = DateTime::parse_from_rfc3339("2015-01-31T16:39:57-05:00").unwrap();
    let dt_truncate = DateTime::parse_from_rfc3339("2017-09-30T16:39:57-05:00").unwrap();
    assert_eq!(
      dt_truncate, 
      dt_b + duration, 
    );
  }

  #[test]
  fn test_time_difference() {
    let t1 = NaiveTime::from_hms(14_u32, 20_u32, 0_u32);
    let t2 = NaiveTime::from_hms(11_u32, 21_u32, 1_u32);
    let expected_positive_duration = Duration::new(true, 0_u32, 0_u32, 0_u32, 2_u32, 58_u32, 59_f32).as_day_time(DAYS_PER_MONTH as f32);
    let positive_delta = Duration::time_difference(t1, t2);
    assert_eq!(expected_positive_duration, positive_delta, "positive time difference");

    let expected_negative_duration = Duration::new(false, 0_u32, 0_u32, 0_u32, 2_u32, 58_u32, 59_f32).as_day_time(DAYS_PER_MONTH as f32);
    let negative_delta = Duration::time_difference(t2, t1);
    assert_eq!(expected_negative_duration, negative_delta, "negative time difference");

  }
}
