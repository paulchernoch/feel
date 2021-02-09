use crate::pest::Parser;
use std::result::Result;
use pest::iterators::{Pairs, Pair};
use super::duration::{DurationVariety, Duration};

#[derive(Parser)]
#[grammar = "parsing/duration.pest"]
pub struct DurationParser;

/// Convert the Pair into an unsigned number, after stripping off the trailing letter (Y, M, D, H, M or S).
fn get_integer<'a>(p: &'a Pair<Rule>) -> u32 {
  let mut s = p.as_str().to_string();
  s.pop();
  s.parse::<u32>().unwrap()
}

/// Convert the Pair into a float, after stripping off the trailing letter (Y, M, D, H, M or S).
fn get_float<'a>(p: &'a Pair<Rule>) -> f32 {
  let mut s = p.as_str().to_string();
  s.pop();
  s.parse::<f32>().unwrap()
}

pub fn parse_duration(duration_string: &str, variety: DurationVariety) -> Result<Duration, String> {
  let parse_result: Result<Pairs<Rule>, pest::error::Error<Rule>> = DurationParser::parse(Rule::date_time, duration_string);
  match parse_result {
    Result::Err(e) => return Result::Err(format!("Error parsing {} into a Duration is {:?}", duration_string, e)),
    Result::Ok(pairs) => {
      let mut positive = true;
      let mut yr = 0_u32;
      let mut mo = 0_u32;
      let mut dy = 0_u32;
      let mut hr = 0_u32;
      let mut mi = 0_u32;
      let mut sc = 0.0;

      for pair in pairs {
        for inner_pair in pair.into_inner() {
          match inner_pair.as_rule() {
            Rule::sign => positive = false,
            Rule::year => yr = get_integer(&inner_pair),
            Rule::month => mo = get_integer(&inner_pair),
            Rule::day => dy = get_integer(&inner_pair),
            Rule::hour => hr = get_integer(&inner_pair),
            Rule::minute => mi = get_integer(&inner_pair),
            Rule::second => sc = get_float(&inner_pair),
            _ => unreachable!()
          };
        }
      }
      let duration = match variety {
        DurationVariety::Full => Duration::new(positive, yr, mo, dy, hr, mi, sc),
        DurationVariety::DayTime => Duration::new_day_time(positive, dy, hr, mi, sc),
        DurationVariety::YearMonth => Duration::new_year_month(positive, yr, mo)
      };
      return Result::Ok(duration);
    }
  }

}

#[cfg(test)]
mod tests {
  use crate::parsing::duration::{DurationVariety, Duration};
  use super::parse_duration;
  #[test]
  fn test_parse_day_time_duration() {
    assert_eq!(
      Duration::new_day_time(true, 5_u32, 0_u32, 30_u32, 0.0),
      parse_duration("P5DT30M", DurationVariety::DayTime).unwrap()
    );
  }

  #[test]
  fn test_parse_year_month_duration() {
    assert_eq!(
      Duration::new_year_month(false, 2_u32, 6_u32),
      parse_duration("-P2Y6M", DurationVariety::YearMonth).unwrap()
    );
  }

  /// Show that Durations with different property values can be equal 
  /// if they refer to the same length of time.
  #[test]
  fn test_equivalent_year_month_durations() {
    // Verify that 1 year matches 12 months
    assert_eq!(
      Duration::new_year_month(true, 1_u32, 0_u32),
      parse_duration("P12M", DurationVariety::YearMonth).unwrap()
    );
  }
}
