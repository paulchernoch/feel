use chrono::{Datelike, Timelike};
use crate::parsing::feel_value::{FeelValue,FeelType};
use crate::parsing::context::{ContextReader};
use crate::parsing::qname::QName;
use std::ops::{Bound};
use crate::parsing::execution_log::ExecutionLog;

pub fn no_such_property(prop_name: &str, value: &FeelValue) -> FeelValue {
    ExecutionLog::log(&format!("Property '{:?}' not defined for {:?} values.", prop_name,  value.get_type()));
    FeelValue::Null
}

/// The DMN Spec supports 29 properties of dates, times, durations and ranges. 
pub trait ValueProperties {
  
    // Properties only for FeelValue::Date or FeelValue::DateAndTime values. 
    // Null returned for other FeelValue variants.

    /// Year from a Date or DateTime
    fn year(&self) -> FeelValue;
  
    /// Month from a Date or DateAndTime as an integer in the range [1..12]
    fn month(&self) -> FeelValue;
  
    /// Day from a Date or DateAndTime as an integer in the range [1..31]
    fn day(&self) -> FeelValue;
  
    /// The day of the week from a Date or DateAndTime as an integer in the interval [1..7] 
    /// where 1 is Monday and 7 is Sunday (compliant with the definition in ISO 8601)
    fn weekday(&self) -> FeelValue;
  
    // ......... 
  
    // Properties only for FeelValue::Time or FeelValue::DateAndTime values. 
    // Null returned for other FeelValue variants.

    /// The hour of the day from a Time or DateAndTime as an integer in the interval [0..23]
    fn hour(&self) -> FeelValue;
  
    /// The minute of the hour from a Time or DateAndTime as an integer in the interval [0..59]
    fn minute(&self) -> FeelValue ;
  
    /// The second of the minute from a Time or DateAndTime as an integer in the interval [0..59]
    fn second(&self) -> FeelValue;
  
    fn time_offset(&self) -> FeelValue;
  
    fn timezone(&self) -> FeelValue;

    // ......... 
  
    // Properties only for FeelValue::YearMonthDuration values. Null returned for other FeelValue variants.

    /// The normalized years component of a years and months duration value as an integer.
    /// This property returns null when invoked on a days and time duration value.
    fn years(&self) -> FeelValue;
  
    /// The normalized months component of a years and months duration value. 
    /// Since the value is normalized, this property must return an integer in the interval [0..11]. 
    /// This property returns null when invoked on a days and time duration value.
    fn months(&self) -> FeelValue;
  
    // ......... 
  
    // Properties only for FeelValue::DayTimeDuration values. Null returned for other FeelValue variants.

    /// The normalized days component of a days and time duration value as an integer. 
    /// This property returns null when invoked on a years and months duration value.
    fn days(&self) -> FeelValue;
  
    /// The normalized hours component of a days and time duration value. 
    /// Since the value is normalized, this property must return an integer in the interval [0..23]. 
    /// This property returns null when invoked on a years and months duration value.
    fn hours(&self) -> FeelValue;
  
    /// The normalized minutes component of a days and time duration value. 
    /// Since the value is normalized, this property must return an integer in the interval [0..59]. 
    /// This property returns null when invoked on a years and months duration value.
    fn minutes(&self) -> FeelValue;
  
    /// The normalized seconds component of a days and time duration value. 
    /// Since the value is normalized, this property must return a decimal in the interval [0..60). 
    /// This property returns null when invoked on a years and months duration value.
    fn seconds(&self) -> FeelValue;

    // ......... 

    // Properties only for FeelValue::Range values. Null returned for other FeelValue variants.
  
    /// the start endpoint of the range
    fn start<C: ContextReader>(&self, contexts: &C) -> FeelValue;
  
    /// the end endpoint of the range
    fn end<C: ContextReader>(&self, contexts: &C) -> FeelValue;
  
    /// true if the start endpoint is included in the range
    fn start_included<C: ContextReader>(&self, contexts: &C) -> FeelValue;
  
    /// true if the end endpoint is included in the range
    fn end_included<C: ContextReader>(&self, contexts: &C) -> FeelValue;

    // ......... 

    /// Access any of the preceding properties by string name. 
    /// This intended to support the "." property acces operator of the Feel language,
    /// as used in Path expressions.
    /// If the value is a Context, retrieve a named property from the context, 
    /// which may or may not be present. If it is not present, return FeelValue::Null.
    ///   - If the FeelValue is a Context, any property name may be specified. 
    ///     If it is missing, a FeelValue::Null will be returned. 
    ///   - If the FeelValue is a Date, Time, DateAndTime, YearMonthDuration,
    ///     DayTimeDuration, or Range, a list of special properties is recognized. 
    ///     > If the property name is not in that list, a Null is returned. 
    ///     > If that property name is not relevant for that FeelType, a Null is returned.
    fn get_property<C: ContextReader>(&self, property_name: &FeelValue, contexts: &C) -> FeelValue;
} 

impl ValueProperties for FeelValue {
    fn year(&self) -> FeelValue {
        match self {
            FeelValue::Date(d) => d.year().into(),
            FeelValue::DateAndTime(d) => d.year().into(),
            _ => no_such_property("year", self)
        }
    }
  
    fn month(&self) -> FeelValue {
        match self {
            FeelValue::Date(d) => d.month().into(),
            FeelValue::DateAndTime(d) => d.month().into(),
            _ => no_such_property("month", self)
        }
    }
  
    fn day(&self) -> FeelValue {
        match self {
            FeelValue::Date(d) => d.day().into(),
            FeelValue::DateAndTime(d) => d.day().into(),
            _ => no_such_property("day", self)
        }
    }
  
    fn weekday(&self) -> FeelValue {
        match self {
            FeelValue::Date(d) => d.weekday().number_from_monday().into(),
            FeelValue::DateAndTime(d) => d.weekday().number_from_monday().into(),
            _ => no_such_property("weekday", self)
        }
    }
  
    fn hour(&self) -> FeelValue {
        match self {
            FeelValue::Time(t) => t.hour().into(),
            FeelValue::DateAndTime(t) => t.hour().into(),
            _ => no_such_property("hour", self)
        }
    }
  
    fn minute(&self) -> FeelValue {
        match self {
            FeelValue::Time(t) => t.minute().into(),
            FeelValue::DateAndTime(t) => t.minute().into(),
            _ => no_such_property("minute", self)
        }
    }
  
    fn second(&self) -> FeelValue {
        match self {
            FeelValue::Time(t) => t.second().into(),
            FeelValue::DateAndTime(t) => t.second().into(),
            _ => no_such_property("second", self)
        }
    }
  
    
    fn time_offset(&self) -> FeelValue {
        // TODO: Our times have no time zones, so the Offset is always zero - must implement time zones.
        match self {
            FeelValue::Time(_t) => FeelValue::new_duration("PT0H").unwrap(),
            FeelValue::DateAndTime(_t) => FeelValue::new_duration("PT0H").unwrap(),
            _ => no_such_property("time offset", self)
        }
    }
  
    fn timezone(&self) -> FeelValue {
        // TODO: Our times have no timezones - must implement time zones.
        match self {
            FeelValue::Time(_t) => "UTC".into(),
            FeelValue::DateAndTime(_t) => "UTC".into(),
            _ => no_such_property("timezone", self)
        }
    }

  
    fn years(&self) -> FeelValue {
        match self {
            FeelValue::YearMonthDuration(d) => d.get_years().into(),
            _ => no_such_property("years", self)
        }
    }
  
    fn months(&self) -> FeelValue {
        match self {
            FeelValue::YearMonthDuration(d) => d.get_months().into(),
            _ => no_such_property("months", self)
        }
    }
  
    fn days(&self) -> FeelValue {
        match self {
            FeelValue::DayTimeDuration(d) => d.get_days().into(),
            _ => no_such_property("days", self)
        }
    }
  
    fn hours(&self) -> FeelValue {
        match self {
            FeelValue::DayTimeDuration(d) => d.get_hours().into(),
            _ => no_such_property("hours", self)
        }
    }
  
    fn minutes(&self) -> FeelValue {
        match self {
            FeelValue::DayTimeDuration(d) => d.get_minutes().into(),
            _ => no_such_property("minutes", self)
        }
    }
  
    fn seconds(&self) -> FeelValue {
        match self {
            FeelValue::DayTimeDuration(d) => d.get_seconds().into(),
            _ => no_such_property("seconds", self)
        }
    }
  
    fn start<C: ContextReader>(&self, contexts: &C) -> FeelValue {
        match self {
            FeelValue::Range(r) => match r.start_bound(contexts) {
                Bound::Included(b) => b.clone(),
                Bound::Excluded(b) => b.clone(),
                Bound::Unbounded => FeelValue::Null
            },
            _ => no_such_property("start", self)
        }
    }
  
    fn end<C: ContextReader>(&self, contexts: &C) -> FeelValue {
        match self {
            FeelValue::Range(r) => match r.end_bound(contexts) {
                Bound::Included(b) => b.clone(),
                Bound::Excluded(b) => b.clone(),
                Bound::Unbounded => FeelValue::Null
            },
            _ => no_such_property("end", self)
        }
    }
  
    fn start_included<C: ContextReader>(&self, contexts: &C) -> FeelValue {
        match self {
            FeelValue::Range(r) => match r.start_bound(contexts) {
                Bound::Included(_) => true.into(),
                _ => false.into()
            } ,
            _ => no_such_property("start_included", self)
        }
    }
  
    fn end_included<C: ContextReader>(&self, contexts: &C) -> FeelValue {
        match self {
            FeelValue::Range(r) => match r.end_bound(contexts) {
                Bound::Included(_) => true.into(),
                _ => false.into()
            } ,
            _ => no_such_property("end_included", self)
        }
    }

    /// Get the value of the named property from the FeelValue, 
    /// which may be invalid for the given FeelType or missing from the context.
    fn get_property<C: ContextReader>(&self, property_name: &FeelValue, contexts: &C) -> FeelValue {
        let prop_name = match property_name {
            FeelValue::String(s) => s.clone(),
            FeelValue::Name(n) => n.to_string(),
            _ => {
                ExecutionLog::log(&format!("Property name may not be a '{:?}'.",  self.get_type()));
                return FeelValue::Null; 
            }
        };
        match self.get_type() {
            // Since contexts may contain properties with any name,
            // attempt a key lookup.
            FeelType::Context => {
                let key: QName = prop_name.into();
                match self.try_get(&key) {
                    Some(value) => value,
                    None => no_such_property(&property_name.to_string(), self)
                }
            },
            FeelType::Date | FeelType::Time | FeelType::DateAndTime 
            | FeelType::YearMonthDuration | FeelType::DayTimeDuration
            | FeelType::Range => {
                match prop_name.as_str() {
                    "year" => self.year(),
                    "month" => self.month(),
                    "day" => self.day(),
                    "weekday" => self.weekday(),
                    "hour" => self.hour(),
                    "minute" => self.minute(),
                    "second" => self.second(),
                    "time offset" => self.time_offset(),
                    "timezone" => self.timezone(),
                    "years" => self.years(),
                    "months" => self.months(),
                    "days" => self.days(),
                    "hours" => self.hours(),
                    "minutes" => self.minutes(),
                    "seconds" => self.seconds(),
                    "start" => self.start(contexts),
                    "end" => self.end(contexts),
                    "start included" => self.start_included(contexts),
                    "end included" => self.end_included(contexts),
                    _ => no_such_property(&property_name.to_string(), self)
                }
            },
            _ => no_such_property(&property_name.to_string(), self)
        }
    }
}

#[cfg(test)]
mod tests {
    use chrono::{NaiveDate};
    use super::ValueProperties;
    use crate::parsing::feel_value::{FeelValue};
    use crate::parsing::context::{Context};
    use crate::parsing::range::Range;


    #[test]
    fn test_value_properties_for_dates() {
        let ctx = Context::new();
        let d = FeelValue::Date(NaiveDate::from_ymd(2021, 6, 15));
        assert_eq!(FeelValue::Number(2021.0), d.year());
        assert_eq!(FeelValue::Number(6.0), d.month());
        assert_eq!(FeelValue::Number(15.0), d.day());
        assert_eq!(FeelValue::Number(2.0), d.weekday()); // Tuesday = 2

        let year: FeelValue = "year".into();
        assert_eq!(FeelValue::Number(2021.0), d.get_property(&year, &ctx));
    }

    #[test]
    fn test_value_properties_for_datetimes() {
        let ctx = Context::new();
        let d = FeelValue::DateAndTime(NaiveDate::from_ymd(2021, 6, 15).and_hms(6, 15, 22));
        assert_eq!(FeelValue::Number(2021.0), d.year());
        assert_eq!(FeelValue::Number(6.0), d.month());
        assert_eq!(FeelValue::Number(15.0), d.day());
        assert_eq!(FeelValue::Number(2.0), d.weekday()); // Tuesday = 2
        assert_eq!(FeelValue::Number(6.0), d.hour());
        assert_eq!(FeelValue::Number(15.0), d.minute());
        assert_eq!(FeelValue::Number(22.0), d.second());

        let minute: FeelValue = "minute".into();
        assert_eq!(FeelValue::Number(15.0), d.get_property(&minute, &ctx));
    }

    #[test]
    fn test_value_properties_for_yearmonth_durations() {
        // The normalized values for the following should be 2 years, 3 months (the months overflow 12).
        let d = FeelValue::new_duration("P1Y15M").unwrap();  
        assert_eq!(FeelValue::Number(2.0), d.years());
        assert_eq!(FeelValue::Number(3.0), d.months());
        assert_eq!(FeelValue::Null, d.year()); // "year" is for Dates, not Durations!
    }

    #[test]
    fn test_value_properties_for_daytime_durations() {
        // The normalized values for the following should be 2 days, 5 hours, 10 minutes, 30 seconds.
        let d = FeelValue::new_duration("P1DT28H69M90S").unwrap();  
        assert_eq!(FeelValue::Number(2.0), d.days());
        assert_eq!(FeelValue::Number(5.0), d.hours());
        assert_eq!(FeelValue::Number(10.0), d.minutes());
        assert_eq!(FeelValue::Number(30.0), d.seconds());
    }

    #[test]
    fn test_value_properties_for_ranges() {
        let ctx = Context::new();
        let r1:Range = (5.0..10.0).into();
        let r = FeelValue::Range(r1);
        assert_eq!(FeelValue::Number(5.0), r.start(&ctx));
        assert_eq!(FeelValue::Number(10.0), r.end(&ctx));
        assert!(r.start_included(&ctx).is_true());
        assert!(r.end_included(&ctx).is_false());
    }
}
