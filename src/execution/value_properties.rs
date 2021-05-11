use chrono::{Datelike, Timelike};
use crate::parsing::feel_value::{FeelValue};
use crate::parsing::context::{ContextReader};
use std::ops::{Bound};
use crate::parsing::execution_log::ExecutionLog;

pub fn no_such_property(prop_name: &str, value: &FeelValue) -> FeelValue {
    ExecutionLog::log(&format!("Property '{:?}' not defined for {:?} values.", prop_name,  value.get_type()));
    FeelValue::Null
}

/// The DMN Spec supports 29 properties, mostly of dates and times. 
pub trait ValueProperties {

    fn year(&self) -> FeelValue;
  
    fn month(&self) -> FeelValue;
  
    fn day(&self) -> FeelValue;
  
    fn weekday(&self) -> FeelValue;
  
    fn hour(&self) -> FeelValue;
  
    fn minute(&self) -> FeelValue ;
  
    fn second(&self) -> FeelValue;
  
    fn time_offset(&self) -> FeelValue;
  
    fn timezone(&self) -> FeelValue;
  
    fn years(&self) -> FeelValue;
  
    fn months(&self) -> FeelValue;
  
    fn days(&self) -> FeelValue;
  
    fn hours(&self) -> FeelValue;
  
    fn minutes(&self) -> FeelValue;
  
    fn seconds(&self) -> FeelValue ;
  
    fn start<C: ContextReader>(&self, contexts: &C) -> FeelValue;
  
    fn end<C: ContextReader>(&self, contexts: &C) -> FeelValue;
  
    fn start_included<C: ContextReader>(&self, contexts: &C) -> FeelValue;
  
    fn end_included<C: ContextReader>(&self, contexts: &C) -> FeelValue;

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
            FeelValue::Date(d) => d.weekday().to_string().into(),
            FeelValue::DateAndTime(d) => d.weekday().to_string().into(),
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

    fn get_property<C: ContextReader>(&self, property_name: &FeelValue, contexts: &C) -> FeelValue {
        let prop_name = match property_name {
            FeelValue::String(s) => s.clone(),
            FeelValue::Name(n) => n.to_string(),
            _ => {
                ExecutionLog::log(&format!("Property name may not be a '{:?}'.",  self.get_type()));
                return FeelValue::Null; 
            }
        };
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
    }
}

#[cfg(test)]
mod tests {
    use chrono::{NaiveDate};
    use super::ValueProperties;
    use crate::parsing::feel_value::{FeelValue};
    use crate::parsing::context::{Context};


    #[test]
    fn test_value_properties_for_dates() {
        let ctx = Context::new();
        let d = FeelValue::Date(NaiveDate::from_ymd(2021, 6, 15));
        assert_eq!(FeelValue::Number(2021.0), d.year());
        assert_eq!(FeelValue::Number(6.0), d.month());
        assert_eq!(FeelValue::Number(15.0), d.day());
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
        assert_eq!(FeelValue::Number(6.0), d.hour());
        assert_eq!(FeelValue::Number(15.0), d.minute());
        assert_eq!(FeelValue::Number(22.0), d.second());

        let minute: FeelValue = "minute".into();
        assert_eq!(FeelValue::Number(15.0), d.get_property(&minute, &ctx));
    }
}
