use std::fmt;
use std::rc::Rc;
use std::cell::{RefCell,Ref};
use std::str::FromStr;
use std::convert::{From,TryFrom};
use std::string::ToString;
use chrono::{NaiveDate, NaiveDateTime, NaiveTime};
use chrono::format::{ParseResult};
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use super::qname::{QName, Stringlike};
use super::context::{Context, ContextReader};
use super::duration::{Duration,DurationVariety,DAYS_PER_MONTH};
use super::range::Range;
use super::feel_function::FeelFunction;
use super::execution_log::ExecutionLog;
use super::lattice_type::LatticeType;
use super::range_access::RangeAccess;

use lazy_static::lazy_static;
use regex::Regex;

// TIME_PATTERN: Regular expression that matches times with optional timezone suffix. 
lazy_static! {
    static ref TIME_PATTERN: Regex = Regex::new(
      r#"(?x)
      ^
      (?P<h>[01]\d|2[0-3])     # Hours
      :(?P<m>[0-5]\d)          # Minutes
      :(?P<s>[0-5]\d)          # Seconds
      (?:[zZ]|[@ ](?P<tz>.+))? # Optional Timezone
      $"#
    ).unwrap();
}

// DATETIME_PATTERN: Regular expression that matches date and time values with optional timezone suffix. 
lazy_static! {
  static ref DATETIME_PATTERN: Regex = Regex::new(
    r#"(?x)
    ^
    (?P<year>-?\d\d\d\d)          # Year
    -(?P<month>0[1-9]|1[012])     # Month
    -(?P<day>0[1-9]|[12]\d|3[01]) # Day
    T(?P<h>[01]\d|2[0-3])         # Hours
    :(?P<m>[0-5]\d)               # Minutes
    :(?P<s>[0-5]\d)               # Seconds
    (?:[zZ]|[@ ](?P<tz>.+))?      # Optional Timezone
    $"#
  ).unwrap();
}

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

impl FeelType {
  /// Get the standard name for the Feel type as used in the Specification. 
  pub fn feel_type(&self) -> &str {
    match self {
      FeelType::Number => "number",
      FeelType::String => "string",
      FeelType::Name => "name", // TODO: Not sure what string should be returned for this. 
      FeelType::Boolean => "boolean",
      FeelType::Date => "date",
      FeelType::Time => "time",
      FeelType::DateAndTime => "date and time",
      FeelType::YearMonthDuration => "years and months duration",
      FeelType::DayTimeDuration => "days and time duration",
      FeelType::Range => "range",
      FeelType::List => "list",
      FeelType::Context => "context",
      FeelType::Function => "function",
      FeelType::Null => "Null", // The Spec capitalizes Null for some reason.
      FeelType::Any => "Any"
    }
  }
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

  /// Every value in Feel has a type string that can be used as the righthand argument of the "instance of" operator.
  /// Some values have parameterized types: list, range, function and context.
  /// For parameterized types, the type is inferred by the contents. 
  ///   - For lists, if the list is empty or has at least two elements that differ in type, the ladder type is "list<Any>". 
  ///     Otherwise, the ladder type will be list<T> where T is the type of its elements, such as "list<number>". 
  ///   - For ranges, the low or high limit (whichever is not missing) determines the parameter, such as "range<date>". 
  ///   - For functions, the types of the inputs and output determine the form, like "(number, number) -> number". 
  ///   - For contexts, the names of the keys and the types of their values determine the form. 
  ///     {"name": "Peter", age: 30} has the form context<”age”: number, “name”:string>". 
  ///     It appears that the keys are sorted to make a canonical type name. 
  /// 
  /// The contexts parameter is only needed by Ranges that refer to a limit indirectly by name instead of value. 
  /// If climb is true, get a more general type. For example, if the ladder type is list<number>, 
  /// if you climb the ladder, the next type is list<Any>. 
  /// 
  /// This method is the basis of the instance of operator.
  pub fn get_lattice_type<C: ContextReader>(&self, contexts: &C) -> LatticeType {
    LatticeType::from_value(self, contexts)
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

  /// Parse a string into a FeelValue::Time, returning None on failure. 
  /// Recognizes but discards the timezone. 
  pub fn new_time(time_string: &str) -> Option<Self> {
    // TODO: Incorporate Time zones into Times and Dates. 
    match TIME_PATTERN.captures(time_string) {
      Some(caps) => {
        match (caps.name("h"), caps.name("m"), caps.name("s"), caps.name("tz")) {
          (Some(hours), Some(minutes), Some(seconds), _) => {
            let h: u32 = hours.as_str().parse().unwrap();
            let m: u32 = minutes.as_str().parse().unwrap();
            let s: u32 = seconds.as_str().parse().unwrap();
            // The Regex pattern was crafted so that only valid ranges for the h,m,s values would be recognized.
            Some(FeelValue::Time(NaiveTime::from_hms(h,m,s)))
          },
          _ => None,
        }
      },
      None => None
    }
  }

  pub fn new_date(date_string: &str) -> Option<Self> {
    match NaiveDate::parse_from_str(date_string, "%Y-%m-%d") {
      ParseResult::Ok(date) => Some(FeelValue::Date(date)),
      ParseResult::Err(_) =>  None
    }
  }

  /// Parse a string into a FeelValue::DateAndTime, returning None on failure. 
  /// Recognizes but discards the timezone. 
  pub fn new_date_and_time(time_string: &str) -> Option<Self> {
    // TODO: Incorporate Time zones into Times and Dates. 
    match DATETIME_PATTERN.captures(time_string) {
      Some(caps) => {
        match (caps.name("year"), caps.name("month"), caps.name("day"), caps.name("h"), caps.name("m"), caps.name("s"), caps.name("tz")) {
          (Some(yr), Some(mo), Some(dy), Some(hours), Some(minutes), Some(seconds), _) => {
            let year: i32 = yr.as_str().parse().unwrap();
            let month: u32 = mo.as_str().parse().unwrap();
            let day: u32 = dy.as_str().parse().unwrap();
            let h: u32 = hours.as_str().parse().unwrap();
            let m: u32 = minutes.as_str().parse().unwrap();
            let s: u32 = seconds.as_str().parse().unwrap();
            // The Regex pattern was crafted so that only valid ranges for the h,m,s values would be recognized.
            // The month is guaranteed, too, but the day could be invalid for that month. 
            match NaiveDate::from_ymd_opt(year, month, day) {
              Some(date) => Some(FeelValue::DateAndTime(date.and_hms(h,m,s))),
              None => None
            }
          },
          _ => None,
        }
      },
      None => None
    }
  }

  pub fn new_duration(duration_string: &str) -> Option<Self> {
    match Duration::from_str(duration_string) {
      Ok(duration) => {
        match duration.get_variety() {
          DurationVariety::DayTime => Some(FeelValue::DayTimeDuration(duration)),
          DurationVariety::YearMonth => Some(FeelValue::YearMonthDuration(duration)),
          DurationVariety::Full if duration.are_year_month_zero() => Some(FeelValue::DayTimeDuration(duration.as_day_time(DAYS_PER_MONTH as f32))),
          _ => Some(FeelValue::YearMonthDuration(duration.as_year_month(DAYS_PER_MONTH as f32)))
        }
      },
      _ => None
    }
  }

  pub fn new_context() -> Self {
    FeelValue::Context(Rc::new(Context::new()))
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

  /// Try to treat the value as a Context and retrieve the value of the given key. 
  /// If not a FeelValue::Context or the key is not in it, return None.
  pub fn try_get(&self, key: &QName) -> Option<FeelValue> {
    match self {
      FeelValue::Context(ctx) => ctx.clone().get(key.clone()),
      _ => None
    }
  }

  /// If the FeelValue is a List, get a Ref to the element at the given index. 
  /// If another variant or the index is out of range, return None. 
  pub fn index<'a>(&'a self, index: usize) -> Option<Ref<'a, Self>> {
    match self {
      FeelValue::List(rr_list) => {
        let list = rr_list.borrow();
        if list.len() <= index {
          None
        }
        else {
          Some(Ref::map(list, |x| &x[index]))
        }
      },
      _ => None
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

  pub fn item_or_contents(&self) -> Vec<FeelValue> 
  {
    match self {
      FeelValue::List(rr_list) => rr_list.borrow().iter().cloned().collect(),
      _ => vec![self.clone()]
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

/// Implement Display for FeelValue so as to conform to the string(value) builtin function in the DMN Feel Spec. 
impl fmt::Display for FeelValue {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      FeelValue::Number(n) => write!(f, "{}", n),
      FeelValue::String(s) => write!(f, "{}", s),
      FeelValue::Name(q) => write!(f, "{}", q),
      FeelValue::Boolean(b) => write!(f, "{}", b),
      FeelValue::Date(d) => write!(f, "{}", d.format("%Y-%m-%d")), // yyyy-mm-dd format
      FeelValue::Time(t) => write!(f, "{}", t.format("%H:%M:%S")), // hh:mm:ss format, in 24-hour time
      FeelValue::DateAndTime(dt) => write!(f, "{}", dt.format("%Y-%m-%dT%H:%M:%Sz")),
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
      // TODO: Format the Function as its source code form. 
      FeelValue::Function(func) => write!(f, "{}", func),
      // NOTE: Builtins::string(FeelValue::Null) will reutrn a null, not this string. 
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
      
impl From<Context> for FeelValue {
  fn from(c: Context) -> Self {
    FeelValue::Context(Rc::new(c))
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

impl RangeAccess for FeelValue {

  fn range_length<C: ContextReader>(&self, contexts: &C) -> usize {
      match self {
        FeelValue::Range(r) => {
            match r.get_integer_loop_bounds(contexts) {
              Some((_, _, count, _)) => count,
              None => 0_usize
            } 
        },
        _ => 0_usize
      }
  }

  fn loop_bounds<C: ContextReader>(&self, contexts: &C) -> (FeelValue,FeelValue,FeelValue,FeelValue) {
      let z = FeelValue::Number(0.0);
      match self {
          FeelValue::Range(r) => {
              match r.get_integer_loop_bounds(contexts) {
                Some((start, stop, count, step)) => (
                  (start as f64).into(),
                  (stop as f64).into(),
                  (count as f64).into(),
                  (step as f64).into()
                ),
                None => (z.clone(), z.clone(), z.clone(), 1.0.into())
              } 
          },
          FeelValue::List(rr_list) => {
            let length = rr_list.borrow().len() as f64;
            let stop = if length > 0.0 { length - 1.0 } else { 0.0 };
            (
              z.clone(),
              stop.into(),
              length.into(),
              1.0.into()
            )
          },
          _ => (z.clone(), z.clone(), z.clone(), 1.0.into())
      }
  }

  fn range_index<C: ContextReader>(&self, position: usize, contexts: &C) -> FeelValue {
      match self {
          FeelValue::Range(r) => {
              match r.get_integer_loop_bounds(contexts) {
                Some((start, _stop, count, step)) => {
                  if position >= count {
                    // TODO: Log error
                    FeelValue::Null
                  }
                  else {
                    FeelValue::Number(start as f64 + (position as f64) * (step as f64))
                  }
                },
                None => 0_.0.into()
              }
          },
          FeelValue::List(rr_list) => {
            let length = rr_list.borrow().len();
            if position >= length {
              // TODO: Log error
              FeelValue::Null
            }
            else {
              rr_list.borrow()[position].clone()
            }
          },
          _ => FeelValue::Null
      }
  }

}

/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
  use chrono::naive::{NaiveDate,NaiveTime};
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

  #[test]
  fn test_new_time() {
    assert_eq!(
      FeelValue::Time(NaiveTime::from_hms(12, 34, 56)), 
      FeelValue::new_time("12:34:56z").unwrap()
    );
    assert_eq!(
      FeelValue::Time(NaiveTime::from_hms(2, 1, 0)), 
      FeelValue::new_time("02:01:00@Etc/EDT").unwrap()
    );
  }

  #[test]
  fn test_new_date_and_time() {
    let expected_dt = FeelValue::DateAndTime(NaiveDate::from_ymd(2021, 5, 4).and_hms(12, 34, 56));
    assert_eq!(
      FeelValue::new_date_and_time("2021-05-04T12:34:56z").unwrap(), 
      expected_dt
    );
  }

}
