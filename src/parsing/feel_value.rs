use super::qname::QName;

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
enum FeelValue {
  /// A Number
  Number(f64),
  /// A String
  String(String),
  /// A Qualified Name, which is a list of strings
  Name(QName), 
  /// A Boolean
  Boolean(bool),
  /// A DateTime value with just the Date part filled in
  Date(DateTime),
  /// A DateTime value with just the Time part filled in
  Time(DateTime),
  /// A DateTime value with both Date and Time parts filled in
  DateAndTime,
  /// Duration with only years and months filled in
  YearMonthDuration, 
  /// Duration with only days, hours, minutes and seconds filled in
  DayTimeDuration,
  /// A sequential collection of other values
  List(Vec<FeelValue>),
  /// A collection of name-value pairs
  Context,
  /// A function taking a single argument (a FeelValue) and returning a single result (a FeelValue)
  Function,
  /// A missing value
  Null,
  /// An execution error
  Error(String)
}
