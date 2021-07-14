use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::Debug;
use std::convert::From;
use std::ops::Index;
use std::ops::RangeBounds;
use super::range::Range;
use super::feel_value::{FeelValue, FeelType};
use super::execution_log::ExecutionLog;

/// Convenient wrapper for processing argument lists for builtin functions.
#[derive(Clone)]
pub struct Arguments {
  pub args: Vec<FeelValue>
}

impl Arguments {
  pub fn new(args: Vec<FeelValue>) -> Arguments {
    Arguments { args: args }
  }

  pub fn new1(arg1: FeelValue) -> Arguments {
    Arguments { args: vec![arg1] }
  }

  pub fn new2(arg1: FeelValue, arg2: FeelValue) -> Arguments {
    Arguments { args: vec![arg1, arg2] }
  }

  pub fn new3(arg1: FeelValue, arg2: FeelValue, arg3: FeelValue) -> Arguments {
    Arguments { args: vec![arg1, arg2, arg3] }
  }

  pub fn new4(arg1: FeelValue, arg2: FeelValue, arg3: FeelValue, arg4: FeelValue) -> Arguments {
    Arguments { args: vec![arg1, arg2, arg3, arg4] }
  }

  /// Counts the number of arguments.
  pub fn count(&self) -> usize {
    self.args.len()
  }

  /// Checks if any arguments are null.
  pub fn any_nulls(&self) -> bool {
    self.args.iter().any(|i| i.is_null())
  }

  /// If the args has one entry and it is a List, 
  /// then return the contents of that List. This is for when a Builtin may 
  /// be called with either many arguments or a singe List.
  pub fn flat_args(self) -> Vec<FeelValue> {
    if self.args.len() == 1 {
      match &self.args[0] {
        FeelValue::List(rr_list) => rr_list.borrow().iter().cloned().collect(),
        _ => self.args
      }
    }
    else {
      self.args
    }
  }
}

impl Index<usize> for Arguments {
  type Output = FeelValue;

  fn index(&self, index: usize) -> &Self::Output {
    if index >= self.count() {
      &FeelValue::Null
    }
    else {
      &self.args[index]
    }
  }
}

#[derive(Clone)]
pub enum Validity {
  Valid { name: Rc<String>, args: Rc<Arguments> },
  Invalid { name: Rc<String>, args: Rc<Arguments> }
}

/// Fluent, fail-fast validation of Arguments objects.
/// You can chain together many validation method calls and the first to fail will be logged,
/// but the rest will be passed by without being checked.
/// The final call in the chain should be is_valid, which 
/// yields the final answer of whether the arguments are valid.
impl Validity {
  pub fn new(name: &Rc<String>, args: &Rc<Arguments>) -> Self {
    Validity::Valid { name: name.clone(), args: args.clone() }
  }

  /// Does this represent a Valid or an Invalid state?
  pub fn is_valid(&self) -> bool {
    match self {
      Validity::Valid {..} => true,
      Validity::Invalid {..} => false
    }
  }

  pub fn validated(self) -> Result<Arguments,String> {
    match self {
      Validity::Valid {name: _, args} => Ok((*args).clone()),
      Validity::Invalid {..} => Err("Arguments invalid".into())
    }
  }

  /// Get an additional reference to the contained name.
  pub fn name(&self) -> Rc<String> {
    match self {
      Validity::Valid { name, .. } => name.clone(),
      Validity::Invalid { name, .. } => name.clone()
    }
  }

  /// Get an additional reference to the contained Arguments.
  pub fn arguments(&self) -> Rc<Arguments> {
    match self {
      Validity::Valid { name: _, args } => args.clone(),
      Validity::Invalid { name: _, args } => args.clone()
    }
  }

  pub fn has_list_as_sole_argument(&self) -> bool {
    let args = self.arguments();
    if args.as_ref().args.len() != 1 {
      false
    }
    else {
      args.as_ref().args[0].get_type() == FeelType::List
    }
  }

  pub fn and(self, passes: bool) -> Self {
    match (passes, &self.is_valid()) {
      (true, _) => self,
      (false, false) => self,
      (false, true) => Validity::Invalid { name: self.name(), args: self.arguments() }
    }
  }

  /// Check that the expected number of arguments were supplied.
  pub fn arity<R: RangeBounds<usize> + Debug>(self, r: R) -> Self {
    if !self.is_valid() {
      return self;
    }
    let args_arity = (*self.arguments()).count();
    if !r.contains(&args_arity) {
      ExecutionLog::log(&format!("Called {:?} with {:?} arguments, expected {:?}", self.name(), args_arity, r));
      self.and(false)
    }
    else {
      self
    }
  }

  /// Check that the argument at the given zero-based position 
  /// has the expected type unless it is null and we permit a null 
  /// (for optional parameters).
  /// If expected_type is FeelType::Any, it passes the test.
  pub fn expect_type(self, zero_based_argument: usize, expected_type: FeelType, allow_null: bool) -> Self {
    if !self.is_valid() {
      return self;
    }
    if zero_based_argument == 0 && self.has_list_as_sole_argument() && expected_type != FeelType::List {
      // If a list with a single list argument, assume the type constraint is 
      // to be applied to the first element of the inner list. 
      let list = &self.arguments()[0];
      match list.list_length() {
        Some(len) if len >= 1 => {
          match list {
            FeelValue::List(rr_list) => {
              let actual_type = rr_list.borrow()[0].get_type();
              let is_null = actual_type == FeelType::Null;
              if actual_type != expected_type && expected_type != FeelType::Any && !(is_null && allow_null)  {
                ExecutionLog::log(&format!(
                  "Called {:?} with {:?} for first argument, expected {:?}", 
                  self.name(), actual_type, expected_type
                ));
                self.and(false)
              }
              else {
                self
              }
            },
            _ => unreachable!()
          }
        },
        _ => {
          ExecutionLog::log(&format!("Called {:?} with empty list, expected {:?}", self.name(), expected_type));
          self.and(false)
        }
      }
    }
    else {
      let actual_type = self.arguments()[zero_based_argument].get_type();
      let is_null = self.arguments()[zero_based_argument].is_null();
      if actual_type != expected_type && expected_type != FeelType::Any && !(is_null && allow_null)  {
        ExecutionLog::log(&format!(
          "Called {:?} with {:?} for argument {:?}, expected {:?}", 
          self.name(), actual_type, zero_based_argument + 1, expected_type
        ));
        self.and(false)
      }
      else {
        self
      }
    }
  }

  pub fn expect_string_or_name(self, zero_based_argument: usize, allow_null: bool) -> Self {
    if !self.is_valid() {
      return self;
    }
    if zero_based_argument == 0 && self.has_list_as_sole_argument() {
      // If a list with a single list argument, assume the type constraint is 
      // to be applied to the first element of the inner list. 
      let list = &self.arguments()[0];
      match list.list_length() {
        Some(len) if len >= 1 => {
          match list {
            FeelValue::List(rr_list) => {
              let actual_type = rr_list.borrow()[0].get_type();
              let is_null = actual_type == FeelType::Null;
              if actual_type != FeelType::String && actual_type != FeelType::Name && !(is_null && allow_null)  {
                ExecutionLog::log(&format!(
                  "Called {:?} with {:?} for first argument, expected String or Qualified Name", 
                  self.name(), actual_type
                ));
                self.and(false)
              }
              else {
                self
              }
            },
            _ => unreachable!()
          }
        },
        _ => {
          ExecutionLog::log(&format!("Called {:?} with empty list, expected String or Qualified Name", self.name()));
          self.and(false)
        }
      }
    }
    else {
      let actual_type = self.arguments()[zero_based_argument].get_type();
      let is_null = self.arguments()[zero_based_argument].is_null();
      if actual_type != FeelType::String && actual_type != FeelType::Name && !(is_null && allow_null)  {
        ExecutionLog::log(&format!(
          "Called {:?} with {:?} for argument {:?}, expected String or Qualified Name", 
          self.name(), actual_type, zero_based_argument + 1
        ));
        self.and(false)
      }
      else {
        self
      }
    }
  }

  /// Range functions support one, two, or four different use cases. 
  /// The first argument may be a point (like a Number or Date) and/or Range.
  /// The second argument may be a point (like a Number or Date) and/or Range.
  pub fn point_or_range(self, 
    first_allow_point: bool,
    first_allow_range: bool,
    second_allow_point: bool,
    second_allow_range: bool
  ) -> Self {
    if !self.is_valid() {
      return self;
    }
    let first_type = self.arguments()[0].get_type();
    let second_type = self.arguments()[1].get_type();
    let first_is_point = Range::is_suitable_as_point(first_type);
    let second_is_point = Range::is_suitable_as_point(second_type);

    if !first_is_point && first_type != FeelType::Range {
      ExecutionLog::log(&format!(
        "Called {:?} with a {:?} for the first argument, which cannot be used with Ranges", 
        self.name(),
        first_type
      ));
      return self.and(false);
    }

    if !second_is_point && second_type != FeelType::Range {
      ExecutionLog::log(&format!(
        "Called {:?} with a {:?} for the second argument, which cannot be used with Ranges", 
        self.name(),
        second_type
      ));
      return self.and(false);
    }

    match (first_is_point, second_is_point) {
      (true, true) => {
        if !first_allow_point {
          ExecutionLog::log(&format!(
            "Called {:?} with a point for first argument, expected a Range", 
            self.name()
          ));
          self.and(false)
        }
        else if !second_allow_point {
          ExecutionLog::log(&format!(
            "Called {:?} with a point for second argument, expected a Range", 
            self.name()
          ));
          self.and(false)
        }
        else {
          self
        }
      },
      (true, false) => {
        if !first_allow_point {
          ExecutionLog::log(&format!(
            "Called {:?} with a point for first argument, expected a Range", 
            self.name()
          ));
          self.and(false)
        }
        else if !second_allow_range {
          ExecutionLog::log(&format!(
            "Called {:?} with a Range for second argument, expected a point", 
            self.name()
          ));
          self.and(false)
        }
        else {
          self
        }
      },
      (false, true) => {
        if !first_allow_range {
          ExecutionLog::log(&format!(
            "Called {:?} with a Range for first argument, expected a point", 
            self.name()
          ));
          self.and(false)
        }
        else if !second_allow_point {
          ExecutionLog::log(&format!(
            "Called {:?} with a point for second argument, expected a Range", 
            self.name()
          ));
          self.and(false)
        }
        else {
          self
        }
      },
      (false, false) => {
        if !first_allow_range {
          ExecutionLog::log(&format!(
            "Called {:?} with a Range for first argument, expected a point", 
            self.name()
          ));
          self.and(false)
        }
        else if !second_allow_range {
          ExecutionLog::log(&format!(
            "Called {:?} with a Range for second argument, expected a point", 
            self.name()
          ));
          self.and(false)
        }
        else {
          self
        }
      }
    }
  }

  /// Check that the argument at the given zero-based position is an integer
  /// unless it is null and we permit a null (for optional parameters).
  pub fn expect_integer(self, zero_based_argument: usize, allow_null: bool) -> Self {
    if !self.is_valid() {
      return self;
    }
    let is_integer = self.arguments()[zero_based_argument].is_integer();
    let is_null = self.arguments()[zero_based_argument].is_null();
    if !is_integer && !(is_null && allow_null) {
      ExecutionLog::log(&format!(
        "Called {:?} with a noninteger for argument {:?}", 
        self.name(), zero_based_argument + 1
      ));
      self.and(false)
    }
    else {
      self
    }
  }

  /// Check that the argument at the given zero-based position is a String or Name,
  /// the only two variants permitted to represent the key of a Context.
  pub fn expect_key(self, zero_based_argument: usize) -> Self {
    if !self.is_valid() {
      return self;
    }
    match self.arguments()[zero_based_argument].get_type() {
      FeelType::String => self,
      FeelType::Name => self,
      _ => {
        ExecutionLog::log(&format!(
          "Called {:?} with a key that is neither a String nor a QName for argument {:?}", 
          self.name(), zero_based_argument + 1
        ));
        self.and(false)
      }
    }
  }

  /// Assume that the argument at the first zero-based position is a List
  /// (from an expect_type constraint)
  /// and verify that the argument at the second zero-based position is a
  /// non-zero integer between 1 and Length (inclusive) or betweeen
  /// -1 and -Length where Length is the number of elements in the List.
  pub fn position_in_range(
    self, 
    zero_based_list_argument: usize, 
    zero_based_position_argument: usize) -> Self {
    if !self.is_valid() {
      return self;
    }
    let list_length = self.arguments()[zero_based_list_argument].list_length();
    let position = &self.arguments()[zero_based_position_argument];

    if !position.is_integer() {
      ExecutionLog::log(&format!(
        "Called {:?} with a noninteger for argument {:?}", 
        self.name(), zero_based_position_argument + 1
      ));
      self.and(false)
    }
    else {
      match (list_length, position) {
        (Some(len), FeelValue::Number(n)) => {
          let length = len as isize;
          let pos = *n as isize;
          if length < pos || -length > pos || pos == 0_isize {
            ExecutionLog::log(&format!(
              "Called {:?} where argument {:?} is not a valid list position in the range [{}..{}] or [{}..{}]", 
              self.name(), zero_based_position_argument + 1,
              -length, -1, 1, length
            ));
            self.and(false)
          }
          else {
            self
          }
        },
        _ => unreachable!() // Should have already verified the argument types
      }
    }
  }

  /// Check that the argument at the given zero-based position 
  /// has date information (i.e. is Date or DateAndTime).
  pub fn expect_date(self, zero_based_argument: usize) -> Self {
    if !self.is_valid() {
      return self;
    }
    let has_date = self.arguments()[zero_based_argument].has_date();
    if !has_date {
      ExecutionLog::log(&format!(
        "Called {:?} where argument {:?} is not a date", 
        self.name(), zero_based_argument + 1
      ));
      self.and(false)
    }
    else {
      self
    }
  }

  /// Check that there is either one argument, a list of only Numbers,
  /// or one or more arguments, all Numbers.
  pub fn expect_number_list(self) -> Self {
    if !self.is_valid() {
      return self;
    }
    match &self.arguments()[0] {
      FeelValue::Number(_) => self.same_types(),
      FeelValue::List(list) => {
        let mismatch =  list.borrow().iter().any(|item| item.get_type() != FeelType::Number);
        if mismatch {
          ExecutionLog::log(&format!(
            "Called {:?} with a list of items which are not all numeric", 
            self.name()
          ));
          self.and(false)
        }
        else {
          self
        }
      },
      _ => {
        ExecutionLog::log(&format!(
          "Called {:?} with non-numeric first argument", 
          self.name()
        ));
        self.and(false)
      }
    }
  }

  /// Check that there is either one argument, a list of all the same type,
  /// or one or more arguments, all the same type.
  pub fn expect_uniform_list(self) -> Self {
    if !self.is_valid() {
      return self;
    }
    if self.arguments().count() == 0 {
      return self;
    }
    match &self.arguments()[0] {
      FeelValue::List(list) => {
        if list.borrow().len() == 0 {
          return self;
        }
        let first_type = list.borrow()[0_usize].get_type();
        let mismatch =  list.borrow().iter().any(|item| item.get_type() != first_type);
        if mismatch {
          ExecutionLog::log(&format!(
            "Called {:?} with a list of unlike items", 
            self.name()
          ));
          self.and(false)
        }
        else {
          self
        }
      },
      _ => self.same_types()
    }
  }

  /// Check that all arguments have the same type as the first argument.
  pub fn same_types(self) -> Self {
    if !self.is_valid() {
      return self;
    }
    let args_vec = &self.arguments().args;
    let first_type = args_vec[0_usize].get_type();
    let mismatch = args_vec.iter().any(|item| item.get_type() != first_type);
    if mismatch {
      ExecutionLog::log(&format!(
        "Called {:?} with arguments which are not all {:?}", 
        self.name(), first_type
      ));
      self.and(false)
    }
    else {
      self
    }
  }

  /// Check that no arguments are null.
  pub fn no_nulls(self) -> Self {
    if !self.is_valid() {
      return self;
    }
    if self.arguments().any_nulls() {
      ExecutionLog::log(&format!("Called {:?} with a null argument", self.name() ));
      self.and(false)
    }
    else {
      self
    }
  }
}

impl From<Arguments> for FeelValue {
  fn from(a: Arguments) -> Self {
    FeelValue::List(Rc::new(RefCell::new(a.args)))
  }
}


impl From<FeelValue> for Arguments {
  /// Convert a FeelValue into an Arguments object,
  /// flattening the value into its elements if it is a list.
  fn from(value: FeelValue) -> Self {
    match value {
      FeelValue::List(list) => {
        Arguments::new(list.borrow().clone())
      },
      _ => Arguments::new1(value)
    }
  }
}

/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
  use std::rc::Rc;
  use std::cell::RefCell;
  use chrono::{NaiveDate};
  use super::super::feel_value::{FeelValue,FeelType};
  use super::{Arguments, Validity};
  use super::super::range::Range;

  #[test]
  fn test_arity() {
    let a = Rc::new(Arguments::new1(1.into()));
    let name: Rc<String> = Rc::new("test".into());
    let v1 = Validity::new(&name, &a);
    assert!(v1.arity(1..2).is_valid());

    let v2 = Validity::new(&name, &a);
    assert!(!v2.arity(2..3).is_valid());
  }

  #[test]
  fn test_expect_type() {
    let a = Rc::new(Arguments::new2("hello".into(), 1.into()));
    let name: Rc<String> = Rc::new("test".into());
    let v1 = Validity::new(&name, &a);
    assert!(v1
      .arity(2..3)
      .expect_type(1, FeelType::Number, false)
      .is_valid()
    );

    let v2 = Validity::new(&name, &a);
    assert!(!v2
      .arity(2..3)
      .expect_type(0, FeelType::Number, false)
      .is_valid()
    );
  }

  #[test]
  fn test_point_or_range() {
    let r = FeelValue::Range(Range::new(&2.into(), &10.into(), true, false));
    let a = Rc::new(Arguments::new2(r, 1.into()));
    let name: Rc<String> = Rc::new("test".into());
    let v1 = Validity::new(&name, &a);
    assert!(v1
      .arity(2..3)
      .point_or_range(false, true, true, false)
      .is_valid()
    );

    let v2 = Validity::new(&name, &a);
    assert!(!v2
      .arity(2..3)
      .point_or_range(true, false, false, true)
      .is_valid()
    );
  }

  #[test]
  fn test_expect_integer() {
    let a = Rc::new(Arguments::new4("hello".into(), 1.into(), 3.14159.into(), FeelValue::Null));
    let name: Rc<String> = Rc::new("test".into());
    let v1 = Validity::new(&name, &a);
    assert!(v1
      .arity(4..5)
      .expect_integer(1, false)
      .expect_integer(3, true) // null when we permit it
      .is_valid()
    );

    let v2 = Validity::new(&name, &a);
    assert!(!v2
      .arity(4..5)
      .expect_integer(2, false)
      .is_valid()
    );

    let v2 = Validity::new(&name, &a);
    assert!(!v2
      .arity(4..5)
      .expect_integer(3, false) // null when we don't want one
      .is_valid()
    );
  }

  #[test]
  fn test_expect_date() {
    let d = FeelValue::Date(NaiveDate::from_ymd(2021_i32, 4_u32, 28_u32));
    let a = Rc::new(Arguments::new2(d, 1.into()));
    let name: Rc<String> = Rc::new("test".into());
    let v1 = Validity::new(&name, &a);
    assert!(v1
      .arity(2..3)
      .expect_date(0)
      .is_valid()
    );

    let v2 = Validity::new(&name, &a);
    assert!(!v2
      .arity(2..3)
      .expect_date(1)
      .is_valid()
    );
  }

  #[test]
  fn test_expect_number_list() {
    let mut a = Rc::new(Arguments::new3(1.into(), 3.14159.into(), 4.into()));
    let name: Rc<String> = Rc::new("test".into());
    let v1 = Validity::new(&name, &a);
    assert!(v1
      .arity(3..4)
      .expect_number_list()
      .is_valid()
    );

    a = Rc::new(Arguments::new3(1.into(), 3.14159.into(), "4".into()));
    let v2 = Validity::new(&name, &a);
    assert!(!v2
      .arity(3..4)
      .expect_number_list()
      .is_valid()
    );

    let two: FeelValue = 2.into();
    let pi: FeelValue = 3.14159.into();
    let list = FeelValue::List(Rc::new(RefCell::new(vec![two, pi])));
    a = Rc::new(Arguments::new1(list));
    let v3 = Validity::new(&name, &a);
    assert!(v3
      .arity(1..2)
      .expect_number_list()
      .is_valid()
    );
  }

  #[test]
  fn test_expect_uniform_list() {
    let mut a = Rc::new(Arguments::new3("1".into(), "Bob".into(), "4".into()));
    let name: Rc<String> = Rc::new("test".into());
    let v1 = Validity::new(&name, &a);
    assert!(v1
      .arity(3..4)
      .expect_uniform_list()
      .is_valid()
    );

    a = Rc::new(Arguments::new3(1.into(), 3.14159.into(), "4".into()));
    let v2 = Validity::new(&name, &a);
    assert!(!v2
      .arity(3..4)
      .expect_uniform_list()
      .is_valid()
    );

    let t: FeelValue = true.into();
    let f: FeelValue = false.into();
    let list = FeelValue::List(Rc::new(RefCell::new(vec![t, f])));
    a = Rc::new(Arguments::new1(list));
    let v3 = Validity::new(&name, &a);
    assert!(v3
      .arity(1..2)
      .expect_uniform_list()
      .is_valid()
    );
  }

  #[test]
  fn test_same_types() {
    let mut a = Rc::new(Arguments::new3("1".into(), "Bob".into(), "4".into()));
    let name: Rc<String> = Rc::new("test".into());
    let v1 = Validity::new(&name, &a);
    assert!(v1
      .arity(3..4)
      .same_types()
      .is_valid()
    );

    a = Rc::new(Arguments::new3(1.into(), 3.14159.into(), "4".into()));
    let v2 = Validity::new(&name, &a);
    assert!(!v2
      .arity(3..4)
      .same_types()
      .is_valid()
    );
  }

  #[test]
  fn test_no_nulls() {
    let mut a = Rc::new(Arguments::new3("1".into(), "Bob".into(), "4".into()));
    let name: Rc<String> = Rc::new("test".into());
    let v1 = Validity::new(&name, &a);
    assert!(v1
      .arity(3..4)
      .no_nulls()
      .is_valid()
    );

    a = Rc::new(Arguments::new3(1.into(), 3.14159.into(), FeelValue::Null));
    let v2 = Validity::new(&name, &a);
    assert!(!v2
      .arity(3..4)
      .no_nulls()
      .is_valid()
    );
  }
   
}
