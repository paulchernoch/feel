use std::ptr;
use std::rc::Rc;
use std::cell::RefCell;
use std::convert::From;
use std::string::ToString;
use std::ops::Deref;
use std::cmp::{Ord, PartialOrd, Ordering};
use std::fmt::{Debug,Display,Formatter,Result};
use super::feel_value::FeelValue;
use super::nested_context::NestedContext;

/// Functions may be builtin or user defined.
#[derive(PartialEq, Eq, Debug, Clone, Copy, ToString, IntoStaticStr)]
pub enum FunctionType {
  Builtin,
  User
}


/// Name given initially to a user defined function, which are not all named.
static ANONYMOUS: &str = "anonymous";

/// Encapsulate an anonymous closure that can be called as a function
/// and optionally assigned a name. 
/// All such functions take two parameters: &FeelValue and &mut NestedContext.
/// They return a FeelValue.
#[derive(Clone)]
pub struct FeelFunction {
  pub function_type: FunctionType,
  name_container: RefCell<String>,
  function_container: Rc<dyn Fn(&FeelValue, &mut NestedContext) -> FeelValue>
}

impl FeelFunction {
  pub fn new_builtin(name: String, func: impl Fn(&FeelValue, &mut NestedContext) -> FeelValue + 'static) -> Self {
    FeelFunction {
      function_type: FunctionType::Builtin,
      name_container: RefCell::new(name),
      function_container: Rc::new(func)
    }
  }

  pub fn new_user(func: impl Fn(&FeelValue, &mut NestedContext) -> FeelValue + 'static) -> Self {
    FeelFunction {
      function_type: FunctionType::User,
      name_container: RefCell::new(ANONYMOUS.to_string()),
      function_container: Rc::new(func)
    }
  }

  /// Get a clone of the function name.
  pub fn get_name(&self) -> String {
    self.name_container.borrow().clone()
  }

  /// Set the function name. 
  /// When parsing the Feel expression, the most common idiom is to 
  /// define a user defined function and stuff it into a Context.
  /// The parser in such cases will need to set the function name
  /// after definition time as soon as it learns the Context key to 
  /// use as the function name.
  /// Note that setting the function's name does not require a mutable reference.
  pub fn set_name(&self, new_name: String) -> String {
    self.name_container.replace(new_name)
  }

}

impl PartialEq for FeelFunction {
  fn eq(&self, other: &Self) -> bool {
    if ptr::eq(self, other) {
      return true;
    }
    let name = self.get_name();
    if name != other.get_name() {
      return false;
    }
    if self.function_type == FunctionType::Builtin && other.function_type == FunctionType::Builtin {
      return true;
    }
    if name == ANONYMOUS {
      // The names match, but they are both anonymous, so we assume that the functions are different
      // (unless they have the same pointer value).
      return false;
    }
    return true;
  }
}

impl Eq for FeelFunction {}

impl PartialOrd for FeelFunction {
  /// Sort by the Function name.
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    self.get_name().partial_cmp(&other.get_name())
  }
}

impl Ord for FeelFunction {
  fn cmp(&self, other: &Self) -> Ordering {
      self.partial_cmp(other).unwrap()
  }
}

impl Debug for FeelFunction {
  fn fmt(&self, f: &mut Formatter) -> Result {
    write!(f, "{:?} Function {}", self.function_type, self.get_name())
  }
}

impl Display for FeelFunction {
  fn fmt(&self, f: &mut Formatter) -> Result {
      write!(f, "{:?}", self)
  }
}

/// By implementing Deref in this way, a FeelFunction may be called as though it were a function!
/// 
/// When calling the function, the first parameter is &FeelValue and the second &mut NestedContext.
impl Deref for FeelFunction {
  type Target = dyn Fn(&FeelValue, &mut NestedContext) -> FeelValue;

  fn deref(&self) -> &Self::Target {
      &(*self.function_container)
  }
}

/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
  use std::assert_ne;
  use super::super::feel_value::{FeelValue};
  use super::super::nested_context::NestedContext;
  use super::FeelFunction;

  fn make_identity_function() -> FeelFunction {
    let f = move |value: &FeelValue, _ctx: &mut NestedContext| -> FeelValue {
      value.clone()
    };
    let ff = FeelFunction::new_user(f);
    ff
  }

  #[test]
  fn test_set_name() {
    let ff = make_identity_function();
    let name = "identity".to_string();
    ff.set_name(name.clone());
    assert_eq!(name, ff.get_name(), "get_name of FeelFunction");
  }

  #[test]
  fn test_cmp_user_function() {
    let ff = make_identity_function();
    assert_eq!(ff, ff, "compare same ptr");

    let ff2 = make_identity_function();
    assert_ne!(ff, ff2, "compare two anonymous functions");
  }

  #[test]
  fn test_call_user_function() {
    let mut ctx = NestedContext::new();
    let ff = make_identity_function();
    let parameter: FeelValue = 42.into();
    let result = ff(&parameter, &mut ctx);
    assert_eq!(parameter, result, "Result should be a clone of parameter");
  }

}
