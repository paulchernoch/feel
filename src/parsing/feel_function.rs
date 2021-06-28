use std::ptr;
use std::rc::Rc;
use std::cell::RefCell;
use std::convert::From;
use std::string::ToString;
use std::ops::Deref;
use std::cmp::{Ord, PartialOrd, Ordering};
use std::fmt::{Debug,Display,Formatter,Result};
use super::feel_value::{FeelValue};
use super::nested_context::NestedContext;
use super::qname::QName;
use super::lattice_type::LatticeType;

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
  /// The function name, being a QName, can have spaces in it!
  name_container: RefCell<QName>,
  function_container: Rc<dyn Fn(&FeelValue, &mut NestedContext) -> FeelValue>,

  /// Defines the argument types and return type of the function.
  lattice_type: LatticeType,
}

impl FeelFunction {
  /// Create a builtin FeelFunction, required to have a name.
  pub fn new_builtin<Q: Into<QName>>(name: Q, lattice_type: LatticeType, func: impl Fn(&FeelValue, &mut NestedContext) -> FeelValue + 'static) -> Self {
    FeelFunction {
      function_type: FunctionType::Builtin,
      name_container: RefCell::new(name.into()),
      function_container: Rc::new(func),
      lattice_type: lattice_type
    }
  }

  /// Create an anonymous (unnamed) user defined FeelFunction.
  /// Its name may be set after creation via set_name.
  pub fn new_user(lattice_type: LatticeType, func: impl Fn(&FeelValue, &mut NestedContext) -> FeelValue + 'static) -> Self {
    FeelFunction {
      function_type: FunctionType::User,
      name_container: RefCell::new(ANONYMOUS.into()),
      function_container: Rc::new(func),
      lattice_type: lattice_type
    }
  }

  /// Get a clone of the function name.
  pub fn get_name(&self) -> QName {
    self.name_container.borrow().clone()
  }

  pub fn get_lattice_type(&self) -> &LatticeType {
    &self.lattice_type
  }

  pub fn get_return_type(&self) -> LatticeType {
    match &self.lattice_type {
      LatticeType::Function { return_type, .. } => return_type.as_ref().clone(),
      _ => unreachable!()
    }
  }

  /// Set the function name. 
  /// When parsing the Feel expression, the most common idiom is to 
  /// define a user defined function and stuff it into a Context.
  /// The parser in such cases will need to set the function name
  /// after definition time as soon as it learns the Context key to 
  /// use as the function name.
  /// Note that setting the function's name does not require a mutable reference.
  pub fn set_name<Q: Into<QName> + Clone>(&self, new_name: &Q) -> QName {
    let new_qname: QName = new_name.clone().into();
    self.name_container.replace(new_qname)
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
    if name.to_string() == ANONYMOUS {
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
  use std::rc::Rc;
  use super::super::feel_value::{FeelValue};
  use super::super::nested_context::NestedContext;
  use super::FeelFunction;
  use super::super::qname::QName;
  use super::super::lattice_type::LatticeType;

  fn make_identity_function() -> FeelFunction {
    let f = move |value: &FeelValue, _ctx: &mut NestedContext| -> FeelValue {
      value.clone()
    };
    let lattice_type = LatticeType::homogeneous_function(1, LatticeType::Any);
    let ff = FeelFunction::new_user(lattice_type, f);
    ff
  }

  #[test]
  fn test_set_name() {
    let ff = make_identity_function();
    let name:QName = "identity".into();
    ff.set_name(&name);
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

  #[derive(Clone)]
  pub struct MockCompiledExpression {}

  impl MockCompiledExpression {
    pub fn exec(&self, value: &FeelValue, _ctx: &mut NestedContext) -> FeelValue {
      FeelValue::String("Hello".to_string())
    }

    pub fn compile(&self) -> Rc<dyn Fn(&FeelValue, &mut NestedContext) -> FeelValue> 
    {
      let c = self.clone();
      let f = move |value: &FeelValue, ctx: &mut NestedContext| -> FeelValue {
        c.exec(value, ctx)
      };
      Rc::new(f)
    }
  }

}
