use std::cmp::{Ord, Ordering};
use super::qname::QName;
use super::feel_value::{FeelValue};
use super::context::{Context};
use super::execution_log::ExecutionLog;

/// Matches names of the formal parameters by position 
/// to values in a FeelValue::List to create a Context.
/// This is for use in implementing user defined functions.
pub struct ParameterBinding {
  /// Names of the formal parameters
  names: Vec<QName>
}

impl ParameterBinding {
  pub fn new(parameter_names: Vec<QName>) -> Self {
    ParameterBinding {
      names: parameter_names
    }
  }

  pub fn arity(&self) -> usize {
    self.names.len()
  }

  pub fn null_context(&self) -> Context {
    let ctx = Context::new();
    for key in self.names.iter() {
      ctx.insert(key.clone(), FeelValue::Null);
    }
    ctx
  }

  /// Assuming that arguments is a FeelValue::List, match each 
  /// corresponding FeelValue in that list to a parameter name
  /// and construct a Context from it.
  ///   - If arguments is not a FeelValue::List, 
  ///     match it to the first parameter name.
  ///   - If arguments is a list that has fewer items than the number 
  ///     of names, supply FeelValue::Null for the unmatched parameters
  ///     and log this discrepancy.
  ///   - If arguments is a list that has more items than the number
  ///     of names, discard the extra values and log this discrepancy.
  pub fn bind(&self, arguments: FeelValue) -> Context {
    let ctx = self.null_context();
    let expected_arity = self.arity();
    let actual_arity: usize = match &arguments {
      FeelValue::List(rr_list) => {
        let arity = rr_list.borrow().len();
        for (position, item) in rr_list.borrow().iter().enumerate() {
          if position < expected_arity {
            ctx.insert(self.names[position].clone(), item.clone());
          }
        }
        arity
      },
      _ => { 
        ctx.insert(self.names[0].clone(), arguments.clone());
        1
      }
    };
    match actual_arity.cmp(&expected_arity) {
      Ordering::Less => {
        ExecutionLog::log(&format!("Function called with {:?} arguments (too few), expected {:?}, supplying nulls for the others", actual_arity, expected_arity));
      },
      Ordering::Greater => {
        ExecutionLog::log(&format!("Function called with {:?} arguments (too many), expected {:?}, discarding the others", actual_arity, expected_arity));
      },
      Ordering::Equal => {
        ()
      }
    };
    ctx
  }
}

/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
  use super::ParameterBinding;
  use super::super::feel_value::FeelValue;
  use super::super::context::ContextReader;
  use super::super::qname::QName;

  /// Test Bind a FeelValue::List to the parameters.
  #[test]
  fn test_bind_list() {
    let names: Vec<QName> = vec!["name".into(), "age".into()];
    let bindings = ParameterBinding::new(names);
    let m: FeelValue = "Methuselah".into();
    let values = FeelValue::new_list(vec![m.clone(), 969.into()]);
    let ctx = bindings.bind(values);
    assert_eq!(ctx.get("name"), Some(m), "check binding of name parameter");
    assert_eq!(ctx.get("age"), Some(FeelValue::Number(969.0)), "check binding of age parameter");
  }

  /// Test Bind a scalar (not a FeelValue::List) to the parameters and expect 
  /// a null to be added for the missing parameter.
  #[test]
  fn test_bind_scalar() {
    let names: Vec<QName> = vec!["name".into(), "age".into()];
    let bindings = ParameterBinding::new(names);
    let m: FeelValue = "Methuselah".into();
    let ctx = bindings.bind(m.clone());
    assert_eq!(ctx.get("name"), Some(m), "check binding of name parameter");
    assert_eq!(ctx.get("age"), Some(FeelValue::Null), "check binding of age parameter");
  }

}