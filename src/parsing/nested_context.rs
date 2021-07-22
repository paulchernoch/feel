use super::feel_value::{FeelValue, FeelType};
use super::qname::QName;
use super::context::{ContextReader,ContextIncrement};

/// In Feel, a scope is a list of contexts, called here a NestedContext.
/// 
/// In the DMN Spec version 1.3, section 10.3.2.11, it says that the contexts are evaluated 
/// from first to last, with the last context being the builtin context.
/// This structure evaluates them from last to first, but stores the contexts in
/// a stack that holds the values in reverse order, so the end result is the same.
pub struct NestedContext {
  /// The stack holds FeelValues that must all be Contexts.
  stack: Vec<FeelValue>
}

impl NestedContext {
  pub fn new() -> Self {
    NestedContext {
      stack: Vec::new()
    }
  }

  /// Push a new Context onto the stack, returning true if successful, false if the value given is not a context.
  pub fn push(&mut self, context: FeelValue) -> bool {
    match context.get_type() {
      FeelType::Context => {
        self.stack.push(context);
        true
      },
      _ => false
    }
  }

  /// Pop the top (last) context from the stack and return it in an Option, or return None if the stack is empty.
  pub fn pop(&mut self) -> Option<FeelValue> {
    self.stack.pop()
  }

  /// Return a clone of the top of the stack, if there is any. 
  /// Changes made to this clone will affect the original, due to internal mutability. 
  /// This leaves the stack unchanged - it does not pop the context off. 
  pub fn peek(&self) -> Option<FeelValue> {
    if self.stack.len() == 0 {
      None
    }
    else {
      Some(self.stack.last().unwrap().clone())
    }
  }

}

impl ContextReader for NestedContext {
  fn contains_key<Q: Into<QName>>(&self, k: Q) -> bool {
    let key = k.into();
    for item in self.stack.iter().rev() {
      match item {
        FeelValue::Context(ctx) => {
          if ctx.contains_key(key.clone()) {
            return true;
          }
          else {
            ()
          }       
        },
        _ => ()
      };
    }
    false
  }

  /// Get the value associated with the key, if any, starting from the context at the top of the stack.
  fn get<Q: Into<QName> + Clone>(&self, k: Q) -> Option<FeelValue> {
    let key = k.into();
    for item in self.stack.iter().rev() {
      match item {
        FeelValue::Context(ctx) => {
          let value = ctx.get(key.clone());
          if value.is_some() {
            return value;
          }        
        },
        _ => ()
      };
    }
    Option::None
  }

  fn move_next(&self) -> bool {
    false
  }

  fn length(&self) -> usize {
    1
  }
}

impl ContextIncrement for NestedContext {
  /// Increment the value associated with a key by one if possible. 
  /// If default is not None and the key is missing from all child contexts,
  /// then add the key to the topmost context with the default value. 
  /// 
  ///   k ............. Key to increment
  ///   default ....... Value to set if key is missing from all child Contexts in this NestedContext. 
  ///                   If None, do not insert the missing key.
  /// 
  /// Returns the incremented value if successful. 
  /// Returns None if the key is missing and None given as default,
  /// or the key is present but its current value is not a Number.
  fn increment<Q: Into<QName>>(&self, k: Q, default: Option<f64>) -> Option<FeelValue> {
    let key: QName = k.into();
    for item in self.stack.iter().rev() {
      match item {
        FeelValue::Context(ctx) => {
          let value = ctx.increment(key.clone(), None);
          if value.is_some() {
            return value;
          }        
        },
        _ => ()
      };
    }
    // Key not found in any child context on the stack. Optionally add a default value to the top
    // of the stack. 
    match (default, self.stack.last()) {
      (Some(default_value), Some(FeelValue::Context(top_ctx))) => {
        let new_value = FeelValue::Number(default_value);
        top_ctx.insert(key, new_value.clone());
        Some(new_value)
      },
      _ => None
    }
  }
}

/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
  use std::rc::Rc;
  use super::super::feel_value::{FeelValue};
  use super::super::context::{Context,ContextReader};
  use super::NestedContext;

  fn make_test_data() -> NestedContext {
    let ctx1 = Context::new();
    ctx1.insert("First name", "Paul".into());
    ctx1.insert("Favorite number", 28.into());

    let ctx2 = Context::new();
    ctx2.insert("Favorite color", "teal".into());
    ctx1.insert("Favorite number", 42.into());

    let mut stack = NestedContext::new();
    stack.push(FeelValue::Context(Rc::new(ctx1)));
    stack.push(FeelValue::Context(Rc::new(ctx2)));

    stack
  }

  #[test]
  fn test_get() {
    let stack = make_test_data();

    let expected_name: FeelValue = "Paul".into();
    let actual_name = stack.get("First name").unwrap();
    assert_eq!(expected_name, actual_name, "get key missing from top context");

    let expected_number: FeelValue = 42.into();
    let actual_number = stack.get("Favorite number").unwrap();
    assert_eq!(expected_number, actual_number, "get key present in top context");
  }

  #[test]
  fn test_get_missing_key() {
    let stack = make_test_data();
    let actual_value = stack.get("Missing key");
    assert_eq!(true, actual_value.is_none(), "get of missing key should return None")
  }

}
