use std::cell::RefCell;
use std::hash::Hash;
use std::collections::HashMap;
use std::fmt::{Debug,Display,Formatter,Result};
use super::qname::QName;
use super::feel_value::FeelValue;


/// A Feel Context is a dictionary.
/// 
/// Interior mutability is required to extend the dictionary by adding key-value pairs.
/// No Keys may be changed or removed.
/// No values may be mutated unless they are FeelValue::Context or FeelValue::List values.
#[derive(Eq, Clone, Debug)]
pub struct Context {
  contents: RefCell<HashMap<QName, FeelValue>>
}

impl Context {
  /// Does the Context contain the given key?
  /// If given a String key, it will be parsed into a QName before checking for the key.
  pub fn contains_key<Q: Into<QName>>(&self, k: Q) -> bool {
    self.contents.borrow().contains_key(&k.into())
  }

  /// Get the value associated with the key, if any.
  pub fn get<Q: Into<QName>>(&self, k: Q) -> Option<FeelValue> {
    match self.contents.borrow().get(&k.into()) {
      Some(value) => Some(value.clone()),
      None => None
    }
  }

  /// Insert a key-value pair into the context and return the prior value, if any.
  pub fn insert<Q: Into<QName>>(&self, k: Q, v: FeelValue) -> Option<FeelValue> {
    self.contents.borrow_mut().insert(k.into(), v)
  }

  pub fn new() -> Self {
    Context { 
      contents: RefCell::new(HashMap::new())
    }
  }
}

fn are_hashmaps_equal<K: Debug + Eq + Hash, V: Eq>(left_map: &HashMap<K, V>, right_map: &HashMap<K, V>) -> bool {
  for (k, left_v) in left_map.iter() {
      let matching = match right_map.get(k) {
        Some(right_v) => left_v == right_v,
        None => false
      };
      if !matching {
        return false
      }
  }
  true
}

impl PartialEq for Context {
  fn eq(&self, other: &Self) -> bool {
    are_hashmaps_equal(&self.contents.borrow(), &other.contents.borrow())
  }
}

impl Display for Context {
  // This trait requires `fmt` with this exact signature.
  fn fmt(&self, f: &mut Formatter) -> Result {
    let mut s = String::with_capacity(1000);
    for (key, val) in self.contents.borrow().iter() {
      s.push_str(&format!("{key} : {val},", key=key, val=val));
    }
    write!(f, "{{ {} }}", s)
  }
}

/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
  use super::super::feel_value::FeelValue;
  use super::Context;

  #[test]
  fn test_contains_key() {
    let ctx = Context::new();
    assert!(!ctx.contains_key("key"), "contains_key on Empty Context");

    let previous = ctx.insert("key", FeelValue::Number(1.0));
    assert_eq!(previous, Option::None, "insert of new key should return None");

    assert!(ctx.contains_key("key"), "contains_key on key that is present")
  }

}
