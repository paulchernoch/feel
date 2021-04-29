use std::cell::RefCell;
use std::rc::Rc;
use std::hash::Hash;
use std::collections::HashMap;
use std::fmt::{Debug,Display,Formatter,Result};
use super::qname::QName;
use super::feel_value::FeelValue;

// TODO: Permit changing the value of a key for when we have an iteration context,
// used when iterating over lists. See §10.3.2.14 of the DMN Spec version 1.3.

/// A Context is a dictionary which corresponds in the Feel specification to a context or a scope.
/// 
/// Interior mutability is required to extend the dictionary by adding key-value pairs.
/// No Keys may be changed or removed.
/// No values may be mutated unless they are FeelValue::Context or FeelValue::List values.
#[derive(Eq, Clone, Debug)]
pub struct Context {
  contents: RefCell<HashMap<QName, FeelValue>>
}

impl Context {
  /// Insert a key-value pair into the context and return the prior value, if any.
  pub fn insert<Q: Into<QName>>(&self, k: Q, v: FeelValue) -> Option<FeelValue> {
    self.contents.borrow_mut().insert(k.into(), v)
  }

  pub fn new() -> Self {
    Context { 
      contents: RefCell::new(HashMap::new())
    }
  }

  fn new_from_pair(k: QName, v: FeelValue) -> Self {
    let ctx = Self::new();
    ctx.insert("key", FeelValue::Name(k));
    ctx.insert("value", v);
    ctx
  }

  /// Generate a list of Contexts where each has two keys, "key" and "value",
  /// one context for each key-value pair in this context. 
  /// If this context is empty, an empty list will be constructed. 
  pub fn get_entries(&self) -> FeelValue {
    let pairs: Vec<FeelValue> = self.contents
      .borrow()
      .iter()
      .map(|(k,v)| FeelValue::Context(Rc::new(Self::new_from_pair(k.clone(), v.clone()))))
      .collect();
    FeelValue::new_list(pairs)
  }

  /// Generate a FeelValue::List holding a sorted list of Contexts where each has two keys, "key" and "value",
  /// one context for each key-value pair in this context. 
  /// If this context is empty, an empty list will be constructed. 
  /// Sorting is by the key.
  pub fn get_entries_sorted(&self) -> FeelValue {
    FeelValue::new_list(self.get_entries_sorted_as_vec())
  }

  /// Generate a Vec holding a sorted list of Contexts where each has two keys, "key" and "value",
  /// one context for each key-value pair in this context. 
  /// If this context is empty, an empty list will be constructed. 
  /// Sorting is by the key.
  pub fn get_entries_sorted_as_vec(&self) -> Vec<FeelValue> {
    let key_key: QName = "key".into();
    let mut pairs: Vec<FeelValue> = self.contents
      .borrow()
      .iter()
      .map(|(k,v)| FeelValue::Context(Rc::new(Self::new_from_pair(k.clone(), v.clone()))))
      .collect();
    pairs.sort_by(|a, b| a.try_get(&key_key).unwrap().cmp(&b.try_get(&key_key).unwrap()));
    pairs
  }
}

pub trait ContextReader {
  /// Does the Context contain the given key?
  /// If given a String key, it will be parsed into a QName before checking for the key.
  fn contains_key<Q: Into<QName>>(&self, k: Q) -> bool;

  /// Get the value associated with the key, if any.
  fn get<Q: Into<QName> + Clone>(&self, k: Q) -> Option<FeelValue>;
}

impl ContextReader for Context {
  /// Does the Context contain the given key?
  /// If given a String key, it will be parsed into a QName before checking for the key.
  fn contains_key<Q: Into<QName>>(&self, k: Q) -> bool {
    self.contents.borrow().contains_key(&k.into())
  }

  /// Get the value associated with the key, if any.
  fn get<Q: Into<QName> + Clone>(&self, k: Q) -> Option<FeelValue> {
    match self.contents.borrow().get(&k.into()) {
      Some(value) => Some(value.clone()),
      None => None
    }
  }
}

fn are_hashmaps_equal<K: Debug + Eq + Hash, V: Eq + Debug>(left_map: &HashMap<K, V>, right_map: &HashMap<K, V>) -> bool {
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
  use super::{Context, ContextReader};

  #[test]
  fn test_contains_key() {
    let ctx = Context::new();
    assert!(!ctx.contains_key("key"), "contains_key on Empty Context");

    let previous = ctx.insert("key", FeelValue::Number(1.0));
    assert_eq!(previous, Option::None, "insert of new key should return None");

    assert!(ctx.contains_key("key"), "contains_key on key that is present");
  }

}
