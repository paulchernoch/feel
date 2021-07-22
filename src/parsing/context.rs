use std::cell::RefCell;
use std::rc::Rc;
use std::hash::Hash;
use std::collections::HashMap;
use std::fmt::{Debug,Display,Formatter,Result};
use super::qname::QName;
use super::feel_value::FeelValue;
use super::lattice_type::{LatticeType,ContextTypeBuilder};

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
  /// Note: Interior mutability exploited so that a non-mutable reference may be passed.
  pub fn insert<Q: Into<QName>>(&self, k: Q, v: FeelValue) -> Option<FeelValue> {
    self.contents.borrow_mut().insert(k.into(), v)
  }

  /// Create a new empty Context.
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

  pub fn infer_lattice_type<C: ContextReader>(&self, contexts: &C) -> LatticeType {
    let mut builder = ContextTypeBuilder::new();
    for (k,v) in self.contents.borrow().iter() {
        builder = builder.add_key(k.to_string(), LatticeType::from_value(v, contexts));
    }
    builder.build()
  }
}

/// The readonly behavior of a context, including contains_key and get. 
pub trait ContextReader {
  /// Does the Context contain the given key?
  /// If given a String key, it will be parsed into a QName before checking for the key.
  fn contains_key<Q: Into<QName>>(&self, k: Q) -> bool;

  /// Get the value associated with the key, if any.
  fn get<Q: Into<QName> + Clone>(&self, k: Q) -> Option<FeelValue>;

  /// If the Context is an iterator (such as an IterationContext in a for loop)
  /// advance the pointer to the next in the series. 
  /// A false return means that the Context is not an iterator OR
  /// it is an iterator but there are no more items to yield. 
  fn move_next(&self) -> bool;

  /// The length of the series of items that this context can generate by calling move_next. 
  /// If this is not an iterable context then return one. 
  /// If an iterable context (like in a for loop) then return the number of items to be looped over. 
  /// The iteration may stop before all such items are generated, such as for the 
  /// "some" and "every" features. 
  fn length(&self) -> usize;

  
}

/// Ability to increment the value of a key in a context whose value is a number.
pub trait ContextIncrement {
  /// Increment the value associated with a key by one if possible. 
  /// 
  ///   k ............. Key to increment
  ///   default ....... Value to set if key is missing from Context. 
  ///                   If None, do not insert the missing key.
  /// 
  /// Returns the incremented value if successful. 
  /// Returns None if the key is missing and None given as default,
  /// or the key is present but its current value is not a Number.
  fn increment<Q: Into<QName>>(&self, k: Q, default: Option<f64>) -> Option<FeelValue>;
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

  fn move_next(&self) -> bool {
    false
  }

  fn length(&self) -> usize {
    1
  }
}

impl ContextIncrement for Context {
  /// Increment the value associated with a key by one if possible. 
  /// 
  ///   k ............. Key to increment
  ///   default ....... Value to set if key is missing from Context. 
  ///                   If None, do not insert the missing key.
  /// 
  /// Returns the incremented value if successful. 
  /// Returns None if the key is missing and None given as default,
  /// or the key is present but its current value is not a Number.
  fn increment<Q: Into<QName>>(&self, k: Q, default: Option<f64>) -> Option<FeelValue> {
    let key: QName = k.into();
    match (self.get(key.clone()), default) {
      (Some(FeelValue::Number(current_value)), _) => {
        let new_value = FeelValue::Number(current_value + 1.0);
        self.insert(key, new_value.clone());
        Some(new_value)
      },
      (None, Some(default_value)) => {
        let new_value = FeelValue::Number(default_value);
        self.insert(key, new_value.clone());
        Some(new_value)
      },
      _ => None
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
  use super::{Context, ContextReader, ContextIncrement};

  #[test]
  fn test_contains_key() {
    let ctx = Context::new();
    assert!(!ctx.contains_key("key"), "contains_key on Empty Context");

    let previous = ctx.insert("key", FeelValue::Number(1.0));
    assert_eq!(previous, Option::None, "insert of new key should return None");

    assert!(ctx.contains_key("key"), "contains_key on key that is present");
  }

  #[test]
  fn test_increment() {
    let ctx = Context::new();
    ctx.insert("count", 0.into());
    assert_eq!(FeelValue::Number(1.0), ctx.increment("count", None).unwrap());
    assert_eq!(FeelValue::Number(2.0), ctx.increment("count", None).unwrap());
  }

}
