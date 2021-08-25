use std::fmt;
use std::ops;
use std::cmp::Ordering;
use std::str::FromStr;
use std::convert::From;

/// A name, consisting of a list of strings.
/// Names may have spaces in them. 
/// NOTE: Due to the developer's confusion, a QName is not a
/// qualified name. A Qualified name is a series of names
/// seprated by periods.
#[derive(Eq, Clone, Debug, Hash)]
pub struct QName {
  parts: Vec<String>
}

impl QName {
  pub fn new<S>(name: &S) -> Self where S: Into<String> + Clone {
    QName { 
      // parts: vec![name.into()] 
      parts: name.clone().into().split_whitespace().map(|s| s.to_string()).collect()
    }
  }
}

impl fmt::Display for QName {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.parts.join(" "))
  }
}

#[derive(Debug, Clone)]
pub struct ParseQNameError;

impl fmt::Display for ParseQNameError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "unable to parse QName from string")
  }
}

impl FromStr for QName {
  type Err = ParseQNameError;

  /// Split a single string into multiple parts on whitespace and create a QName from them.
  /// If multiple consecutive whitespace characters are encountered, blank words will be skipped.
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    Result::Ok(QName { parts : s.to_string().split_whitespace().map(|s| s.to_string()).collect() })
  }
}

pub trait Stringlike {}
impl Stringlike for String {}
impl Stringlike for &str {}

impl<S: Into<String> + Stringlike> From<S> for QName {
  fn from(item: S) -> Self {
    QName { parts : item.into().split_whitespace().map(|s| s.to_string()).collect() }
  }
}

impl<S: Into<String> + Clone + Stringlike> From<&Vec<S>> for QName {
  fn from(v: &Vec<S>) -> Self {
    QName{ parts : v.iter().map(|s| s.clone().into()).collect() }
  }
}

impl PartialEq for QName {
  fn eq(&self, other: &Self) -> bool {
      self.parts.len() == other.parts.len()
      && self.parts.iter().zip(other.parts.iter()).all(|(a, b)| a == b)
  }
}

impl PartialOrd for QName {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    self.to_string().partial_cmp(&other.to_string())
  }
}

impl Ord for QName {
  fn cmp(&self, other: &Self) -> Ordering {
    self.partial_cmp(other).unwrap()
  }
}


impl<S> ops::Add<S> for &QName where S: Into<String> {
  type Output = QName;
  fn add(self, rhs: S) -> QName {
    let mut new_parts = self.parts.clone();
    new_parts.push(rhs.into());
    QName { parts: new_parts }
  }
}

#[cfg(test)]
mod tests {
  use super::QName;
  use std::collections::hash_map::DefaultHasher;
  use std::hash::{Hash, Hasher};

  macro_rules! vec_of_strings {
    ($($x:expr),*) => (vec![$($x.to_string()),*]);
  }

  fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
  }

  #[test]
  fn test_eq() {
    let q1 = QName { parts: vec_of_strings!["John", "Smith"] };
    let q2 = QName { parts: vec_of_strings!["John", "Smith"] };
    let q3 = QName { parts: vec_of_strings!["Jane", "Smith"] };
    let q4 = QName { parts: vec_of_strings!["John", "Smith", "Jr"] };
    assert_eq!(q1, q2);
    assert_ne!(q1, q3);
    assert_ne!(q1, q4);
  }

  #[test]
  fn test_qname_hash() {
    let q1 = &QName::new(&"Sam") + "Hill";
    let q2 = &QName::new(&"Sam") + "Hill";
    assert_eq!(
      calculate_hash(&q1),
      calculate_hash(&q2)
    )
  }

  #[test]
  fn test_qname_fromstr() {
    let q1: QName = "John Smith".parse().unwrap();
    assert_eq!(
      QName { parts: vec_of_strings!["John", "Smith"] },
      q1,
    )
  }

  #[test]
  fn test_qname_fromstr_extra_spaces() {
    let q1: QName = "John   Smith".parse().unwrap();
    assert_eq!(
      QName { parts: vec_of_strings!["John", "Smith"] },
      q1,
    )
  }

  #[test]
  fn test_new() {
    let q1 = QName::new(&"Faux pas");
    let q2 = "Faux   pas".parse().unwrap();
    assert_eq!(q1, q2);
  }
}
