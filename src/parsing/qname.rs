use std::fmt;

#[derive(Clone, Debug)]
pub struct QName {
  parts: Vec<String>
}

impl fmt::Display for QName {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.parts.join(" "))
  }
}

impl PartialEq for QName {
  fn eq(&self, other: &Self) -> bool {
      self.parts.len() == other.parts.len()
      && self.parts.iter().zip(other.parts.iter()).all(|(a, b)| a == b)
  }
}

impl Eq for QName {}

#[cfg(test)]
mod tests {
  use super::QName;

  macro_rules! vec_of_strings {
    ($($x:expr),*) => (vec![$($x.to_string()),*]);
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
}
