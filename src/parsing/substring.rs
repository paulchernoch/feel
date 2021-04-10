use std::cmp;

pub trait Substring {
    /// Get a substring from a string that counts unicode characters, not bytes. 
    ///   start ..... If positive, then a zero-based starting position within the string. 
    ///               If negative, then a stgarting position relative to the end of the string. 
    ///               If the negative value is too large, truncate it to the start of the string. 
    ///   len ....... Optional number of characters (not bytes) to extract. 
    ///               If None, include all characters from start to the end of the string. 
    fn substring(&self, start: i32, len: Option<usize>) -> String;
}

impl Substring for str {
    /// Get a substring from a string that counts unicode characters, not bytes. 
    ///   start ..... If positive, then a zero-based starting position within the string. 
    ///               If negative, then a stgarting position relative to the end of the string. 
    ///               If the negative value is too large, truncate it to the start of the string. 
    ///   len ....... Optional number of characters (not bytes) to extract. 
    ///               If None, include all characters from start to the end of the string. 
    /// Unusually large values for start or optional_length will be truncated to reasonable values. 
    fn substring(&self, start: i32, optional_length: Option<usize>) -> String {
        let total_length = self.chars().count() as i32;
        let start_position = 
          if total_length == 0_i32 { 0_i32 }
          else if start < 0 { cmp::max(0_i32, total_length + start) } 
          else { cmp::min(total_length, start) };

        let max_len = (total_length - start_position) as usize;
        let copy_length: usize = match optional_length {
            None => max_len,
            Some(len) => cmp::min(max_len, len as usize)
        };
        self.chars().skip(start_position as usize).take(copy_length).collect::<String>()
    }
}



#[cfg(test)]
mod tests {
  use super::super::substring::Substring;

  #[test]
  fn test_substring() {
    let s = &"Hello World";
    
    assert_eq!("Hello", s.substring(0, Some(5)));
    assert_eq!("World", s.substring(6, None));
    assert_eq!("ll", s.substring(2, Some(2)));
    assert_eq!("ld", s.substring(-2, None));
    assert_eq!("lo", s.substring(-8, Some(2)));
    assert_eq!("", s.substring(20, None));
  }

}