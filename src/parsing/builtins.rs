use std::rc::Rc;
use std::cell::Ref;
use std::cmp::Ordering;
use std::ops::Bound;
use regex::Regex; // TODO: Should have a Regex LRU cache.
use math::round;
use std::collections::HashSet;
use std::convert::{TryFrom,TryInto};
use super::range::Range;
use super::context::{Context,ContextReader};
use super::feel_value::{FeelValue, FeelType};
use super::execution_log::ExecutionLog;
use super::arguments::{Arguments,Validity};
use super::substring::Substring;
use super::statistics::{sample_standard_deviation, mode_with_ties, MedianIndex};
use super::statistics::median as stats_median;

pub enum RangeCase {
  PointRange,
  PointPoint,
  RangePoint,
  RangeRange
}

/// A Factory for creating a Context containing all the builtin functions needed by the Feel Language.
/// 
/// In Feel, builtins take one to four arguments (or more, for functions like sum, product, min, and max). 
/// These FeelFunctions will all take two arguments: 
///    - parameters: an Arguments object that holds the argument list
///      that conforms to the Feel specification. 
///      If named arguments are used, this may be a context,
///      which names the supplied parameter values.
///    - context: the currently in scope context, available in case the
///      function needs global context to perform its work. 
/// 
pub struct Builtins {}

impl Builtins {
  /// Create a Context containing all the builtin functions needed by the Feel Language.
  /// 
  /// The keys in the Context are the names of the functions (as QNames).
  /// All the values are FeelValue::Function variants.
  pub fn new_context() -> Context {
    let builtin_context = Context::new();

    builtin_context
  }

  /// Prepare the Arguments object and a validator. 
  /// If the supplied parameters is a single FeelValue::List, it will be flattened into a list of
  /// possibly many arguments.
  fn make_validator(function_name: &str, parameters: FeelValue) -> Validity {
    let args: Rc<Arguments> = Rc::new(parameters.into()); // Flattens if a FeelValue::List.
    let name: Rc<String> = Rc::new(function_name.into());
    Validity::new(&name, &args)
  }

  /// Prepare the Arguments object and a validator for situations where the argument is a FeelValue::List
  /// that should not be flattened into a longer parameter list. 
  fn make_list_validator(function_name: &str, parameters: FeelValue) -> Validity {
    let args: Rc<Arguments> = Rc::new(Arguments::new1(parameters)); // No flattening.
    let name: Rc<String> = Rc::new(function_name.into());
    Validity::new(&name, &args)
  }

  //// ////////////////////////////////////////////////
  ////                                             ////
  ////             Boolean function                ////
  ////                                             ////
  //// ////////////////////////////////////////////////
  
  /// not(b) returns false for a true value, true for a false, and Null for all others.
  pub fn not<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "not";
    match Builtins::make_validator(fname, parameters)
      .arity(1..2)
      .no_nulls()
      .expect_type(0_usize, FeelType::Boolean, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::Boolean(b) => (!b).into(),
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  //// ////////////////////////////////////////////////
  ////                                             ////
  ////             String functions                ////
  ////                                             ////
  //// ////////////////////////////////////////////////
  
  fn string_transform<C: ContextReader, F: FnOnce(&String) -> FeelValue>(parameters: FeelValue, _contexts: &C, fname: &str, xform: F) -> FeelValue {
    match Builtins::make_validator(fname, parameters)
      .arity(1..2)
      .no_nulls()
      .expect_type(0_usize, FeelType::String, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::String(s) => xform(s),
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// Helper function for performing a tailored search of a string for a match.
  fn string_match<C: ContextReader, F: FnOnce(&String, &String) -> FeelValue>(parameters: FeelValue, _contexts: &C, fname: &str, matcher: F) -> FeelValue {
    match Builtins::make_validator(fname, parameters)
      .arity(2..3)
      .no_nulls()
      .expect_type(0_usize, FeelType::String, false)
      .expect_type(1_usize, FeelType::String, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        let b = &arguments[1];
        match (a, b) {
          (FeelValue::String(search_string), FeelValue::String(match_string)) => matcher(search_string, match_string),
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// Validate regex flags: only i, s, m and x permitted. 
  fn are_flags_valid<S: Into<String>>(flags: S) -> bool {
    let string_flags: String = flags.into();
    let mut found = 0_usize;
    if string_flags.contains('i') { found += 1_usize; }
    if string_flags.contains('s') { found += 1_usize; }
    if string_flags.contains('m') { found += 1_usize; }
    if string_flags.contains('x') { found += 1_usize; }
    found == string_flags.len()
  }
  

  // substring(string, start position, length?) returns a portion of the input string, where start position 
  // may be negative to count from the end of the string and length is optional, meaning to include 
  // all remaining characters.
  // start position is one based.
  pub fn substring<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "substring";
    match Builtins::make_validator(fname, parameters)
      .arity(2..4)
      .expect_type(0_usize, FeelType::String, false)
      .expect_integer(1_usize, false)
      .expect_integer(2_usize, true) // Optional length parameter, so can be Null
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        let b = &arguments[1];
        let c = &arguments[2];
        // Ensure that one-based positions are converted to zero-based before calling internal substring.
        match (a, b, c) {
          (_, FeelValue::Number(start_position), _) if *start_position == 0.0 => {
            ExecutionLog::log(&format!("{:?} called with a position of zero. Positions are one-based.", fname));
            FeelValue::Null
          },
          (FeelValue::String(search_string), FeelValue::Number(start_position), FeelValue::Null) => {
            let position = if *start_position > 0.0 { *start_position as i32 - 1 } else { *start_position as i32 };
            FeelValue::String(search_string.substring(position, None))
          },
          (FeelValue::String(search_string), FeelValue::Number(start_position), FeelValue::Number(length)) => {
            let position = if *start_position > 0.0 { *start_position as i32 - 1 } else { *start_position as i32 };
            FeelValue::String(search_string.substring(position, Some(*length as usize)))
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// string length(string) returns the number of characters in the string.
  /// Note that this does not count the number of bytes in the string, but the number of Unicode code points. 
  /// An accented non-Latin character may contain two bytes but count as one character.
  pub fn string_length<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    Builtins::string_transform(parameters, _contexts, "string length", |s| FeelValue::Number(s.chars().count() as f64))
  }

  /// upper case(string) returns the string with all lowercase characters made uppercase.
  pub fn upper_case<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    Builtins::string_transform(parameters, _contexts, "upper case", |s| s.to_uppercase().into())
  }

  // lower case(string) returns the string with all uppercase characters made lowercase.
  /// upper case(string) returns the string with all lowercase characters made uppercase.
  pub fn lower_case<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    Builtins::string_transform(parameters, _contexts, "lower case", |s| s.to_lowercase().into())
  }

  /// substring before(string, match) returns all the string that comes before the match, or an empty string if no match.
  pub fn substring_before<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    Builtins::string_match(
      parameters, 
      _contexts, 
      "substring before", 
      |search_string, match_string| {
        match search_string.find(match_string.as_str()) {
          Some(position) => search_string[0..position].to_string().into(),
          None => "".into()
        }
      }
    )
  }

  /// substring after(string, match) returns all the string that comes after the match, or an empty string if no match.
  pub fn substring_after<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    Builtins::string_match(
      parameters, 
      _contexts, 
      "substring after", 
      |search_string, match_string| {
        match search_string.find(match_string.as_str()) {
          Some(position) => search_string[position + match_string.len() ..].to_string().into(),
          None => "".into()
        }
      }
    )
  }

  // replace(input, pattern, replacement, flags?) Regular expression pattern matching and replacement with optional flags.
  /// The flags are optional. If present, the string may be empty or contain
  /// any or all of the letters i, s, m or x.
  ///   i ... Case insensitive search. 
  ///   s ... Enables single-line mode. Dot matches newlines.
  ///   m ... Enables Multiline-mode. Caret and dollar match before and after newlines. 
  ///   x ... Enables Free-spacing mode. Ignore whitespace between regex tokens (for readability). 
  /// If the flags are invalid or the regex is invalid, a FeelValue::Null is returned. 
  /// If no match is found, the input is returned unchanged. 
  /// If one or more matches are found, only the first mathch is replaced by the replacement and the resulting string returned.
  /// The replacement string may contain backreferences to matched substrings as $1, $2, etc. 
  /// Such backreferences will be replaced by the corresponding capture group.
  /// 
  /// Note: The DMN Version 1.3 Spec does not say whether one match or all matches should be replaced,
  ///       but the TCK tests have an example that indicates all occurrances should be replaced. 
  pub fn replace<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "replace";
    match Builtins::make_validator(fname, parameters)
      .arity(3..5)
      .expect_type(0_usize, FeelType::String, false) // input
      .expect_type(1_usize, FeelType::String, false) // pattern
      .expect_type(2_usize, FeelType::String, false) // replacement
      .expect_type(3_usize, FeelType::String, true)  // flags (Optional)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        let b = &arguments[1];
        let c = &arguments[2];
        let d = &arguments[3];
        match (a, b, c, d) {
          (FeelValue::String(input), FeelValue::String(pattern), FeelValue::String(replacement), FeelValue::Null) => {
            match Regex::new(&pattern) {
              Ok(regex) => {
                regex.replace_all(&input, replacement).into_owned().into()
              },
              Err(err) => {
                ExecutionLog::log(&format!("{:?} called with invalid pattern {:?}. Regex error: {:?}.", fname, pattern, err));
                FeelValue::Null
              }
            }
          },
          (FeelValue::String(input), FeelValue::String(pattern), FeelValue::String(replacement), FeelValue::String(flags)) => {
            if ! Builtins::are_flags_valid(flags) {
              ExecutionLog::log(&format!("{:?} called with invalid flags {:?}. Only i, s, m and x supported.", fname, flags));
              return FeelValue::Null;
            }
            let flagged_pattern = if flags.len() == 0 { format!("{}", pattern) } else { format!("(?{}){}", flags, pattern) };
            match Regex::new(&flagged_pattern) {
              Ok(regex) => {
                regex.replace_all(&input, replacement).into_owned().into()
              },
              Err(err) => {
                ExecutionLog::log(&format!("{:?} called with invalid pattern {:?}. Regex error: {:?}.", fname, pattern, err));
                FeelValue::Null
              }
            }
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }


  /// contains(string, match) Does the string contain the match?
  pub fn contains<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    Builtins::string_match(
      parameters, 
      _contexts, 
      "contains", 
      |search_string, match_string| {
        match search_string.find(match_string.as_str()) {
          Some(_) => true.into(),
          None => false.into()
        }
      }
    )
  }
  
  /// starts with(string, match) Does the string start with the match?
  pub fn starts_with<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    Builtins::string_match(
      parameters, 
      _contexts, 
      "starts with", 
      |search_string, match_string| search_string.starts_with(match_string.as_str()).into()
    )
  }

  /// ends with(string, match) Does the string end with the match?
  pub fn ends_with<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    Builtins::string_match(
      parameters, 
      _contexts, 
      "ends with", 
      |search_string, match_string| search_string.ends_with(match_string.as_str()).into()
    )
  }

  /// matches(input, pattern, flags?) Does the input match the regexp pattern?
  /// The flags are optional. If present, the string may be empty or contain
  /// any or all of the letters i, s, m or x.
  ///   i ... Case insensitive search. 
  ///   s ... Enables single-line mode. Dot matches newlines.
  ///   m ... Enables Multiline-mode. Caret and dollar match before and after newlines. 
  ///   x ... Enables Free-spacing mode. Ignore whitespace between regex tokens (for readability). 
  /// If the flags are invalid or the regex is invalid, a FeelValue::Null is returned. 
  /// If no match is found, FeelValue::Boolean(false) is returned. 
  /// If a match is found, FeelValue::Boolean(true) is returned. 
  pub fn matches<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "matches";
    match Builtins::make_validator(fname, parameters)
      .arity(2..4)
      .expect_type(0_usize, FeelType::String, false)
      .expect_type(1_usize, FeelType::String, false)
      .expect_type(2_usize, FeelType::String, true) // flags are Optional
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        let b = &arguments[1];
        let c = &arguments[2];
        match (a, b, c) {
          (FeelValue::String(input), FeelValue::String(pattern), FeelValue::Null) => {
            match Regex::new(&pattern) {
              Ok(regex) => {
                FeelValue::Boolean(regex.is_match(&input))
              },
              Err(err) => {
                ExecutionLog::log(&format!("{:?} called with invalid pattern {:?}. Regex error: {:?}.", fname, pattern, err));
                FeelValue::Null
              }
            }
          },
          (FeelValue::String(input), FeelValue::String(pattern), FeelValue::String(flags)) => {
            if ! Builtins::are_flags_valid(flags) {
              ExecutionLog::log(&format!("{:?} called with invalid flags {:?}. Only i, s, m and x supported.", fname, flags));
              return FeelValue::Null;
            }
            let flagged_pattern = if flags.len() == 0 { format!("{}", pattern) } else { format!("(?{}){}", flags, pattern) };
            match Regex::new(&flagged_pattern) {
              Ok(regex) => {
                FeelValue::Boolean(regex.is_match(&input))
              },
              Err(err) => {
                ExecutionLog::log(&format!("{:?} called with invalid pattern {:?}. Regex error: {:?}.", fname, pattern, err));
                FeelValue::Null
              }
            }
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }
  
  /// split(string, delimiter) splits the string into a list of substrings, breaking at each occurrence of the delimiter pattern,
  /// which may be a regex.
  pub fn split<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    Builtins::string_match(
      parameters, 
      _contexts, 
      "split", 
      |search_string, delimiter| {
        // delimiter may be a regex. If creating a regex from the string fails, then assume it is a regular string delimiter. 
        match Regex::new(delimiter) {
          Ok(regex) => {
            let pieces: Vec<FeelValue> = regex.split(search_string).map(|piece| FeelValue::String(piece.into())).collect();
            FeelValue::new_list(pieces)
          },
          Err(_) => {
            let pieces: Vec<FeelValue> = search_string.split(delimiter.as_str()).map(|piece| FeelValue::String(piece.into())).collect();
            FeelValue::new_list(pieces)
          }
        }
      }
    )
  }

  //// ////////////////////////////////////////////////
  ////                                             ////
  ////               List functions                ////
  ////                                             ////
  //// ////////////////////////////////////////////////

  /// Handles common validation and extraction of relevant data for 
  /// functions expecting a single FeelValue::List as argument 
  /// and are not expected to handle varargs as an implicit list.
  fn list_helper<C: ContextReader, F: FnOnce(&Vec<FeelValue>) -> FeelValue>(parameters: FeelValue, _contexts: &C, fname: &str, xform: F) -> FeelValue {
    match Builtins::make_list_validator(fname, parameters)
      .arity(1..2)
      .expect_type(0_usize, FeelType::List, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::List(rr_list) => {
            xform(&rr_list.borrow())
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// Handles common validation and extraction of relevant data for 
  /// functions expecting EITHER a single FeelValue::List as argument 
  /// OR a variable number of arguments in place of the list. 
  /// All elements of the list (if a single argument) or all the arguments (if two or more)
  /// must be of the same type with an option to be Null.
  fn list_or_varargs_helper<C: ContextReader, F: FnOnce(&Vec<FeelValue>) -> FeelValue>(
      parameters: FeelValue, _contexts: &C, fname: &str, expected_type: FeelType, allow_nulls: bool, xform: F) -> FeelValue {
    match Builtins::make_validator(fname, parameters)
      .arity(1..10000)
      .expect_type(0_usize, expected_type, allow_nulls)
      .expect_uniform_list()
      .validated() {
      Ok(arguments) => xform(&arguments.flat_args()),
      Err(_) => FeelValue::Null
    }
  }

  fn heterogeneous_list_or_varargs_helper<C: ContextReader, F: FnOnce(&Vec<FeelValue>) -> FeelValue>(
    parameters: FeelValue, _contexts: &C, fname: &str, xform: F) -> FeelValue {
    match Builtins::make_validator(fname, parameters)
      .arity(0..10000)
      .validated() {
      Ok(arguments) => xform(&arguments.flat_args()),
      Err(_) => FeelValue::Null
    }
  }

  /// list contains(list, element): Does the list contain the element? Can even find nulls.
  pub fn list_contains<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "list contains";
    match Builtins::make_validator(fname, parameters)
      .arity(2..3)
      .expect_type(0_usize, FeelType::List, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        let b = &arguments[1];
        match (a, b) {
          (FeelValue::List(rr_list), search_item) => {
            let vec = rr_list.borrow();
            FeelValue::Boolean(vec.iter().any(|item| item == search_item))
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// count(list)**: return size of list, or zero if list is empty
  pub fn count<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    Builtins::list_helper(parameters, contexts, "count", 
      |list| (list.len() as f64).into()
    )
  }

  /// min(list or varargs)
  pub fn min<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    Builtins::list_or_varargs_helper(parameters, contexts, "min", FeelType::Any, false,
      |list| {
        match list.iter().min() {
          Some(min) => min.clone(),
          None => FeelValue::Null 
        }
      }
    )
  }

  /// max(list or varargs) obtains the maximum value (according to sort order) of a list of items
  /// assumed to be of the same type. If types vary, null is returned. 
  pub fn max<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    Builtins::list_or_varargs_helper(parameters, contexts, "max", FeelType::Any, false,
      |list| {
        match list.iter().max() {
          Some(max) => max.clone(),
          None => FeelValue::Null
        }
      }
    )
  }

  /// sum(list or varargs) sums a list of numbers but returns null for other types
  pub fn sum<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    // TODO: sum can overflow, which panics. 
    //       When f128 is ready, cast to f128 before summing, then do a TryFrom/TryInto
    //       conversion and log an error on overflow. 
    Builtins::list_or_varargs_helper(parameters, contexts, "sum", FeelType::Number, false,
      |list| {
        let sum: f64 = list.iter().map(|fv| {
          let x: f64 = fv.try_into().unwrap();
          x
        }).sum();
        FeelValue::Number(sum)
      }
    )
  }

  /// mean(list or varargs) computes the mean of a list of numbers but returns FeelValue::Null 
  /// if any other types are in the list.
  pub fn mean<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    // TODO: sum can overflow, which panics. 
    //       When f128 is ready, cast to f128 before summing, then do a TryFrom/TryInto
    //       conversion and log an error on overflow. 
    Builtins::list_or_varargs_helper(parameters, contexts, "mean", FeelType::Number, false,
      |list| {
        // Validation excludes an empty list.
        let count: f64 = list.len() as f64;
        let sum: f64 = list.iter().map(|fv| {
          let x: f64 = fv.try_into().unwrap();
          x
        }).sum();
        FeelValue::Number(sum / count)
      }
    )
  }

  /// all(list or varargs) returns true if list is empty or all values are true, 
  /// false if any are false (even if some are null), 
  /// but null otherwise.
  pub fn all<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    Builtins::heterogeneous_list_or_varargs_helper(parameters, contexts, "all",
      |list| {
        if list.len() == 0 {
          FeelValue::Boolean(true)
        }
        else {
          let mut true_count = 0_usize;
          for item in list.iter() {
            if item.is_false() { return FeelValue::Boolean(false); }
            if item.is_true() { true_count += 1; }
          }
          if true_count == list.len() {
            FeelValue::Boolean(true)
          }
          else {
            FeelValue::Null
          }
        }
      }
    )
  }

  /// any(list or varargs) returns true if at least one value in the list is true (even if some are null),
  /// false if list is empty or all items are false, otherwise null.
  pub fn any<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    Builtins::heterogeneous_list_or_varargs_helper(parameters, contexts, "any",
      |list| {
        if list.len() == 0 {
          FeelValue::Boolean(false)
        }
        else {
          let mut false_count = 0_usize;
          for item in list.iter() {
            if item.is_true() { return FeelValue::Boolean(true); }
            if item.is_false() { false_count += 1; }
          }
          if false_count == list.len() {
            FeelValue::Boolean(false)
          }
          else {
            FeelValue::Null
          }
        }
      }
    )
  }

  fn expect_length_in_range(fname: &str, items_to_take: isize, list_length: isize, start_position_one_based: isize) -> bool {
    if items_to_take < 1 {
      ExecutionLog::log(&format!("{:?} cannot act upon a negative number of items from a list.", fname));
      false
    }
    else if items_to_take > list_length - start_position_one_based + 1 {
      ExecutionLog::log(&format!("{:?} cannot act upon more items than list contains.", fname));
      false
    }
    else {
      true
    }
  }

  /// sublist(list, start position, length?) copies out a portion of the list, 
  /// starting from the given one-based position, which if negative is relative to the end of the list.
  /// If length is omitted, copy all items to the end of the list, otherwise the
  /// given number of items. 
  pub fn sublist<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "sublist";
    match Builtins::make_validator(fname, parameters)
      .arity(2..4)
      .expect_type(0_usize, FeelType::List, false)
      .position_in_range(0_usize, 1_usize)
      .expect_integer(2_usize, true)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        let b = &arguments[1];
        let c = &arguments[2];
        match (a,b,c) {
          (FeelValue::List(rr_list), FeelValue::Number(pos), FeelValue::Null) => {
            // third argument omitted; copy from position to the end of the list
            let mut position_one_based = *pos as isize;
            if position_one_based < 0 {
              position_one_based += 1_isize + rr_list.borrow().len() as isize;
            }
            let position_zero_based = (position_one_based - 1) as usize;
            let contents: Vec<FeelValue> = rr_list.borrow().iter().skip(position_zero_based).cloned().collect();
            FeelValue::new_list(contents)
          },
          (FeelValue::List(rr_list), FeelValue::Number(pos), FeelValue::Number(len)) => {
            // third argument omitted; copy from position to the end of the list
            let mut position_one_based = *pos as isize;
            let full_length = rr_list.borrow().len() as isize;
            let length = *len as isize;
            if position_one_based < 0 {
              position_one_based += 1_isize + full_length;
            }  
            if ! Builtins::expect_length_in_range(fname, length, full_length, position_one_based) {
              FeelValue::Null
            }
            else {
              let position_zero_based = (position_one_based - 1) as usize;
              let contents: Vec<FeelValue> = rr_list.borrow()
                .iter().skip(position_zero_based).take(length as usize).cloned().collect();
              FeelValue::new_list(contents)
            }
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// append(list, item...): Append one or more items to the list, returning a new list.
  pub fn append<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "append";
    match Builtins::make_validator(fname, parameters)
      .arity(2..10000)
      .expect_type(0_usize, FeelType::List, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::List(rr_list) => {
            // Chain together the values from the first argument (a list)
            // with all the other arguments, to form a longer list.
            let contents: Vec<FeelValue> = 
              rr_list.borrow().iter().cloned()
              .chain(arguments.args.iter().skip(1).cloned()).collect();
            FeelValue::new_list(contents)
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// concatenate(list...): Concatenate one or more lists to form a new list.
  pub fn concatenate<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "concatenate";
    match Builtins::make_validator(fname, parameters)
      .arity(1..10000)
      .expect_type(0_usize, FeelType::List, false)
      .same_types()
      .validated() {
      Ok(arguments) => {
        let mut contents: Vec<FeelValue> = Vec::new();
        for arg in arguments.args.iter() {
          let mut more = arg.item_or_contents();
          contents.append(&mut more);
        }
        FeelValue::new_list(contents)
      },
      Err(_) => FeelValue::Null
    }
  }

  /// insert before(list, position, newItem) inserts a single item before the item at the given
  /// one-based position, but if the position is negative, it counts from the end of the list. 
  pub fn insert_before<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "insert before";
    match Builtins::make_validator(fname, parameters)
      .arity(3..4)
      .expect_type(0_usize, FeelType::List, false)
      .expect_type(1_usize, FeelType::Number, false)
      .position_in_range(0, 1)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        let b = &arguments[1];
        let c = &arguments[2];
        match (a,b,c) {
          (FeelValue::List(rr_list), FeelValue::Number(pos), item) => {
            let mut list = rr_list.borrow().clone();
            let mut position_one_based = *pos as isize;
            if position_one_based < 0 {
              position_one_based += list.len() as isize;
            }
            let position_zero_based = (position_one_based - 1) as usize;
            list.insert(position_zero_based, item.clone());
            FeelValue::new_list(list)
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  // remove(list, position) removes one item from the list at the given one-based position,
  // or if the position is negative, relative to the end of the list, with -1 being the last item. 
  pub fn remove<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "remove";
    match Builtins::make_validator(fname, parameters)
      .arity(2..3)
      .expect_type(0_usize, FeelType::List, false)
      .expect_type(1_usize, FeelType::Number, false)
      .position_in_range(0, 1)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        let b = &arguments[1];
        match (a,b) {
          (FeelValue::List(rr_list), FeelValue::Number(pos)) => {
            let mut list = rr_list.borrow().clone();
            let mut position_one_based = *pos as isize;
            if position_one_based < 0 {
              position_one_based += 1_isize + list.len() as isize;
            }
            let position_zero_based = (position_one_based - 1) as usize;
            list.remove(position_zero_based);
            FeelValue::new_list(list)
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// reverse(list) creates a new list with the elements in reverse order
  pub fn reverse<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    Builtins::list_helper(parameters, contexts, "reverse", 
      |list| FeelValue::new_list(list.iter().rev().cloned().collect())
    )
  }

  // index of(list, match) returns a list containing every one-based index into the list where match is found.
  // If the match is not found, an empty list is returned. 
  // The match may be null, allowing one to search for nulls. 
  pub fn index_of<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "index of";
    match Builtins::make_validator(fname, parameters)
      .arity(2..3)
      .expect_type(0_usize, FeelType::List, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        let b = &arguments[1];
        match (a,b) {
          (FeelValue::List(rr_list), match_item) => {
            let contents: Vec<FeelValue> = rr_list.borrow()
              .iter()
              .enumerate()
              .filter(|(_,item)| *item == match_item)
              .map(|(i, _)| FeelValue::Number((i + 1) as f64)) // Convert to one-based indexing for result
              .collect();
            FeelValue::new_list(contents)
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }


  /// union(list...): concatenate one or more lists with duplicate removal.
  pub fn union<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "union";
    match Builtins::make_validator(fname, parameters)
      .arity(1..10000)
      .expect_type(0_usize, FeelType::List, false)
      .same_types()
      .validated() {
      Ok(arguments) => {
        let mut list_vecs: Vec<Ref<Vec<FeelValue>>> = Vec::new();
        for arg in arguments.args.iter() {
          match arg {
            FeelValue::List(rr_list) => {
              list_vecs.push(rr_list.borrow());
              ()
            },
            _ => () // Validation already verified it is a List, so really unreachable. 
          };
        }
        let mut deduped: Vec<FeelValue> = Vec::new();
        let mut already_seen_set: HashSet<&FeelValue> = HashSet::new();
        for rr_vec_b in list_vecs.iter() {
          for item in rr_vec_b.iter() {
            if already_seen_set.insert(&item) {
              deduped.push(item.clone());
            }
          }          
        }
        FeelValue::new_list(deduped)
      },
      Err(_) => FeelValue::Null
    }
  }

  /// distinct values(list): Duplicate removal that preserves order.
  pub fn distinct_values<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    Builtins::list_helper(parameters, contexts, "distinct values", 
      |list| { 
        let mut set: HashSet<&FeelValue> = HashSet::with_capacity(list.len());
        let deduped: Vec<FeelValue> = list
          .iter()
          .filter(|item| set.insert(item))
          .cloned()
          .collect();
        FeelValue::new_list(deduped) 
      }
    )
  }

  /// flatten(list): Flatten nested lists.
  pub fn flatten<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    Builtins::list_helper(parameters, contexts, "flatten", 
      |list| { 
        let mut flat_list: Vec<FeelValue> = Vec::new();
        for item in list.iter() {
          item.flatten_into(&mut flat_list);
        }
        FeelValue::new_list(flat_list) 
      }
    )
  }

  // product(list or varargs): Returns the product of the numbers.
  pub fn product<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    let fname = "product";
    Builtins::list_or_varargs_helper(parameters, contexts, fname, FeelType::Number, false,
      |list| {
        let product: f64 = list.iter().map(|fv| {
          let x: f64 = fv.try_into().unwrap();
          x
        }).product();
        if product.is_finite() {
          FeelValue::Number(product)
        }
        else {
          ExecutionLog::log(&format!("{:?} of list of {} numbers overflowed.", fname, list.len()));
          FeelValue::Null
        }
      }
    )
  }


  /// median(list or varargs) computes the median of a list of numbers, which is the average of the
  /// two numbers in the middle if the length of the list is even, or the middle of the list
  /// if its length is odd. If any elements are not numbers, null is returned.
  pub fn median<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    let fname = "median";
    Builtins::list_or_varargs_helper(parameters, contexts, fname, FeelType::Number, false,
      |list| {
        match stats_median(list) {
          MedianIndex::Single(index) => {
            list[index].clone()
          },
          MedianIndex::Dual(low_index, high_index) => {
            match (&list[low_index], &list[high_index]) {
              (FeelValue::Number(low_value), FeelValue::Number(high_value)) => {
                FeelValue::Number((low_value + high_value) / 2.0)
              },
              _ => {
                ExecutionLog::log(&format!("{:?} of list of {} numbers yielded something other than a number.", fname, list.len()));
                FeelValue::Null
              }
            }
          },
          MedianIndex::None => {
            ExecutionLog::log(&format!("{:?} of list of {} numbers failed.", fname, list.len()));
            FeelValue::Null
          }
        }
      }
    )
  }


  // stddev(list or varargs) returns the sample standard deviation, or null
  // if there are fewer than two items in the list.
  pub fn stddev<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    // TODO: sum can overflow, which panics. 
    //       When f128 is ready, cast to f128 before summing, then do a TryFrom/TryInto
    //       conversion and log an error on overflow. 
    let fname = "stddev";
    Builtins::list_or_varargs_helper(parameters, contexts, fname, FeelType::Number, false,
      |list| {
        if list.len() < 2 {
          ExecutionLog::log(&format!("{:?} can not be computed for a list with only {} points.", fname, list.len()));
          return FeelValue::Null;
        }
        let points: Vec<f64> = list.iter().map(|val| f64::try_from(val).unwrap()).collect();
        match sample_standard_deviation(&points) {
          Some(sd) => FeelValue::Number(sd),
          // TODO: Log error here
          None => {
            ExecutionLog::log(&format!("{:?} could not be computed, possibly due to overflow.", fname));
            FeelValue::Null
          }
        }
      }
    )
  }

  /// Checks if there are any arguments in this parameter list. 
  /// Returns false if this is an empty FeelValue::List or a List with a single element,
  /// which itself is an empty List. This covers the two cases: variable argument list
  /// and arguments given in a child list. 
  fn any_arguments(ref_parameters: &FeelValue) -> bool {
    match ref_parameters.list_length() {
      Some(0_usize) => false,
      Some(1_usize) => {
        match ref_parameters {
          FeelValue::List(list) => match list.borrow()[0].list_length() {
            Some(0) => false,
            _ => true
          },
          _ => true
        }
      },
      _ => true
    }
  }

  // mode(list or varargs) returns the most common item or items in a list, or an empty list if there are no items
  // in the supplied list.
  pub fn mode<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    let fname = "mode";
    if !Builtins::any_arguments(&parameters) {
      return FeelValue::new_list(Vec::new());
    }
    Builtins::list_or_varargs_helper(parameters, contexts, fname, FeelType::Number, true,
      |list| {
        FeelValue::new_list(mode_with_ties(list))
      }
    )
  }
  
  //// ////////////////////////////////////////////////
  ////                                             ////
  ////             Numeric functions               ////
  ////                                             ////
  //// ////////////////////////////////////////////////


  /// Helper function for validation of numbers that on failure performs logging and returns a Null.
  /// Rejects NaN and infinity. 
  fn validate_number<S: Into<String>>(n: f64, action: S) -> FeelValue {
    if !n.is_finite() {
      ExecutionLog::log(&format!("{:?} did not yield a finite result, substituting null", action.into()));
      FeelValue::Null
    }
    else {
      FeelValue::Number(n)
    }
  }


  /// decimal(number, places) rounds the number to the desired scale.
  /// It uses the "round to even" rule.
  /// For example, 1.5 rounds to 2.0, but so does 2.5. 
  /// The round to even rule reduces bias when performing many calculations that involve rounded numbers. 
  pub fn decimal<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "decimal";
    match Builtins::make_validator(fname, parameters)
      .arity(2..3)
      .no_nulls()
      .expect_type(0_usize, FeelType::Number, false)
      .expect_integer(1_usize, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        let b = &arguments[1];
        match (a, b) {
          (_, FeelValue::Number(places)) if *places < -127.0_f64 => {
            ExecutionLog::log(&format!("{:?} attempted to round to too many negative places", fname));
            FeelValue::Null
          },    
          (_, FeelValue::Number(places)) if *places > 127.0_f64 => {
            ExecutionLog::log(&format!("{:?} attempted to round to too many places", fname));
            FeelValue::Null
          },
          (FeelValue::Number(number), FeelValue::Number(places)) => {
            let u_places = *places as i8;
            let rounded = round::half_to_even(*number, u_places);
            FeelValue::Number(rounded)
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }


  /// floor(number) returns the largest integer less than or equal to the number.
  pub fn floor<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "floor";
    match Builtins::make_validator(fname, parameters)
      .arity(1..2)
      .no_nulls()
      .expect_type(0_usize, FeelType::Number, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::Number(value) => Builtins::validate_number(value.floor(), format!("{:?}({:?})", fname, value)),
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// ceiling(number) returns the smallest integer greater than or equal to the number.
  pub fn ceiling<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "ceiling";
    match Builtins::make_validator(fname, parameters)
      .arity(1..2)
      .no_nulls()
      .expect_type(0_usize, FeelType::Number, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::Number(value) => Builtins::validate_number(value.ceil(), format!("{:?}({:?})", fname, value)),
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// abs(number) returns the absolute value of a number, a year month duration or a day time duration.
  pub fn abs<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "abs";
    match Builtins::make_validator(fname, parameters)
      .arity(1..2)
      .no_nulls()
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::Number(value) => Builtins::validate_number(value.abs(), format!("{:?}({:?})", fname, value)),
          FeelValue::YearMonthDuration(duration) => {
            if duration.is_negative() { a.negate() }
            else { a.clone() }
          },
          FeelValue::DayTimeDuration(duration) => {
            if duration.is_negative() { a.negate() }
            else { a.clone() }
          },
          _ => {
            ExecutionLog::log(&format!(
              "Called {:?} with {:?} for argument 1, expected a Number or a duration type", 
              fname, a.get_type()
            ));
            FeelValue::Null
          }
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// modulo(dividend,divisor) returns the remainder of the division of dividend by divisor.
  /// If either is negative, take the sign of the divisor.
  /// If the divisor is zero, return null.
  pub fn modulo<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "modulo";
    match Builtins::make_validator(fname, parameters)
      .arity(2..3)
      .no_nulls()
      .expect_type(0_usize, FeelType::Number, false)
      .expect_type(1_usize, FeelType::Number, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        let b = &arguments[1];
        match (a, b) {
          (_, FeelValue::Number(divisor)) if *divisor == 0.0_f64 => {
            ExecutionLog::log(&format!("{:?} attempted division by zero", fname));
            FeelValue::Null
          },
          (FeelValue::Number(dividend), FeelValue::Number(divisor)) => {
            // For negative values, the FEEL semantics for modulo are NOT the same as Rust's % operator! 
            let remainder = *dividend - *divisor * (*dividend / *divisor).floor();
            FeelValue::Number(remainder)
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// sqrt(number) returns the square root.
  /// Return Null on negative numbers.
  pub fn sqrt<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "sqrt";
    match Builtins::make_validator(fname, parameters)
      .arity(1..2)
      .no_nulls()
      .expect_type(0_usize, FeelType::Number, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::Number(value) => {
            if !value.is_finite() {
              ExecutionLog::log(&format!("Input to {}() is not finite, substituting null", fname));
              FeelValue::Null
            }
            else if *value < 0.0_f64 {
              ExecutionLog::log(&format!("Input to {}() is negative, substituting null", fname));
              FeelValue::Null
            }
            else {
              Builtins::validate_number(value.sqrt(), format!("{:?}({:?})", fname, value))
            }
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// log(number) returns the natural log.
  /// Return Null on non-positive numbers.
  pub fn log<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "log";
    match Builtins::make_validator(fname, parameters)
      .arity(1..2)
      .no_nulls()
      .expect_type(0_usize, FeelType::Number, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::Number(value) => {
            if !value.is_finite() {
              ExecutionLog::log(&format!("Input to {}() is not finite, substituting null", fname));
              FeelValue::Null
            }
            else if *value <= 0.0_f64 {
              ExecutionLog::log(&format!("Input to {}() is not positive, substituting null", fname));
              FeelValue::Null
            }
            else {
              Builtins::validate_number(value.ln(), format!("{:?}({:?})", fname, value))
            }
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// exp(number) returns Euler's number e raised to the given power.
  /// Return Null on Overflow.
  pub fn exp<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "exp";
    match Builtins::make_validator(fname, parameters)
      .arity(1..2)
      .no_nulls()
      .expect_type(0_usize, FeelType::Number, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::Number(value) => Builtins::validate_number(value.exp(), format!("{:?}({:?})", fname, value)),
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// even(number) returns true for integers that are even, false for
  /// odd integers or numbers with a fractional component, and Null for
  /// anything that is not a Number. 
  pub fn even<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    match Builtins::make_validator("even", parameters)
      .arity(1..2)
      .no_nulls()
      .expect_type(0_usize, FeelType::Number, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::Number(value) => {
            if value % 2.0 == 0.0 { true.into() }
            else { false.into() }
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// odd(number) returns true for integers that are odd, false for
  /// even integers or numbers with a fractional component, and Null for
  /// anything that is not a Number. 
  pub fn odd<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    match Builtins::make_validator("odd", parameters)
      .arity(1..2)
      .no_nulls()
      .expect_type(0_usize, FeelType::Number, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::Number(value) => {
            if value % 2.0 == 0.0 { false.into() }
            else if value % 1.0 == 0.0 { true.into() }
            else { false.into() }
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  //// ////////////////////////////////////////////////
  ////                                             ////
  ////             Range functions                 ////
  ////                                             ////
  //// ////////////////////////////////////////////////


  // The Range Builtin functions are inspired by HL7 CQL 1.4 (Clinical Query Language).
  // Arguments may be points or Ranges, where a Point (normally a Number or a date type) 
  // may be any FeelType but Null, Range, Context, List or Function.

  /// All the range functions take two parameters in Feel 
  /// (packaged here into a single FeelValue::List), but
  /// the behavior varies based on the types of the parameters, which may be Ranges
  /// or points, where a point is a Number, Date, or one of several other permitted types.
  fn get_range_case(a: FeelType, b: FeelType) -> Option<RangeCase> {
    match(Range::is_suitable_as_point(a), Range::is_suitable_as_point(b), 
          a == FeelType::Range, b == FeelType::Range) {
      (true, true, _, _) => Some(RangeCase::PointPoint),
      (true, false, _, true) => Some(RangeCase::PointRange),
      (false, true, true, _) => Some(RangeCase::RangePoint),
      (false, false, true, true) => Some(RangeCase::RangeRange),
      _ => None
    }
  }


  fn before_helper<C: ContextReader>(function_name: &str, parameters: FeelValue, contexts: &C) -> FeelValue {
    match Builtins::make_validator(function_name, parameters)
      .arity(2..3)
      .no_nulls()
      .point_or_range(true, true, true, true)
      .validated() {
      Ok(arguments) => {
        match Builtins::get_range_case(arguments[0].get_type(), arguments[1].get_type()) {
          Some(RangeCase::PointPoint) => (arguments[0] < arguments[1]).into(),
          Some(RangeCase::PointRange) => {
            match arguments[1].try_range_compare(&arguments[0], contexts) {
              Some(Ordering::Less) => true.into(),
              _ => false.into()
            }
          },
          Some(RangeCase::RangePoint) => {
            match arguments[0].try_range_compare(&arguments[1], contexts) {
              Some(Ordering::Greater) => true.into(),
              _ => false.into()
            }
          },
          Some(RangeCase::RangeRange) => {
            match (&arguments[0], &arguments[1]) {
              (FeelValue::Range(a), FeelValue::Range(b)) => {
                match (a.end_bound(contexts), b.start_bound(contexts)) {
                  (Bound::Unbounded, _) => false.into(),
                  (_, Bound::Unbounded) => false.into(),
                  (Bound::Excluded(a_end), Bound::Excluded(b_start)) => (a_end <= b_start).into(),
                  (Bound::Included(a_end), Bound::Excluded(b_start)) => (a_end <= b_start).into(),
                  (Bound::Excluded(a_end), Bound::Included(b_start)) => (a_end <= b_start).into(),
                  (Bound::Included(a_end), Bound::Included(b_start)) => (a_end < b_start).into()
                }
              },
              _ => unreachable!() 
            }
          },
          None => FeelValue::Null
        }
      },
      Err(_) => FeelValue::Null
    }
  }

  fn reverse_parameters(parameters: FeelValue) -> FeelValue {
    match parameters {
      FeelValue::List(list) => {
        let reversed: Vec<FeelValue> = list.borrow().iter().cloned().rev().collect();
        return FeelValue::new_list(
          reversed
        );
      },
      _ => {
        return parameters;
      }
    };
  }

  /// Compare two bounds to see which is less or greater than the other, 
  /// respecting whether they are a lower bound or an upper bound.
  pub fn compare_bounds<T: Ord>(a: &Bound<T>, a_is_lower_bound: bool, b: &Bound<T>, b_is_lower_bound: bool) -> Ordering {
    match (a, a_is_lower_bound, b, b_is_lower_bound) {
      (Bound::Unbounded, true, Bound::Unbounded, true) => Ordering::Equal,
      (Bound::Unbounded, true, _, _) => Ordering::Less,
      (_, _, Bound::Unbounded, true) => Ordering::Greater,
      (Bound::Unbounded, false, Bound::Unbounded, false) => Ordering::Equal,
      (Bound::Unbounded, false, _, _) => Ordering::Greater,
      (_, _, Bound::Unbounded, false) => Ordering::Less,
      (Bound::Included(a_value), _, Bound::Included(b_value), _) => a_value.cmp(b_value),
      (Bound::Excluded(a_value), _, Bound::Excluded(b_value), _) => a_value.cmp(b_value),
      (Bound::Included(a_value), true, Bound::Excluded(b_value), true) => {
        if a_value <= b_value { Ordering::Less } else { Ordering::Greater }
      },
      (Bound::Included(a_value), false, Bound::Excluded(b_value), false) => {
        if a_value >= b_value { Ordering::Greater } else { Ordering::Less }
      },
      (Bound::Included(a_value), true, Bound::Excluded(b_value), false) => {
        if a_value >= b_value { Ordering::Greater } else { Ordering::Less }
      },
      (Bound::Included(a_value), false, Bound::Excluded(b_value), true) => {
        if a_value <= b_value { Ordering::Less } else { Ordering::Greater }
      },

      (Bound::Excluded(a_value), true, Bound::Included(b_value), true) => {
        if a_value < b_value { Ordering::Less } else { Ordering::Greater }
      },
      (Bound::Excluded(a_value), false, Bound::Included(b_value), false) => {
        if a_value > b_value { Ordering::Greater } else { Ordering::Less }
      },
      (Bound::Excluded(a_value), true, Bound::Included(b_value), false) => {
        if a_value >= b_value { Ordering::Greater } else { Ordering::Less }
      },
      (Bound::Excluded(a_value), false, Bound::Included(b_value), true) => {
        if a_value <= b_value { Ordering::Less } else { Ordering::Greater }
      }
    }
  }

  /// before: Does first argument fall before the second with no overlap?
  pub fn before<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    Builtins::before_helper("before", parameters, contexts)
  }

  /// after: Does first argument fall after the second with no overlap?
  pub fn after<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    // before and after are symmetric; just reverse the argument order
    let parameters_reversed = Builtins::reverse_parameters(parameters);
    return Builtins::before_helper("after", parameters_reversed, contexts);
  }

  fn meets_helper<C: ContextReader>(function_name: &str, parameters: FeelValue, contexts: &C) -> FeelValue {
    match Builtins::make_validator(function_name, parameters)
      .arity(2..3)
      .no_nulls()
      .point_or_range(false, true, false, true)
      .validated() {
      Ok(arguments) => {
        match (&arguments[0], &arguments[1]) {
          (FeelValue::Range(a), FeelValue::Range(b)) => {
            match (a.end_bound(contexts), b.start_bound(contexts)) {
              (Bound::Included(a_end), Bound::Included(b_start)) => (a_end == b_start).into(),
              _ => false.into()
            }
          },
          _ => unreachable!() 
        }
      },
      Err(_) => FeelValue::Null
    }
  }

  /// first range meets the second, with its end included and equaling the start of the next (also included)
  pub fn meets<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    Builtins::meets_helper("meets", parameters, contexts)
  }
  
  /// first range is met by the second, with its start included and equaling the end of the next (also included)
  pub fn met_by<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    // meets and met_by are symmetric; just reverse the argument order
    let parameters_reversed = Builtins::reverse_parameters(parameters);
    Builtins::meets_helper("met by", parameters_reversed, contexts)
  }

  /// check if first range overlaps the seconds
  pub fn overlaps<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    match Builtins::make_validator("overlaps", parameters)
      .arity(2..3)
      .no_nulls()
      .point_or_range(false, true, false, true)
      .validated() {
      Ok(arguments) => {
        let (a, b) = (&arguments[0], &arguments[1]);
        match (a, b) {
          (FeelValue::Range(a_range), FeelValue::Range(b_range)) => {
            // Sorting the ranges into which starts the earliest or latest makes the rest of the 
            // comparisons much simpler.
            let range_cmp = a_range.cmp(b_range);
            if range_cmp == Ordering::Equal {
              return true.into();
            }
            let (low, high) = if range_cmp == Ordering::Less { (a_range, b_range) } else { (b_range, a_range) };

            // Had to work out these cases using a truth table - there are 81 cases! 
            // If there is an edge case with an error, this is the place to look first...
            match (low.start_bound(contexts), high.start_bound(contexts), low.end_bound(contexts), high.end_bound(contexts)) {
              (_, Bound::Unbounded, _, _) => true.into(),
              (Bound::Unbounded, _, Bound::Unbounded, _) => true.into(),
              (_, _, Bound::Unbounded, _) => true.into(),
              (_, _, Bound::Included(earliest_end), Bound::Unbounded) => high.includes(&earliest_end, contexts).into(),
              (_, Bound::Excluded(latest_start), Bound::Excluded(earliest_end), Bound::Unbounded) => (latest_start > earliest_end).into(),
              (_, Bound::Included(latest_start), _, _) => low.includes(&latest_start, contexts).into(),
              (_, _, Bound::Included(earliest_end), Bound::Included(latest_end) ) => {
                (high.includes(&earliest_end, contexts) || earliest_end >= latest_end).into()
              },
              (_, _, Bound::Included(earliest_end), Bound::Excluded(latest_end) ) => {
                (high.includes(&earliest_end, contexts) || earliest_end >= latest_end).into()
              },
              (_, Bound::Excluded(latest_start), Bound::Excluded(earliest_end), Bound::Included(_)) => (latest_start < earliest_end).into(),
              (_, Bound::Excluded(latest_start), Bound::Excluded(earliest_end), Bound::Excluded(_)) => (latest_start < earliest_end).into()
            }
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  /// Checks if first range "overlaps before" the second, meaning that the first range must start before
  /// the second and may not go past the end of the second.
  pub fn overlaps_before<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    match Builtins::make_validator("overlaps before", parameters)
      .arity(2..3)
      .no_nulls()
      .point_or_range(false, true, false, true)
      .validated() {
      Ok(arguments) => {
        let (a, b) = (&arguments[0], &arguments[1]);
        match (a, b) {
          (FeelValue::Range(a_range), FeelValue::Range(b_range)) => {
            let a_start = a_range.start_bound(contexts);
            let b_start = b_range.start_bound(contexts);
            let a_end = a_range.end_bound(contexts);
            let b_end = b_range.end_bound(contexts);
            // We must have a_start < b_start <= a_end <= b_end  
            if Builtins::compare_bounds(&a_start, true, &b_start, true) != Ordering::Less { return false.into(); }
            if Builtins::compare_bounds(&b_start, true, &a_end, false) == Ordering::Greater { return false.into(); }
            if Builtins::compare_bounds(&a_end, false, &b_end, false) == Ordering::Greater { return false.into(); }
            true.into()
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  // overlaps_after

  /// Checks if first range "overlaps after" the second, meaning that the first range must end after
  /// the second and may not go before the start of the second.
  pub fn overlaps_after<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    match Builtins::make_validator("overlaps after", parameters)
      .arity(2..3)
      .no_nulls()
      .point_or_range(false, true, false, true)
      .validated() {
      Ok(arguments) => {
        let (a, b) = (&arguments[0], &arguments[1]);
        match (a, b) {
          (FeelValue::Range(a_range), FeelValue::Range(b_range)) => {
            let a_start = a_range.start_bound(contexts);
            let b_start = b_range.start_bound(contexts);
            let a_end = a_range.end_bound(contexts);
            let b_end = b_range.end_bound(contexts);
            // We must have a_end > b_end >= a_start >= b_start 
            if Builtins::compare_bounds(&a_end, false, &b_end, false) != Ordering::Greater { return false.into(); }
            if Builtins::compare_bounds(&b_end, false, &a_start, true) == Ordering::Less { return false.into(); }
            if Builtins::compare_bounds(&a_start, true, &b_start, true) == Ordering::Less { return false.into(); }
            true.into()
          },
          _ => unreachable!()
        }        
      },
      Err(_) => FeelValue::Null
    }
  }

  fn finishes_helper<C: ContextReader>(function_name: &str, parameters: FeelValue, contexts: &C) -> FeelValue {
    match Builtins::make_validator(function_name, parameters)
      .arity(2..3)
      .no_nulls()
      .point_or_range(true, true, false, true)
      .validated() {
      Ok(arguments) => {
        match (&arguments[0], &arguments[1]) {
          // Range-Range case
          (FeelValue::Range(a), FeelValue::Range(b)) => {
            let end_match = match (a.end_bound(contexts), b.end_bound(contexts)) {
              (Bound::Included(a_end), Bound::Included(b_end)) => a_end == b_end,
              (Bound::Excluded(a_end), Bound::Excluded(b_end)) => a_end == b_end,
              _ => false
            };
            if !end_match { return false.into(); }
            let start_match = match (a.start_bound(contexts), b.start_bound(contexts)) {
              (Bound::Included(a_start), Bound::Included(b_start)) => a_start >= b_start,
              (Bound::Excluded(a_start), Bound::Excluded(b_start)) => a_start >= b_start,
              (Bound::Included(a_start), Bound::Excluded(b_start)) => a_start > b_start,
              (Bound::Excluded(a_start), Bound::Included(b_start)) => a_start >= b_start,
              (Bound::Unbounded, _) => false,
              (_, Bound::Unbounded) => true
            };
            start_match.into()
          },
          // Point-Range Case
          (a, FeelValue::Range(b)) => {
            match b.end_bound(contexts) {
              Bound::Included(b_end) => (*a == b_end).into(),
              _ => false.into()
            }
          },
          _ => unreachable!() 
        }
      },
      Err(_) => FeelValue::Null
    }
  }

  /// One point or range finishes the other, being a subset which matches the 
  /// end bound of the second argument, which must be a range.
  pub fn finishes<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    Builtins::finishes_helper("finishes", parameters, contexts)
  }

  /// finished_by is symmetric with finishes; just swap parameters.
  pub fn finished_by<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    let parameters_reversed = Builtins::reverse_parameters(parameters);
    Builtins::finishes_helper("finished by", parameters_reversed, contexts)
  }

  fn includes_helper<C: ContextReader>(function_name: &str, parameters: FeelValue, contexts: &C) -> FeelValue {
    match Builtins::make_validator(function_name, parameters)
      .arity(2..3)
      .no_nulls()
      .point_or_range(false, true, true, true)
      .validated() {
      Ok(arguments) => {
        match (&arguments[0], &arguments[1]) {
          // Range-Range case
          (FeelValue::Range(a), FeelValue::Range(b)) => {
            let includes_start = match b.start_bound(contexts) {
              Bound::Included(b_start) => a.includes(&b_start, contexts),
              Bound::Excluded(b_start) => {
                match a.start_bound(contexts) {
                  Bound::Included(a_start) => a_start <= b_start,
                  Bound::Excluded(a_start) => a_start <= b_start,
                  Bound::Unbounded => true
                }
              },
              // Undefined behavior. 
              // If both ranges have no lower bound, the spec is unclear. 
              // We return false.
              Bound::Unbounded => false
            };
            if !includes_start { return false.into() }

            let includes_end = match b.end_bound(contexts) {
              Bound::Included(b_end) => a.includes(&b_end, contexts),
              Bound::Excluded(b_end) => {
                match a.end_bound(contexts) {
                  Bound::Included(a_end) => a_end >= b_end,
                  Bound::Excluded(a_end) => a_end >= b_end,
                  Bound::Unbounded => true
                }
              },
              // Undefined behavior. 
              // If both ranges have no upper bound, the spec is unclear. 
              // We return false.
              Bound::Unbounded => false
            };
            includes_end.into()
          },
          // Range-Point Case
          (FeelValue::Range(a), b) => a.includes(b, contexts).into(),
          _ => unreachable!() 
        }
      },
      Err(_) => FeelValue::Null
    }
  }

  /// The first range includes the second argument (a point or range).
  /// If the second is a range, all items in the second range are included by the first.
  pub fn includes<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    Builtins::includes_helper("includes", parameters, contexts)
  }

  /// First argument (a point or range) falls during the second range 
  /// (is completely included by it).
  /// This is symmetric with includes if you swap arguments.
  pub fn during<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    let parameters_reversed = Builtins::reverse_parameters(parameters);
    Builtins::includes_helper("during", parameters_reversed, contexts)
  }

  fn starts_helper<C: ContextReader>(function_name: &str, parameters: FeelValue, contexts: &C) -> FeelValue {
    match Builtins::make_validator(function_name, parameters)
      .arity(2..3)
      .no_nulls()
      .point_or_range(true, true, false, true)
      .validated() {
      Ok(arguments) => {
        match (&arguments[0], &arguments[1]) {
          // Range-Range case
          (FeelValue::Range(a), FeelValue::Range(b)) => {
            let start_match = match (a.start_bound(contexts), b.start_bound(contexts)) {
              (Bound::Included(a_start), Bound::Included(b_start)) => a_start == b_start,
              (Bound::Excluded(a_start), Bound::Excluded(b_start)) => a_start == b_start,
              _ => false
            };
            if !start_match { return false.into(); }
            let end_match = match (a.end_bound(contexts), b.end_bound(contexts)) {
              (Bound::Included(a_end), Bound::Included(b_end)) => a_end <= b_end,
              (Bound::Excluded(a_end), Bound::Excluded(b_end)) => a_end <= b_end,
              (Bound::Included(a_end), Bound::Excluded(b_end)) => a_end < b_end,
              (Bound::Excluded(a_end), Bound::Included(b_end)) => a_end <= b_end,
              (Bound::Unbounded, _) => false,
              (_, Bound::Unbounded) => true
            };
            end_match.into()
          },
          // Point-Range Case
          (a, FeelValue::Range(b)) => {
            match b.start_bound(contexts) {
              Bound::Included(b_start) => (*a == b_start).into(),
              _ => false.into()
            }
          },
          _ => unreachable!() 
        }
      },
      Err(_) => FeelValue::Null
    }
  }

  /// One point or range starts the other, being a subset which matches the 
  /// start bound of the second argument, which must be a range.
  pub fn starts<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    Builtins::starts_helper("starts", parameters, contexts)
  }

  /// started_by is symmetric with starts; just swap parameters.
  pub fn started_by<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    let parameters_reversed = Builtins::reverse_parameters(parameters);
    Builtins::starts_helper("started by", parameters_reversed, contexts)
  }

  /// The ranges coincide, hence are equal.
  pub fn coincides<C: ContextReader>(parameters: FeelValue, contexts: &C) -> FeelValue {
    match Builtins::make_validator("coincides", parameters)
      .arity(2..3)
      .no_nulls()
      .point_or_range(true, true, true, true)
      .same_types() // Only permit point-point or range-range
      .validated() {
      Ok(arguments) => {
        match (&arguments[0], &arguments[1]) {
          (FeelValue::Range(a), FeelValue::Range(b)) => {
            // We could use the equals operator except for the case when one or both
            // of the ranges refers to a context value for its bounds.
            (    a.start_bound(contexts) == b.start_bound(contexts)
              && a.end_bound(contexts)   == b.end_bound(contexts)).into()
          },
          _ => {
            (arguments[0] == arguments[1]).into()
          } 
        }
      },
      Err(_) => FeelValue::Null
    }
  }

  //// ///////////// END Range functions /////////////////

  //// ////////////////////////////////////////////////
  ////                                             ////
  ////               Context functions             ////
  ////                                             ////
  //// ////////////////////////////////////////////////

  /// Get value from a Context by key.
  /// For the key, a String or Name will be accepted.
  pub fn get_value<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "get value";
    match Builtins::make_validator(fname, parameters)
      .arity(2..3)
      .expect_type(0_usize, FeelType::Context, false)
      .expect_key(1_usize)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        let b = &arguments[1];
        let optional_result = match (a, b) {
          (FeelValue::Context(arg_ctx), FeelValue::String(key_as_string)) => {
            arg_ctx.get(key_as_string.clone())
          },
          (FeelValue::Context(arg_ctx), FeelValue::Name(key_as_qname)) => {
            arg_ctx.get(key_as_qname.clone())
          },
          _ => unreachable!()
        };
        match optional_result {
          Some(value) => value,
          None => { 
            ExecutionLog::log(&format!("Called {} on a context missing the supplied key {:?}", fname, b));
            FeelValue::Null
          }
        }   
      },
      Err(_) => FeelValue::Null
    }
  }

  /// Convert a Context to a List of Contexts, one per each key-value pair in the 
  /// original Contet. Each resulting context has two keys: "key" and "value".
  pub fn get_entries<C: ContextReader>(parameters: FeelValue, _contexts: &C) -> FeelValue {
    let fname = "get entries";
    match Builtins::make_validator(fname, parameters)
      .arity(1..2)
      .expect_type(0_usize, FeelType::Context, false)
      .validated() {
      Ok(arguments) => {
        let a = &arguments[0];
        match a {
          FeelValue::Context(arg_ctx) => {
            (*arg_ctx).get_entries()
          },
          _ => unreachable!()
        }
      },
      Err(_) => FeelValue::Null
    }
  }

}



/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
  use std::rc::Rc;
  use super::super::feel_value::{FeelValue};
  use super::super::context::{Context};
  use std::ops::{RangeBounds, Bound};
  use std::cmp::Ordering;
  use std::str::FromStr;
  use super::super::range::Range;
  use super::Builtins;
  use super::super::exclusive_inclusive_range::ExclusiveInclusiveRange;
  use super::super::exclusive_range::ExclusiveRange;
  use super::super::duration::Duration;
  use super::super::execution_log::ExecutionLog;

  //// Boolean function tests
  
  /// Tests of floor builtin function from the spec
  #[test]
  fn test_not() {
    let ctx = Context::new();
    assert!(Builtins::not(FeelValue::Boolean(true), &ctx) == FeelValue::Boolean(false));
    assert!(Builtins::not(FeelValue::Boolean(false), &ctx) == FeelValue::Boolean(true));
  }

  //// String function tests
  
  /// Test of builtin substring(string, start position, length?)
  #[test]
  fn test_substring() {
    fn ss(search: &str, start: i32, len_opt: Option<usize>, exp: &str) {
      let ctx = Context::new();
      let f_search: FeelValue = search.into();
      let f_start: FeelValue = start.into();
      let args = match len_opt {
        Some(len) => {
          let f_len = FeelValue::Number(len as f64);
          FeelValue::new_list(vec![f_search, f_start, f_len])
        },
        None => {
          FeelValue::new_list(vec![f_search, f_start])
        }
      };
      let actual = Builtins::substring(args, &ctx);
      let expected = FeelValue::String(exp.into());
      assert!(actual == expected, "substring({:?}, {:?}, {:?}) = {:?} expected, found {:?}", search, start, len_opt, exp, actual);
    }
    ss("foobar", 3, None, "obar");
    ss("foobar", 3, Some(3), "oba");
    ss("foobar", -2, Some(1), "a");
    ss("\u{01F40E}ab", 2, None, "ab"); // Unicode character of three bytes is one single character.
  }

  /// Test of builtin string length(string)
  #[test]
  fn test_string_length() {
    fn sl(s: &str, exp: usize) {
      let ctx = Context::new();
      let f_string: FeelValue = s.into();
      let f_expected: FeelValue = FeelValue::Number(exp as f64);
      let actual = Builtins::string_length(f_string, &ctx);
      assert!(actual == f_expected, "string length({:?}) = {:?} expected, found {:?}", s, exp, actual);
    }
    sl("foo", 3);
    sl("\u{01F40E}ab", 3);
  }

  /// Test of builtin upper case(string)
  #[test]
  fn test_upper_case() {
    fn uc(s: &str, exp: &str) {
      let ctx = Context::new();
      let f_string: FeelValue = s.into();
      let f_expected: FeelValue = exp.into();
      let actual = Builtins::upper_case(f_string, &ctx);
      assert!(actual == f_expected, "upper case({:?}) = {:?} expected, found {:?}", s, exp, actual);
    }
    uc("aBc4", "ABC4");
  }
 
  /// Test of builtin lower case(string)
  #[test]
  fn test_lower_case() {
    fn lc(s: &str, exp: &str) {
      let ctx = Context::new();
      let f_string: FeelValue = s.into();
      let f_expected: FeelValue = exp.into();
      let actual = Builtins::lower_case(f_string, &ctx);
      assert!(actual == f_expected, "lower case({:?}) = {:?} expected, found {:?}", s, exp, actual);
    }
    lc("aBc4", "abc4");
  }
 
  /// Test of builtin substring before(string, match)
  #[test]
  fn test_substring_before() {
    fn sb(s: &str, m: &str, exp: &str) {
      let ctx = Context::new();
      let f_string: FeelValue = s.into();
      let f_match: FeelValue = m.into();
      let args = FeelValue::new_list(vec![f_string, f_match]);
      let f_expected: FeelValue = exp.into();
      let actual = Builtins::substring_before(args, &ctx);
      assert!(actual == f_expected, "substring before({:?}, {:?}) = {:?} expected, found {:?}", s, m, exp, actual);
    }
    sb("foobar", "bar", "foo");
    sb("foobar", "xyz", "");
  }
 
  /// Test of builtin substring after(string, match)
  #[test]
  fn test_substring_after() {
    fn sa(s: &str, m: &str, exp: &str) {
      let ctx = Context::new();
      let f_string: FeelValue = s.into();
      let f_match: FeelValue = m.into();
      let args = FeelValue::new_list(vec![f_string, f_match]);
      let f_expected: FeelValue = exp.into();
      let actual = Builtins::substring_after(args, &ctx);
      assert!(actual == f_expected, "substring after({:?}, {:?}) = {:?} expected, found {:?}", s, m, exp, actual);
    }
    sa("foobar", "ob", "ar");
    sa("", "a", "");
  }
 
  /// Test of builtin replace(input, pattern, replacement, flags?)
  #[test]
  fn test_replace() {
    fn replace(s: &str, m: &str, repl: &str, flags: &str, f_expected: FeelValue) {
      let ctx = Context::new();
      let f_string: FeelValue = s.into();
      let f_match: FeelValue = m.into();
      let f_replacement: FeelValue = repl.into();
      let f_flags: FeelValue = flags.into();
      ExecutionLog::clear();
      let args = FeelValue::new_list(vec![f_string, f_match, f_replacement, f_flags]);
      let actual = Builtins::replace(args, &ctx);
      // ExecutionLog::print("Replace Error: ");
      assert!(actual == f_expected, "replace({:?}, {:?}, {:?}, {:?}) = {:?} expected, found {:?}", s, m, repl, flags, f_expected, actual);
    }
    replace("abcd", "(ab)|(a)", "[1=$1][2=$2]", "", "[1=ab][2=]cd".into());
    replace("abcd", "((ab)|(a)", "[1=$1][2=$2]", "", FeelValue::Null); // Regex syntax error
    replace("bananas", "a", "o", "", "bononos".into()); // Replace multiple occurrances
  }
 
  /// Test of builtin contains(string, match)
  #[test]
  fn test_contains() {
    fn contains(s: &str, m: &str, exp: bool) {
      let ctx = Context::new();
      let f_string: FeelValue = s.into();
      let f_match: FeelValue = m.into();
      let args = FeelValue::new_list(vec![f_string, f_match]);
      let f_expected: FeelValue = exp.into();
      let actual = Builtins::contains(args, &ctx);
      assert!(actual == f_expected, "contains({:?}, {:?}) = {:?} expected, found {:?}", s, m, exp, actual);
    }
    contains("foobar", "of", false);
    contains("foobar", "ob", true);
  }
 
  /// Test of builtin starts with(string, match)
  #[test]
  fn test_starts_with() {
    fn starts_with(s: &str, m: &str, exp: bool) {
      let ctx = Context::new();
      let f_string: FeelValue = s.into();
      let f_match: FeelValue = m.into();
      let args = FeelValue::new_list(vec![f_string, f_match]);
      let f_expected: FeelValue = exp.into();
      let actual = Builtins::starts_with(args, &ctx);
      assert!(actual == f_expected, "starts with({:?}, {:?}) = {:?} expected, found {:?}", s, m, exp, actual);
    }
    starts_with("foobar", "fo", true);
    starts_with("foobar", "oo", false);
  }
 
  /// Test of builtin ends with(string, match)
  #[test]
  fn test_ends_with() {
    fn ends_with(s: &str, m: &str, exp: bool) {
      let ctx = Context::new();
      let f_string: FeelValue = s.into();
      let f_match: FeelValue = m.into();
      let args = FeelValue::new_list(vec![f_string, f_match]);
      let f_expected: FeelValue = exp.into();
      let actual = Builtins::ends_with(args, &ctx);
      assert!(actual == f_expected, "ends with({:?}, {:?}) = {:?} expected, found {:?}", s, m, exp, actual);
    }
    ends_with("foobar", "r", true);
    ends_with("foobar", "ba", false);
  }
 
  /// Test of builtin matches(input, pattern, flags?)
  #[test]
  fn test_matches() {
    fn matches(s: &str, m: &str, flags: &str, f_expected: FeelValue) {
      let ctx = Context::new();
      let f_string: FeelValue = s.into();
      let f_match: FeelValue = m.into();
      let f_flags: FeelValue = flags.into();
      let args = FeelValue::new_list(vec![f_string, f_match, f_flags]);
      let actual = Builtins::matches(args, &ctx);
      assert!(actual == f_expected, "matches({:?}, {:?}, {:?}) = {:?} expected, found {:?}", s, m, flags, f_expected, actual);
    }
    matches("foobar", "^fo*b", "", true.into());
    matches("foobar", "FOO", "", false.into()); // Case sensitive match
    matches("foobar", "FOO", "i", true.into()); // Case insensitive match
    matches("foobar", "^foo", "!", FeelValue::Null); // Bad flag
  }
 
  /// Test of builtin split(string, delimiter)
  #[test]
  fn test_split() {
    fn split(s: &str, delimiter: &str, f_expected: FeelValue) {
      let ctx = Context::new();
      let f_string: FeelValue = s.into();
      let f_delimiter: FeelValue = delimiter.into();
      let args = FeelValue::new_list(vec![f_string, f_delimiter]);
      let actual = Builtins::split(args, &ctx);
      assert!(actual == f_expected, "split({:?}, {:?}) = {:?} expected, found {:?}", s, delimiter, f_expected, actual);
    }
    // Split by Regex pattern
    split("John Doe", "\\s", FeelValue::new_list(vec!["John".into(), "Doe".into()]));
    // Split by string
    split("a;b;c;;", ";", FeelValue::new_list(vec!["a".into(), "b".into(), "c".into(), "".into(), "".into()]));
  }

  //// //////////////////////////////////////////////

  //// List function tests
  
  /// Test of list contains(list, element)
  #[test]
  fn test_list_contains() {
    fn list_contains(list: FeelValue, search_item: FeelValue, f_expected: FeelValue) {
      let ctx = Context::new();
      let args = FeelValue::new_list(vec![list, search_item]);
      let args_string = format!("{:?}", args);
      let actual = Builtins::list_contains(args, &ctx);
      assert!(actual == f_expected, "list contains({:?}) = {:?} expected, found {:?}", args_string, f_expected, actual);
    }
    list_contains(FeelValue::new_from_iterator(vec![1,2,3]), 3.into(), FeelValue::Boolean(true));
    list_contains(FeelValue::new_list(vec![1.into(),FeelValue::Null,3.into()]), FeelValue::Null, FeelValue::Boolean(true));
    list_contains(FeelValue::new_from_iterator(vec![1,2,3]), 4.into(), FeelValue::Boolean(false));
  }

  /// Test of count(list)
  #[test]
  fn test_count() {
    fn count(list: Vec<FeelValue>, exp: usize) {
      let ctx = Context::new();
      let args = FeelValue::new_list(list);
      let f_expected: FeelValue = (exp as f64).into();
      let actual = Builtins::count(args, &ctx);
      assert!(actual == f_expected, "count(list) = {:?} expected, found {:?}", f_expected, actual);
    }
    count(vec![1.into(),2.into(),3.into()], 3);
    count(vec![], 0);
    count(vec![1.into(), FeelValue::new_list(vec![2.into(),3.into()])], 2);
  }

  /// Test of min(list or varargs)
  #[test]
  fn test_min() {
    fn min(args: FeelValue, f_expected: FeelValue) {
      let ctx = Context::new();
      let actual = Builtins::min(args, &ctx);
      assert!(actual == f_expected, "min(list) = {:?} expected, found {:?}", f_expected, actual);
    }
    min(FeelValue::new_list_of_list(vec![1.into(),2.into(),3.into()]), 1.into()); // min([1,2,3]) = 1
    min(FeelValue::new_from_iterator(vec![1,2,3]), 1.into()); // min(1,2,3) = 1
    min(FeelValue::new_list_of_list(vec![1.into()]), 1.into()); // min([1]) = 1
    min(FeelValue::new_from_iterator(vec![1]), 1.into()); // min(1) = 1
    min(FeelValue::new_list_of_list(vec![]), FeelValue::Null); // min([]) = null
  }

  /// Test of max(list or varargs)
  #[test]
  fn test_max() {
    fn max(args: FeelValue, f_expected: FeelValue) {
      let ctx = Context::new();
      let actual = Builtins::max(args, &ctx);
      assert!(actual == f_expected, "max(list) = {:?} expected, found {:?}", f_expected, actual);
    }
    max(FeelValue::new_list_of_list(vec![1.into(),2.into(),3.into()]), 3.into()); // max([1,2,3]) = 1
    max(FeelValue::new_from_iterator(vec![1,2,3]), 3.into()); // max(1,2,3) = 1
    max(FeelValue::new_list_of_list(vec![1.into()]), 1.into()); // max([1]) = 1
    max(FeelValue::new_from_iterator(vec![1]), 1.into()); // max(1) = 1
    max(FeelValue::new_list_of_list(vec![]), FeelValue::Null); // max([]) = null
  }

  /// Test of sum(list or varargs)
  #[test]
  fn test_sum() {
    fn sum(args: FeelValue, f_expected: FeelValue) {
      let ctx = Context::new();
      let args_string = format!("{:?}", args);
      let actual = Builtins::sum(args, &ctx);
      assert!(actual == f_expected, "sum({:?}) = {:?} expected, found {:?}", args_string, f_expected, actual);
    }
    sum(FeelValue::new_list_of_list(vec![1.into(),2.into(),3.into()]), 6.into()); 
    sum(FeelValue::new_from_iterator(vec![1,2,3]), 6.into());
    sum(FeelValue::new_from_iterator(vec![1]), 1.into());
    sum(FeelValue::new_list_of_list(vec![]), FeelValue::Null); 
  }

  /// Test of mean(list or varargs)
  #[test]
  fn test_mean() {
    fn mean(args: FeelValue, f_expected: FeelValue) {
      let ctx = Context::new();
      let args_string = format!("{:?}", args);
      let actual = Builtins::mean(args, &ctx);
      assert!(actual == f_expected, "mean({:?}) = {:?} expected, found {:?}", args_string, f_expected, actual);
    }
    mean(FeelValue::new_list_of_list(vec![1.into(),2.into(),3.into()]), 2.into()); 
    mean(FeelValue::new_from_iterator(vec![1,2,3]), 2.into());
    mean(FeelValue::new_from_iterator(vec![1]), 1.into());
    mean(FeelValue::new_list_of_list(vec![]), FeelValue::Null); 
  }

  /// Test of all(list or varargs)
  #[test]
  fn test_all() {
    fn all(args: FeelValue, f_expected: FeelValue) {
      let ctx = Context::new();
      let actual = Builtins::all(args, &ctx);
      // ExecutionLog::print("all Error: ");
      assert!(actual == f_expected, "all(list) = {:?} expected, found {:?}", f_expected, actual);
    }
    // all([false,null,true]) = false
    all(
      FeelValue::new_list_of_list(vec![false.into(), FeelValue::Null, true.into()]), 
      false.into()
    );
    // all(true) = true
    all(
      FeelValue::new_list(vec![true.into()]), 
      true.into()
    );
    // all([true]) = true
    all(
      FeelValue::new_list_of_list(vec![true.into()]), 
      true.into()
    );
    // all([]) = true
    all(
      FeelValue::new_list_of_list(Vec::new()), 
      true.into()
    );
    // all(0) = null
    all(
      FeelValue::new_list_of_list(vec![0.0.into()]), 
      FeelValue::Null
    ); 
  }

  /// Test of any(list or varargs)
  #[test]
  fn test_any() {
    fn any(args: FeelValue, f_expected: FeelValue) {
      let ctx = Context::new();
      let actual = Builtins::any(args, &ctx);
      // ExecutionLog::print("all Error: ");
      assert!(actual == f_expected, "any(list) = {:?} expected, found {:?}", f_expected, actual);
    }
    // any([false,null,true]) = true
    any(
      FeelValue::new_list_of_list(vec![false.into(), FeelValue::Null, true.into()]), 
      true.into()
    );
    // any(false) = false
    any(
      FeelValue::new_list(vec![false.into()]), 
      false.into()
    );
    // any([false]) = false
    any(
      FeelValue::new_list_of_list(vec![false.into()]), 
      false.into()
    );
    // any([]) = false
    any(
      FeelValue::new_list_of_list(Vec::new()), 
      false.into()
    );
    // any(0) = null
    any(
      FeelValue::new_list_of_list(vec![0.0.into()]), 
      FeelValue::Null
    ); 
  }

  // Test of sublist(list, start position, length?)
  #[test]
  fn test_sublist() {
    fn sublist(list: FeelValue, position: i32, length: FeelValue, f_expected: FeelValue) {
      let ctx = Context::new();
      let args = FeelValue::new_list(vec![list, position.into(), length]);
      let args_string = format!("{:?}", args);
      let actual = Builtins::sublist(args, &ctx);
      assert!(actual == f_expected, "sublist({:?}) = {:?} expected, found {:?}", args_string, f_expected, actual);
    }
    // sublist([4,5,6], 1, 2) = [4,5]
    sublist(FeelValue::new_from_iterator(vec![4,5,6]), 1, 2.into(), FeelValue::new_from_iterator(vec![4,5]));  

    // sublist([4,5,6], 2) = [5,6]
    sublist(FeelValue::new_from_iterator(vec![4,5,6]), 2, FeelValue::Null, FeelValue::new_from_iterator(vec![5,6])); 

    // sublist([1,2,3,4,5], -3, 2) = [3,4]
    sublist(FeelValue::new_from_iterator(vec![1,2,3,4,5]), -3, 2.into(), FeelValue::new_from_iterator(vec![3,4]));  
  }

  /// Test of append(list, item...)
  #[test]
  fn test_append() {
    fn append(list: FeelValue, items: &mut Vec<FeelValue>, f_expected: FeelValue) {
      let ctx = Context::new();
      let mut arg_vec: Vec<FeelValue> = Vec::new();
      arg_vec.push(list);
      arg_vec.append(items);
      let args = FeelValue::new_list(arg_vec);
      let args_string = format!("{:?}", args);
      let actual = Builtins::append(args, &ctx);
      assert!(actual == f_expected, "append({:?}) = {:?} expected, found {:?}", args_string, f_expected, actual);
    }
    // append([1,2], 3) = [1,2,3]
    append(
      FeelValue::new_from_iterator(vec![1,2]), 
      &mut vec![3.into()],
      FeelValue::new_list(vec![1.into(), 2.into(), 3.into()])
    );  
    // append([1,2,3], 3, null, true) = [1,2,3,3,null,true] 
    append(
      FeelValue::new_from_iterator(vec![1,2,3]), 
      &mut vec![3.into(),FeelValue::Null,true.into()],
      FeelValue::new_list(vec![1.into(), 2.into(), 3.into(), 3.into(), FeelValue::Null, true.into()])
    );
  }


  /// Test of concatenate(list...)
  #[test]
  fn test_concatenate() {
    fn concatenate(list: FeelValue, items: &mut Vec<FeelValue>, f_expected: FeelValue) {
      let ctx = Context::new();
      let mut arg_vec: Vec<FeelValue> = Vec::new();
      arg_vec.push(list);
      arg_vec.append(items);
      let args = FeelValue::new_list(arg_vec);
      let args_string = format!("{:?}", args);
      let actual = Builtins::concatenate(args, &ctx);
      assert!(actual == f_expected, "concatenate({:?}) = {:?} expected, found {:?}", args_string, f_expected, actual);
    }
    // concatenate([1,2],[3]) = [1,2,3]
    concatenate(
      FeelValue::new_from_iterator(vec![1,2]), 
      &mut vec![FeelValue::new_from_iterator(vec![3])],
      FeelValue::new_from_iterator(vec![1,2,3])
    );  
  }

  /// Test of insert before(list, position, newItem)
  #[test]
  fn test_insert_before() {
    fn insert_before(list: FeelValue, position: i32, item: FeelValue, f_expected: FeelValue) {
      let ctx = Context::new();
      let args = FeelValue::new_list(vec![list, position.into(), item]);
      let args_string = format!("{:?}", args);
      let actual = Builtins::insert_before(args, &ctx);
      assert!(actual == f_expected, "insert before({:?}) = {:?} expected, found {:?}", args_string, f_expected, actual);
    }
    // insert before([1,3], 1, 2) = [2,1,3]
    insert_before(FeelValue::new_from_iterator(vec![1,3]), 1, 2.into(), FeelValue::new_from_iterator(vec![2,1,3]));  

    // insert before([1,2,3,4], -2, 9) = [1,2,9,3,4] (negative position)
    insert_before(
      FeelValue::new_from_iterator(vec![1,2,3,4]), 
      -1,
      9.into(),
      FeelValue::new_from_iterator(vec![1,2,9,3,4])
    ); 

    // insert before([1,3], 3, 2) = null (position too high)
    insert_before(FeelValue::new_from_iterator(vec![1,3]), 3, 2.into(), FeelValue::Null);  
  }

  /// Test of remove(list, position)
  #[test]
  fn test_remove() {
    fn remove(list: FeelValue, position: i32, f_expected: FeelValue) {
      let ctx = Context::new();
      let args = FeelValue::new_list(vec![list, position.into()]);
      let args_string = format!("{:?}", args);
      let actual = Builtins::remove(args, &ctx);
      assert!(actual == f_expected, "remove({:?}) = {:?} expected, found {:?}", args_string, f_expected, actual);
    }
    // remove([1,2,3], 2) = [1,3]
    remove(FeelValue::new_from_iterator(vec![1,2,3]), 2, FeelValue::new_from_iterator(vec![1,3]));  

    // remove([1,2,3,4], -1) = [1,2,3] (negative position)
    remove(
      FeelValue::new_from_iterator(vec![1,2,3,4]), 
      -1,
      FeelValue::new_from_iterator(vec![1,2,3])
    ); 

    // remove([1,2,3], 4) = null (position too high)
    remove(FeelValue::new_from_iterator(vec![1,2,3]), 4, FeelValue::Null);  
  }


  /// Test of reverse(list)
  #[test]
  fn test_reverse() {
    fn reverse(args: FeelValue, f_expected: FeelValue) {
      let ctx = Context::new();
      let actual = Builtins::reverse(args, &ctx);
      assert!(actual == f_expected, "reverse(list) = {:?} expected, found {:?}", f_expected, actual);
    }
    reverse(
      FeelValue::new_from_iterator(vec![1,2,3]), 
      FeelValue::new_from_iterator(vec![3,2,1])
    );
  }

  /// Test of index of(list, match)
  #[test]
  fn test_index_of() {
    fn index_of(list: FeelValue, match_item: FeelValue, f_expected: FeelValue) {
      let ctx = Context::new();
      let args = FeelValue::new_list(vec![list, match_item]);
      let args_string = format!("{:?}", args);
      let actual = Builtins::index_of(args, &ctx);
      assert!(actual == f_expected, "index of({:?}) = {:?} expected, found {:?}", args_string, f_expected, actual);
    }
    // index of([1,2,3,2], 2) = [2,4]
    index_of(
      FeelValue::new_from_iterator(vec![1,2,3,2]), 
      2.into(),
      FeelValue::new_from_iterator(vec![2,4])
    );      
    // index of([1,2,3,null], null) = [4]
    index_of(
      FeelValue::new_list(vec![1.into(),2.into(),3.into(),FeelValue::Null]), 
      FeelValue::Null,
      FeelValue::new_from_iterator(vec![4])
    );  
  }

  /// Test of union(list...)
  #[test]
  fn test_union() {
    fn union(list: FeelValue, items: &mut Vec<FeelValue>, f_expected: FeelValue) {
      let ctx = Context::new();
      let mut arg_vec: Vec<FeelValue> = Vec::new();
      arg_vec.push(list);
      arg_vec.append(items);
      let args = FeelValue::new_list(arg_vec);
      let args_string = format!("{:?}", args);
      let actual = Builtins::union(args, &ctx);
      assert!(actual == f_expected, "union({:?}) = {:?} expected, found {:?}", args_string, f_expected, actual);
    }
    // union([1,2], [2,3]) = [1,2,3]
    union(
      FeelValue::new_from_iterator(vec![1,2]), 
      &mut vec![FeelValue::new_from_iterator(vec![2,3])],
      FeelValue::new_from_iterator(vec![1,2,3])
    );  
    // union([10,9,8,7], [5,6,7], [1,2,3,4,5]) = [10,9,8,7,5,6,1,2,3,4]
    union(
      FeelValue::new_from_iterator(vec![10,9,8,7]), 
      &mut vec![
        FeelValue::new_from_iterator(vec![5,6,7]),
        FeelValue::new_from_iterator(vec![1,2,3,4,5])
      ],
      FeelValue::new_from_iterator(vec![10,9,8,7,5,6,1,2,3,4])
    );  
  }

  /// Test of distinct values(list)
  #[test]
  fn test_distinct_values() {
    fn distinct_values(list: Vec<FeelValue>, exp: Vec<FeelValue>) {
      let ctx = Context::new();
      let args = FeelValue::new_list(list);
      let f_expected = FeelValue::new_list(exp);
      let actual = Builtins::distinct_values(args, &ctx);
      assert!(actual == f_expected, "distinct values(list) = {:?} expected, found {:?}", f_expected, actual);
    }
    distinct_values(
      vec![1.into(),2.into(),3.into(),2.into(),1.into(),], 
      vec![1.into(),2.into(),3.into()]
    );
  }

  /// Test of flatten(list)
  #[test]
  fn test_flatten() {
    fn flatten(args: FeelValue, f_expected: FeelValue) {
      let ctx = Context::new();
      let actual = Builtins::flatten(args, &ctx);
      assert!(actual == f_expected, "flatten(list) = {:?} expected, found {:?}", f_expected, actual);
    }
    // In JSON, the following unflattened object would be [[1,2],[[3]],4]
    let unflattened = FeelValue::new_list(
        vec![
          FeelValue::new_list(vec![1.into(), 2.into()]), 
          FeelValue::new_list(vec![FeelValue::new_list(vec![3.into()])]), 
          4.into()]
    );
    let flattened = FeelValue::new_list(vec![1.into(), 2.into(), 3.into(), 4.into()]);
    flatten(unflattened, flattened);
  }

  /// Test of product(list or varargs)
  #[test]
  fn test_product() {
    fn product(args: FeelValue, f_expected: FeelValue) {
      let ctx = Context::new();
      let args_string = format!("{:?}", args);
      let actual = Builtins::product(args, &ctx);
      assert!(actual == f_expected, "product({:?}) = {:?} expected, found {:?}", args_string, f_expected, actual);
    }
    product(FeelValue::new_list_of_list(vec![2.into(),3.into(),4.into()]), 24.into()); 
    product(FeelValue::new_from_iterator(vec![2,3,4]), 24.into());
    product(FeelValue::new_from_iterator(vec![1]), 1.into());
  }

  /// Test of median(list or varargs)
  #[test]
  fn test_median() {
    fn median(args: FeelValue, f_expected: FeelValue) {
      let ctx = Context::new();
      let args_string = format!("{:?}", args);
      let actual = Builtins::median(args, &ctx);
      assert!(actual == f_expected, "median({:?}) = {:?} expected, found {:?}", args_string, f_expected, actual);
    }
    median(
      FeelValue::new_from_iterator(vec![8,2,5,3,4]), 
      4.into()
    );
    median(
      FeelValue::new_list_of_list(vec![6.into(), 1.into(), 2.into(), 3.into()]),
      2.5.into()
    );
    median(
      FeelValue::new_list(vec![]), 
      FeelValue::Null
    );
  }

  /// Test of stddev(list or varargs)
  #[test]
  fn test_stddev() {
    fn stddev(args: FeelValue, f_expected: FeelValue) {
      let ctx = Context::new();
      let args_string = format!("{:?}", args);
      let actual = Builtins::stddev(args, &ctx);
      assert!(actual == f_expected, "stddev({:?}) = {:?} expected, found {:?}", args_string, f_expected, actual);
    }
    stddev(FeelValue::new_from_iterator(vec![2,4,7,5]), 2.081665999466133.into());
    stddev(FeelValue::new_list_of_list(vec![47.into()]), FeelValue::Null); 
    stddev(FeelValue::new_from_iterator(vec![47]), FeelValue::Null);
  }

  /// Test of mode(list or varargs)
  #[test]
  fn test_mode() {
    fn mode(args: FeelValue, f_expected: FeelValue) {
      let ctx = Context::new();
      let args_string = format!("{:?}", args);
      let actual = Builtins::mode(args, &ctx);
      assert!(actual == f_expected, "mode({:?}) = {:?} expected, found {:?}", args_string, f_expected, actual);
    }
    mode(
      FeelValue::new_from_iterator(vec![6,3,9,6,6]), 
      FeelValue::new_from_iterator(vec![6])
    );
    mode(
      FeelValue::new_from_iterator(vec![6,1,9,6,1]), 
      FeelValue::new_from_iterator(vec![1,6])
    );
    mode(
      FeelValue::new_list(vec![]), 
      FeelValue::new_list(vec![])
    );
  }
  
  //// /////////////////////////////////////////////////

  //// Numeric function tests
  
  /// Test if two Numbers are approximately equal, differing by no more than delta.
  fn are_near(a: &FeelValue, b: &FeelValue, delta: f64) -> bool {
    match (a, b) {
      (FeelValue::Number(x), FeelValue::Number(y)) => x.is_finite() && y.is_finite() && (x - y).abs() <= delta,
      _ => false
    }
  }

  // Tests of decimal builtin function from the spec (plus one more)
  #[test]
  fn test_decimal() {
    fn decimal_case(a: f64, b: f64, expected: f64, case_number: u32) -> () {
      let ctx = Context::new();
      let a_value: FeelValue = a.into();
      let b_value: FeelValue = b.into();
      let exp: FeelValue = expected.into();
      let args = FeelValue::new_list(vec![a_value, b_value]);
      let actual = Builtins::decimal(args, &ctx);
      let message = format!("expected decimal({},{}) = {}, actual = {:?} [case {}]", a, b, expected, actual, case_number);
      assert!(are_near(&actual, &exp, 0.00000000001_f64), "{}", message);
    }
    decimal_case(1.0_f64/3.0_f64, 2.0_f64, 0.33_f64, 1);
    decimal_case(1.5_f64, 0.0_f64, 2.0_f64, 2);
    decimal_case(2.5_f64, 0.0_f64, 2.0_f64, 3);
    decimal_case(4321.0_f64, -2.0_f64, 4300.0_f64, 4);
  }

  /// Tests of floor builtin function from the spec
  #[test]
  fn test_floor() {
    let ctx = Context::new();
    assert!(are_near(&Builtins::floor(1.5.into(), &ctx), &1.0_f64.into(), 0.00000000005_f64), "case 1");
    assert!(are_near(&Builtins::floor((-1.5).into(), &ctx), &(-2.0_f64).into(), 0.00000000005_f64), "case 2");
  }

  /// Tests of ceiling builtin function from the spec
  #[test]
  fn test_ceiling() {
    let ctx = Context::new();
    assert!(are_near(&Builtins::ceiling(1.5.into(), &ctx), &2.0_f64.into(), 0.00000000005_f64), "case 1");
    assert!(are_near(&Builtins::ceiling((-1.5).into(), &ctx), &(-1.0_f64).into(), 0.00000000005_f64), "case 2");
  }

  /// Tests of abs builtin function from the spec
  #[test]
  fn test_abs() {
    let ctx = Context::new();
    assert!(are_near(&Builtins::abs(10.into(), &ctx), &10.0_f64.into(), 0.00000000005_f64), "case 1");
    assert!(are_near(&Builtins::abs((-10.0).into(), &ctx), &10.0_f64.into(), 0.00000000005_f64), "case 2");

    let negative_duration = FeelValue::DayTimeDuration(Duration::from_str("-PT5H").unwrap());
    let positive_duration = FeelValue::DayTimeDuration(Duration::from_str("PT5H").unwrap());

    assert!(positive_duration == Builtins::abs(positive_duration.clone(), &ctx), "case 3");
    assert!(positive_duration == Builtins::abs(negative_duration, &ctx), "case 4");
  }

  // Tests of modulo builtin function from the spec
  #[test]
  fn test_modulo() {
    fn mod_case(a: f64, b: f64, expected: f64, case_number: u32) -> () {
      let ctx = Context::new();
      let a_value: FeelValue = a.into();
      let b_value: FeelValue = b.into();
      let exp: FeelValue = expected.into();
      let args = FeelValue::new_list(vec![a_value, b_value]);
      let actual = Builtins::modulo(args, &ctx);
      let message = format!("expected modulo({},{}) = {}, actual = {:?} [case {}]", a, b, expected, actual, case_number);
      assert!(are_near(&actual, &exp, 0.00000000001_f64), "{}", message);
    }
    mod_case(12.0_f64, 5.0_f64, 2.0_f64, 1);
    mod_case(-12.0_f64, 5.0_f64, 3.0_f64, 2);
    mod_case(12.0_f64, -5.0_f64, -3.0_f64, 3);
    mod_case(-12.0_f64, -5.0_f64, -2.0_f64, 4);
    mod_case(10.1_f64, 4.5_f64, 1.1_f64, 5);
    mod_case(-10.1_f64, 4.5_f64, 3.4_f64, 6);
    mod_case(10.1_f64, -4.5_f64, -3.4_f64, 7);
    mod_case(-10.1_f64, -4.5_f64, -1.1_f64, 8);
  }


  /// Tests of sqrt builtin function from the spec, plus more
  #[test]
  fn test_sqrt() {
    let ctx = Context::new();
    assert!(are_near(&Builtins::sqrt(16.into(), &ctx), &4.0_f64.into(), 0.00000000005_f64), "case 1");
    assert!(are_near(&Builtins::sqrt(1.into(), &ctx), &1.0_f64.into(), 0.000000000005_f64), "case 2");
    assert!(are_near(&Builtins::sqrt(0.into(), &ctx), &0.0_f64.into(), 0.000000000005_f64), "case 3");
    assert!(Builtins::sqrt((-1.0).into(), &ctx).is_null(), "case 4");
  }

  /// Tests of log builtin function from the spec, plus more
  #[test]
  fn test_log() {
    let ctx = Context::new();
    assert!(are_near(&Builtins::log(10.into(), &ctx), &2.30258509299_f64.into(), 0.00000000005_f64), "case 1");
    assert!(are_near(&Builtins::log(std::f64::consts::E.into(), &ctx), &1.0_f64.into(), 0.000000000005_f64), "case 2");
    assert!(are_near(&Builtins::log(1.into(), &ctx), &0.0_f64.into(), 0.000000000005_f64), "case 3");
    assert!(Builtins::log(0.0.into(), &ctx).is_null(), "case 4");
    assert!(Builtins::log((-1.0).into(), &ctx).is_null(), "case 5");
  }
  
  /// Tests of exp builtin function from the spec, plus more
  #[test]
  fn test_exp() {
    let ctx = Context::new();
    assert!(are_near(&Builtins::exp(5.into(), &ctx), &148.413159102577_f64.into(), 0.000000000005_f64), "case 1");
    assert!(are_near(&Builtins::exp(0.into(), &ctx), &1.0_f64.into(), 0.000000000005_f64), "case 2");
    assert!(Builtins::exp(99999999.into(), &ctx).is_null(), "case 3");
  }

  /// Tests of even builtin function from the spec, plus more
  #[test]
  fn test_even() {
    let ctx = Context::new();
    assert!(Builtins::even(5.into(), &ctx).is_false(), "case 1");
    assert!(Builtins::even(2.into(), &ctx).is_true(), "case 2");
    assert!(Builtins::even(8.5.into(), &ctx).is_false(), "case 3");
    assert!(Builtins::even("Bad".into(), &ctx).is_null(), "case 4");

    let list = FeelValue::new_list(vec![8.into()]);
    assert!(Builtins::even(list, &ctx).is_true(), "case 5");
  }

  /// Tests of odd builtin function from the spec, plus more
  #[test]
  fn test_odd() {
    let ctx = Context::new();
    assert!(Builtins::odd(5.into(), &ctx).is_true(), "case 1");
    assert!(Builtins::odd(2.into(), &ctx).is_false(), "case 2");
    assert!(Builtins::odd(8.5.into(), &ctx).is_false(), "case 3");
    assert!(Builtins::odd("Bad".into(), &ctx).is_null(), "case 4");
  }

  //// Helper functions for Range tests

  fn pt_rng<R: RangeBounds<f64>>(a: i32, b: R) -> FeelValue {
    let range: Range = b.into();
    FeelValue::new_list(vec![FeelValue::Number(a as f64), FeelValue::Range(range)])
  }

  fn rng_pt<R: RangeBounds<f64>>(a: R, b: i32) -> FeelValue {
    let range: Range = a.into();
    FeelValue::new_list(vec![FeelValue::Range(range), FeelValue::Number(b as f64)])
  }

  fn pt_pt(a: i32, b: i32) -> FeelValue {
    FeelValue::new_list(vec![FeelValue::Number(a as f64), FeelValue::Number(b as f64)])
  }

  fn rng_rng<R1: RangeBounds<f64>, R2: RangeBounds<f64>>(a: R1, b: R2) -> FeelValue {
    let range_a: Range = a.into();
    let range_b: Range = b.into();
    FeelValue::new_list(vec![FeelValue::Range(range_a), FeelValue::Range(range_b)])
  }

  //// Range function Tests

  /// Test the "before" examples given in Table 78 of the Spec.
  #[test]
  fn test_before() {
    let ctx = Context::new();
    assert!(Builtins::before(pt_pt(1, 10), &ctx).is_true(), "case 1");
    assert!(Builtins::before(pt_pt(10, 1), &ctx).is_false(), "case 2");
    assert!(Builtins::before(pt_rng(1, 1.0..=10.0), &ctx).is_false(), "case 3");
    let r_1_to_10 = ExclusiveInclusiveRange { start: &1.0_f64, end: &10.0_f64 };
    assert!(Builtins::before(pt_rng(1, r_1_to_10), &ctx).is_true(), "case 4"); 
    assert!(Builtins::before(pt_rng(1, 5.0..=10.0), &ctx).is_true(), "case 5");
    assert!(Builtins::before(rng_pt(1.0..=10.0, 10), &ctx).is_false(), "case 6");
    assert!(Builtins::before(rng_pt(1.0..10.0, 10), &ctx).is_true(), "case 7");
    assert!(Builtins::before(rng_pt(1.0..10.0, 15), &ctx).is_true(), "case 8");
    assert!(Builtins::before(rng_rng(1.0..=10.0, 15.0..=20.0), &ctx).is_true(), "case 9");
    assert!(Builtins::before(rng_rng(1.0..=10.0, 10.0..=20.0), &ctx).is_false(), "case 10");
    assert!(Builtins::before(rng_rng(1.0..10.0, 10.0..=20.0), &ctx).is_true(), "case 11");
    let r_10_to_20 = ExclusiveInclusiveRange { start: &10.0_f64, end: &20.0_f64 };
    assert!(Builtins::before(rng_rng(1.0..=10.0, r_10_to_20), &ctx).is_true(), "case 12"); 
  }

  /// Test the "after" examples given in Table 78 of the Spec.
  #[test]
  fn test_after() {
    let ctx = Context::new();
    let r_11_to_20 = ExclusiveInclusiveRange { start: &11.0_f64, end: &20.0_f64 };

    assert!(Builtins::after(pt_pt(10, 5), &ctx).is_true(), "case 1");
    assert!(Builtins::after(pt_pt(5, 10), &ctx).is_false(), "case 2");
    assert!(Builtins::after(pt_rng(12, 1.0..=10.0), &ctx).is_true(), "case 3");
    assert!(Builtins::after(pt_rng(10, 1.0..10.0), &ctx).is_true(), "case 4"); 
    assert!(Builtins::after(pt_rng(10, 1.0..=10.0), &ctx).is_false(), "case 5");
    assert!(Builtins::after(rng_pt(11.0..=20.0, 12), &ctx).is_false(), "case 6");
    assert!(Builtins::after(rng_pt(11.0..=20.0, 10), &ctx).is_true(), "case 7");
    assert!(Builtins::after(rng_pt(r_11_to_20, 11), &ctx).is_true(), "case 8");
    assert!(Builtins::after(rng_pt(11.0..=20.0, 11), &ctx).is_false(), "case 9");
    assert!(Builtins::after(rng_rng(11.0..=20.0, 1.0..=10.0), &ctx).is_true(), "case 10");
    assert!(Builtins::after(rng_rng(1.0..=10.0, 11.0..=20.0), &ctx).is_false(), "case 11");
    assert!(Builtins::after(rng_rng(11.0..=20.0, 1.0..11.0), &ctx).is_true(), "case 12"); 
    assert!(Builtins::after(rng_rng(r_11_to_20, 1.0..=11.0), &ctx).is_true(), "case 13"); 
  }

  /// Test the "meets" examples given in Table 78 of the Spec.
  #[test]
  fn test_meets() {
    let ctx = Context::new();
    let r_5_to_10 = ExclusiveInclusiveRange { start: &5.0_f64, end: &10.0_f64 };

    assert!(Builtins::meets(rng_rng(1.0..=5.0, 5.0..=10.0), &ctx).is_true(), "case 1");
    assert!(Builtins::meets(rng_rng(1.0..5.0, 5.0..=10.0), &ctx).is_false(), "case 2");
    assert!(Builtins::meets(rng_rng(1.0..=5.0, r_5_to_10), &ctx).is_false(), "case 3"); 
    assert!(Builtins::meets(rng_rng(1.0..=5.0, 6.0..=10.0), &ctx).is_false(), "case 4"); 
  }

  /// Test the "met by" examples given in Table 78 of the Spec.
  #[test]
  fn test_met_by() {
    let ctx = Context::new();
    let r_5_to_10 = ExclusiveInclusiveRange { start: &5.0_f64, end: &10.0_f64 };

    assert!(Builtins::met_by(rng_rng(5.0..=10.0, 1.0..=5.0), &ctx).is_true(), "case 1");
    assert!(Builtins::met_by(rng_rng(5.0..=10.0, 1.0..5.0), &ctx).is_false(), "case 2");
    assert!(Builtins::met_by(rng_rng(r_5_to_10, 1.0..=5.0), &ctx).is_false(), "case 3"); 
    assert!(Builtins::met_by(rng_rng(6.0..=10.0, 1.0..=5.0), &ctx).is_false(), "case 4"); 
  }

  /// Test the "overlaps" examples  given in Table 78 of the Spec.
  #[test]
  fn test_overlaps() {
    // TODO: There are 81 different cases possible, but we only test 14.
    // This would be a good place to add more test cases than the spec has.
    let ctx = Context::new();
    let r_5_to_8 = ExclusiveInclusiveRange { start: &5.0_f64, end: &8.0_f64 };

    assert!(Builtins::overlaps(rng_rng(1.0..=5.0, 3.0..=8.0), &ctx).is_true(), "case 1");
    assert!(Builtins::overlaps(rng_rng(3.0..=8.0, 1.0..=5.0), &ctx).is_true(), "case 2");
    assert!(Builtins::overlaps(rng_rng(1.0..=8.0, 3.0..=5.0), &ctx).is_true(), "case 3"); 
    assert!(Builtins::overlaps(rng_rng(3.0..=5.0, 1.0..=8.0), &ctx).is_true(), "case 4"); 
    assert!(Builtins::overlaps(rng_rng(1.0..=5.0, 6.0..=8.0), &ctx).is_false(), "case 5"); 
    assert!(Builtins::overlaps(rng_rng(6.0..=8.0, 1.0..=5.0), &ctx).is_false(), "case 6"); 
    assert!(Builtins::overlaps(rng_rng(1.0..=5.0, 5.0..=8.0), &ctx).is_true(), "case 7"); 
    assert!(Builtins::overlaps(rng_rng(1.0..=5.0, r_5_to_8), &ctx).is_false(), "case 8"); 
    assert!(Builtins::overlaps(rng_rng(1.0..5.0, 5.0..=8.0), &ctx).is_false(), "case 9"); 
    assert!(Builtins::overlaps(rng_rng(1.0..5.0, r_5_to_8), &ctx).is_false(), "case 10"); 
    assert!(Builtins::overlaps(rng_rng(5.0..=8.0, 1.0..=5.0), &ctx).is_true(), "case 11"); 
    assert!(Builtins::overlaps(rng_rng(r_5_to_8, 1.0..=5.0), &ctx).is_false(), "case 12"); 
    assert!(Builtins::overlaps(rng_rng(5.0..=8.0, 1.0..5.0), &ctx).is_false(), "case 13"); 
    assert!(Builtins::overlaps(rng_rng(r_5_to_8, 1.0..5.0), &ctx).is_false(), "case 14"); 
  }

  /// Test the "overlaps before" examples  given in Table 78 of the Spec.
  #[test]
  fn test_overlaps_before() {
    // TODO: There are 81 different cases possible, but we only test 14.
    // This would be a good place to add more test cases than the spec has.
    let ctx = Context::new();
    let r_5_to_8 = ExclusiveInclusiveRange { start: &5.0_f64, end: &8.0_f64 };
    let r_1_to_5 = ExclusiveInclusiveRange { start: &1.0_f64, end: &5.0_f64 };

    assert!(Builtins::overlaps_before(rng_rng(1.0..=5.0, 3.0..=8.0), &ctx).is_true(), "case 1");
    assert!(Builtins::overlaps_before(rng_rng(1.0..=5.0, 6.0..=8.0), &ctx).is_false(), "case 2");
    assert!(Builtins::overlaps_before(rng_rng(1.0..=5.0, 5.0..=8.0), &ctx).is_true(), "case 3"); 
    assert!(Builtins::overlaps_before(rng_rng(1.0..=5.0, r_5_to_8), &ctx).is_false(), "case 4"); 
    assert!(Builtins::overlaps_before(rng_rng(1.0..5.0, 5.0..=8.0), &ctx).is_false(), "case 5"); 
    assert!(Builtins::overlaps_before(rng_rng(1.0..5.0, r_1_to_5), &ctx).is_true(), "case 6"); 
    assert!(Builtins::overlaps_before(rng_rng(1.0..=5.0, r_1_to_5), &ctx).is_true(), "case 7"); 
    assert!(Builtins::overlaps_before(rng_rng(1.0..5.0, 1.0..=5.0), &ctx).is_false(), "case 8"); 
    assert!(Builtins::overlaps_before(rng_rng(1.0..=5.0, 1.0..=5.0), &ctx).is_false(), "case 9"); 
  }

  /// Test the "overlaps after" examples  given in Table 78 of the Spec.
  #[test]
  fn test_overlaps_after() {
    // TODO: There are 81 different cases possible, but we only test 14.
    // This would be a good place to add more test cases than the spec has.
    let ctx = Context::new();
    let r_5_to_8 = ExclusiveInclusiveRange { start: &5.0_f64, end: &8.0_f64 };
    let r_1_to_5 = ExclusiveInclusiveRange { start: &1.0_f64, end: &5.0_f64 };

    assert!(Builtins::overlaps_after(rng_rng(3.0..=8.0, 1.0..=5.0), &ctx).is_true(), "case 1");
    assert!(Builtins::overlaps_after(rng_rng(6.0..=8.0, 1.0..=5.0), &ctx).is_false(), "case 2");
    assert!(Builtins::overlaps_after(rng_rng(5.0..=8.0, 1.0..=5.0), &ctx).is_true(), "case 3"); 
    assert!(Builtins::overlaps_after(rng_rng(r_5_to_8, 1.0..=5.0), &ctx).is_false(), "case 4"); 
    assert!(Builtins::overlaps_after(rng_rng(5.0..=8.0, 1.0..5.0), &ctx).is_false(), "case 5"); 
    assert!(Builtins::overlaps_after(rng_rng(r_1_to_5, 1.0..5.0), &ctx).is_true(), "case 6"); 
    // The following case has a "true" expectation in the DMN Spec, but appears to be incorrect.
    // The first range must end after the second range, not be coterminous. 
    // The spec has a missing clause in its logic and does not explain the semantics in words.
    // Consulted the HL7 CQL documentation for a textual description of the intended semantics. 
    assert!(Builtins::overlaps_after(rng_rng(r_1_to_5, 1.0..=5.0), &ctx).is_false(), "case 7"); 
    // Again, the spec has a wrong answer for this test case as well. 
    assert!(Builtins::overlaps_after(rng_rng(1.0..=5.0, 1.0..5.0), &ctx).is_true(), "case 8"); 
    assert!(Builtins::overlaps_after(rng_rng(1.0..=5.0, 1.0..=5.0), &ctx).is_false(), "case 9"); 
  }

  /// Test the "finishes" examples given in Table 78 of the Spec.
  #[test]
  fn test_finishes() {
    let ctx = Context::new();
    let r_1_to_10xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &10.0_f64 };

    assert!(Builtins::finishes(pt_rng(10, 1.0..=10.0), &ctx).is_true(), "case 1");
    assert!(Builtins::finishes(pt_rng(10, 1.0..10.0), &ctx).is_false(), "case 2");
    assert!(Builtins::finishes(rng_rng(5.0..=10.0, 1.0..=10.0), &ctx).is_true(), "case 3");
    assert!(Builtins::finishes(rng_rng(5.0..10.0, 1.0..=10.0), &ctx).is_false(), "case 4"); 
    assert!(Builtins::finishes(rng_rng(5.0..10.0, 1.0..10.0), &ctx).is_true(), "case 5");
    assert!(Builtins::finishes(rng_rng(1.0..=10.0, 1.0..=10.0), &ctx).is_true(), "case 6");
    assert!(Builtins::finishes(rng_rng(r_1_to_10xi, 1.0..=10.0), &ctx).is_true(), "case 7"); 
  }

  /// Test the "finished by" examples given in Table 78 of the Spec.
  #[test]
  fn test_finished_by() {
    let ctx = Context::new();
    let r_1_to_10xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &10.0_f64 };

    assert!(Builtins::finished_by(rng_pt(1.0..=10.0, 10), &ctx).is_true(), "case 1");
    assert!(Builtins::finished_by(rng_pt(1.0..10.0, 10), &ctx).is_false(), "case 2");
    assert!(Builtins::finished_by(rng_rng(1.0..=10.0, 5.0..=10.0), &ctx).is_true(), "case 3");
    assert!(Builtins::finished_by(rng_rng(1.0..=10.0, 5.0..10.0), &ctx).is_false(), "case 4"); 
    assert!(Builtins::finished_by(rng_rng(1.0..10.0, 5.0..10.0), &ctx).is_true(), "case 5");
    assert!(Builtins::finished_by(rng_rng(1.0..=10.0, 1.0..=10.0), &ctx).is_true(), "case 6");
    assert!(Builtins::finished_by(rng_rng(1.0..=10.0, r_1_to_10xi), &ctx).is_true(), "case 7"); 
  }

  /// Test the "includes" examples given in Table 78 of the Spec.
  #[test]
  fn test_includes() {
    let ctx = Context::new();
    let r_1_to_5xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &5.0_f64 };
    let r_1_to_10xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &10.0_f64 };
    let r_1_to_10xx = ExclusiveRange { start: &1.0_f64, end: &10.0_f64 };

    assert!(Builtins::includes(rng_pt(1.0..=10.0, 5), &ctx).is_true(), "case 1");
    assert!(Builtins::includes(rng_pt(1.0..=10.0, 12), &ctx).is_false(), "case 2");
    assert!(Builtins::includes(rng_pt(1.0..=10.0, 1), &ctx).is_true(), "case 3");
    assert!(Builtins::includes(rng_pt(1.0..=10.0, 10), &ctx).is_true(), "case 4"); 
    assert!(Builtins::includes(rng_pt(r_1_to_10xi, 1), &ctx).is_false(), "case 5");
    assert!(Builtins::includes(rng_pt(1.0..10.0, 10), &ctx).is_false(), "case 6");
    assert!(Builtins::includes(rng_rng(1.0..=10.0, 4.0..=6.0), &ctx).is_true(), "case 7");
    assert!(Builtins::includes(rng_rng(1.0..=10.0, 1.0..=5.0), &ctx).is_true(), "case 8");
    assert!(Builtins::includes(rng_rng(r_1_to_10xi, r_1_to_5xi), &ctx).is_true(), "case 9");
    assert!(Builtins::includes(rng_rng(1.0..=10.0, r_1_to_10xx), &ctx).is_true(), "case 10"); 
    assert!(Builtins::includes(rng_rng(1.0..10.0, 5.0..10.0), &ctx).is_true(), "case 11"); 
    assert!(Builtins::includes(rng_rng(1.0..=10.0, 1.0..10.0), &ctx).is_true(), "case 12"); 
    assert!(Builtins::includes(rng_rng(1.0..=10.0, r_1_to_10xi), &ctx).is_true(), "case 13"); 
    assert!(Builtins::includes(rng_rng(1.0..=10.0, 1.0..=10.0), &ctx).is_true(), "case 14"); 
  }

  /// Test the "during" examples given in Table 78 of the Spec.
  #[test]
  fn test_during() {
    let ctx = Context::new();
    let r_1_to_5xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &5.0_f64 };
    let r_1_to_10xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &10.0_f64 };
    let r_1_to_10xx = ExclusiveRange { start: &1.0_f64, end: &10.0_f64 };

    assert!(Builtins::during(pt_rng(5, 1.0..=10.0), &ctx).is_true(), "case 1");
    assert!(Builtins::during(pt_rng(12, 1.0..=10.0), &ctx).is_false(), "case 2");
    assert!(Builtins::during(pt_rng(1, 1.0..=10.0), &ctx).is_true(), "case 3");
    assert!(Builtins::during(pt_rng(10, 1.0..=10.0), &ctx).is_true(), "case 4"); 
    assert!(Builtins::during(pt_rng(1, r_1_to_10xi), &ctx).is_false(), "case 5");
    assert!(Builtins::during(pt_rng(10, 1.0..10.0), &ctx).is_false(), "case 6");
    assert!(Builtins::during(rng_rng(4.0..=6.0, 1.0..=10.0), &ctx).is_true(), "case 7");
    assert!(Builtins::during(rng_rng(1.0..=5.0, 1.0..=10.0), &ctx).is_true(), "case 8");
    assert!(Builtins::during(rng_rng(r_1_to_5xi, r_1_to_10xi), &ctx).is_true(), "case 9");
    assert!(Builtins::during(rng_rng(r_1_to_10xx, 1.0..=10.0), &ctx).is_true(), "case 10"); 
    assert!(Builtins::during(rng_rng(5.0..10.0, 1.0..10.0), &ctx).is_true(), "case 11"); 
    assert!(Builtins::during(rng_rng(1.0..10.0, 1.0..=10.0), &ctx).is_true(), "case 12"); 
    assert!(Builtins::during(rng_rng(r_1_to_10xi, 1.0..=10.0), &ctx).is_true(), "case 13"); 
    assert!(Builtins::during(rng_rng(1.0..=10.0, 1.0..=10.0), &ctx).is_true(), "case 14"); 
  }

  /// Test the "starts" examples given in Table 78 of the Spec.
  #[test]
  fn test_starts() {
    let ctx = Context::new();
    let r_1_to_5xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &5.0_f64 };
    let r_1_to_10xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &10.0_f64 };
    let r_1_to_10xx = ExclusiveRange { start: &1.0_f64, end: &10.0_f64 };

    assert!(Builtins::starts(pt_rng(1, 1.0..=10.0), &ctx).is_true(), "case 1");
    assert!(Builtins::starts(pt_rng(1, r_1_to_10xi), &ctx).is_false(), "case 2");
    assert!(Builtins::starts(pt_rng(2, 1.0..=10.0), &ctx).is_false(), "case 3");
    assert!(Builtins::starts(rng_rng(1.0..=5.0, 1.0..10.0), &ctx).is_true(), "case 4"); 
    assert!(Builtins::starts(rng_rng(r_1_to_5xi, r_1_to_10xi), &ctx).is_true(), "case 5");
    assert!(Builtins::starts(rng_rng(r_1_to_5xi, 1.0..=10.0), &ctx).is_false(), "case 6");
    assert!(Builtins::starts(rng_rng(1.0..=5.0, r_1_to_10xi), &ctx).is_false(), "case 7");
    assert!(Builtins::starts(rng_rng(1.0..10.0, 1.0..=10.0), &ctx).is_true(), "case 8");
    assert!(Builtins::starts(rng_rng(1.0..10.0, 1.0..=10.0), &ctx).is_true(), "case 9");
    assert!(Builtins::starts(rng_rng(r_1_to_10xx, r_1_to_10xi), &ctx).is_true(), "case 10"); 
  }

    /// Test the "started by" examples given in Table 78 of the Spec.
    #[test]
    fn test_started_by() {
      let ctx = Context::new();
      let r_1_to_5xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &5.0_f64 };
      let r_1_to_10xi = ExclusiveInclusiveRange { start: &1.0_f64, end: &10.0_f64 };
      let r_1_to_10xx = ExclusiveRange { start: &1.0_f64, end: &10.0_f64 };
  
      assert!(Builtins::started_by(rng_pt(1.0..=10.0, 1), &ctx).is_true(), "case 1");
      assert!(Builtins::started_by(rng_pt(r_1_to_10xi, 1), &ctx).is_false(), "case 2");
      assert!(Builtins::started_by(rng_pt(1.0..=10.0, 2), &ctx).is_false(), "case 3");
      assert!(Builtins::started_by(rng_rng(1.0..10.0, 1.0..=5.0), &ctx).is_true(), "case 4"); 
      assert!(Builtins::started_by(rng_rng(r_1_to_10xi, r_1_to_5xi), &ctx).is_true(), "case 5");
      assert!(Builtins::started_by(rng_rng(1.0..=10.0, r_1_to_5xi), &ctx).is_false(), "case 6");
      assert!(Builtins::started_by(rng_rng(r_1_to_10xi, 1.0..=5.0), &ctx).is_false(), "case 7");
      assert!(Builtins::started_by(rng_rng(1.0..=10.0, 1.0..10.0), &ctx).is_true(), "case 8");
      assert!(Builtins::started_by(rng_rng(1.0..=10.0, 1.0..10.0), &ctx).is_true(), "case 9");
      assert!(Builtins::started_by(rng_rng(r_1_to_10xi, r_1_to_10xx), &ctx).is_true(), "case 10"); 
    }
   
    /// Test the "coincides" examples given in Table 78 of the Spec.
    #[test]
    fn test_coincides() {
      let ctx = Context::new();
      let r_1_to_5 = ExclusiveRange { start: &1.0_f64, end: &5.0_f64 };
  
      assert!(Builtins::coincides(pt_pt(5, 5), &ctx).is_true(), "case 1");
      assert!(Builtins::coincides(pt_pt(3, 4), &ctx).is_false(), "case 2");
      assert!(Builtins::coincides(rng_rng(1.0..=5.0, 1.0..=5.0), &ctx).is_true(), "case 3"); 
      assert!(Builtins::coincides(rng_rng(r_1_to_5, 1.0..=5.0), &ctx).is_false(), "case 4"); 
      assert!(Builtins::coincides(rng_rng(1.0..=5.0, 2.0..=6.0), &ctx).is_false(), "case 5"); 
    }

    #[test]
    fn test_compare_bounds() {
      compare_bounds_test_case("[!", "[!", Ordering::Equal);
      compare_bounds_test_case("[3", "[3", Ordering::Equal);
      compare_bounds_test_case("(3", "(3", Ordering::Equal);
      compare_bounds_test_case("[3", "3]", Ordering::Equal);
      compare_bounds_test_case("3]", "[3", Ordering::Equal);
      compare_bounds_test_case("3]", "3]", Ordering::Equal);
      compare_bounds_test_case("3)", "3)", Ordering::Equal);
      compare_bounds_test_case("!]", "!]", Ordering::Equal);

      compare_bounds_test_case("[!", "!]", Ordering::Less);
      compare_bounds_test_case("[!", "[3", Ordering::Less);
      compare_bounds_test_case("[!", "(3", Ordering::Less);
      compare_bounds_test_case("[!", "3]", Ordering::Less);
      compare_bounds_test_case("[!", "3)", Ordering::Less);
      compare_bounds_test_case("[3", "[4", Ordering::Less);
      compare_bounds_test_case("[3", "(4", Ordering::Less);
      compare_bounds_test_case("[3", "(3", Ordering::Less);
      compare_bounds_test_case("(3", "[4", Ordering::Less);
      compare_bounds_test_case("(3", "(4", Ordering::Less);
      compare_bounds_test_case("[3", "4]", Ordering::Less);
      compare_bounds_test_case("[3", "4)", Ordering::Less);
      compare_bounds_test_case("(3", "!]", Ordering::Less);
      compare_bounds_test_case("[3", "!]", Ordering::Less);
      compare_bounds_test_case("3]", "4]", Ordering::Less);
      compare_bounds_test_case("3]", "4)", Ordering::Less);
      compare_bounds_test_case("3]", "(3", Ordering::Less);
      compare_bounds_test_case("3)", "[4", Ordering::Less);
      compare_bounds_test_case("3)", "(4", Ordering::Less);
      compare_bounds_test_case("3)", "[3", Ordering::Less); // Failing

      compare_bounds_test_case("!]", "[3", Ordering::Greater);
      compare_bounds_test_case("!]", "(3", Ordering::Greater);
      compare_bounds_test_case("!]", "[!", Ordering::Greater);
      compare_bounds_test_case("[4", "[3", Ordering::Greater);
      compare_bounds_test_case("[4", "(3", Ordering::Greater);
      compare_bounds_test_case("[4", "[!", Ordering::Greater);
      compare_bounds_test_case("(4", "[4", Ordering::Greater);
      compare_bounds_test_case("(4", "(3", Ordering::Greater);
      compare_bounds_test_case("(4", "[3", Ordering::Greater);
      compare_bounds_test_case("4]", "3]", Ordering::Greater);
      compare_bounds_test_case("4]", "4)", Ordering::Greater);
      compare_bounds_test_case("4]", "[!", Ordering::Greater);
      compare_bounds_test_case("4]", "[3", Ordering::Greater);
      compare_bounds_test_case("4]", "(3", Ordering::Greater);
      compare_bounds_test_case("4)", "3)", Ordering::Greater);
      compare_bounds_test_case("!]", "4)", Ordering::Greater);
      compare_bounds_test_case("!]", "4]", Ordering::Greater);
    }

    /// Parse a string into a Bound, with the following syntax (where # is an integer):
    /// 
    ///   [#  ... Lower bound, inclusive
    ///   (#  ... Lower bound, exclusive
    ///   [!  ... Lower bound, unbounded
    ///   #]  ... Upper bound, inclusive
    ///   #)  ... Upper bound, exclusive
    ///   !]  ... Upper bound, unbounded
    /// 
    /// Returns a tuple, where the first item is true for a lower bound, false for an upper bound.
    fn parse_bound(s: &str) -> (bool, Bound<FeelValue>) {
      match (s, s.chars().nth(0).unwrap(), s.chars().last().unwrap()) {
        ("[!", _, _) => (true, Bound::Unbounded),
        ("!]", _, _) => (false, Bound::Unbounded),
        (_, '[', _) => (true, Bound::Included(s[1..].parse::<i32>().unwrap().into())),
        (_, '(', _) => (true, Bound::Excluded(s[1..].parse::<i32>().unwrap().into())),
        (_, _, ']') => (false, Bound::Included(s[..s.len()-1].parse::<i32>().unwrap().into())),
        (_, _, ')') => (false, Bound::Excluded(s[..s.len()-1].parse::<i32>().unwrap().into())),
        _ => panic!("Bad format")
      }
    }

    fn compare_bounds_test_case(a_pattern: &str, b_pattern: &str, expect: Ordering) -> () {
      let (a_is_lower_bound, a) = parse_bound(a_pattern);
      let (b_is_lower_bound, b) = parse_bound(b_pattern);
      let actual = Builtins::compare_bounds(&a, a_is_lower_bound, &b, b_is_lower_bound);
      assert!(expect == actual, "Comparison {:?} vs {:?} of {:?} with {:?} got {:?} but expected {:?}", a_pattern, b_pattern, a, b, actual, expect);
    }

    //// Context function Tests
    
    #[test]
    fn test_context_get_value() {
      let ctx = Context::new();
      let arg_ctx = Context::new();
      let expected_value: FeelValue = "value1".into();

      let _previous_value = arg_ctx.insert("key1", expected_value.clone());
      let parameters = FeelValue::new_list(
        vec![FeelValue::Context(Rc::new(arg_ctx)), "key1".into()]
      );
      let actual_value = Builtins::get_value(parameters, &ctx);
      assert_eq!(actual_value, expected_value);
    }

    #[test]
    fn test_context_get_value_missing() {
      let ctx = Context::new();
      let arg_ctx = Context::new();
      let expected_value = FeelValue::Null;
      let parameters = FeelValue::new_list(
        vec![FeelValue::Context(Rc::new(arg_ctx)), "unexistent-key".into()]
      );
      let actual_value = Builtins::get_value(parameters, &ctx);
      assert_eq!(actual_value, expected_value);
    }

    #[test]
    fn test_context_get_entries() {
      let ctx = Context::new();
      let arg_ctx = Context::new();
      let key1 = FeelValue::Name("key1".into());
      let value1: FeelValue = "value1".into();
      let key2 = FeelValue::Name("key2".into());
      let value2: FeelValue = "value2".into();

      let _ = arg_ctx.insert("key1", value1.clone());
      let _ = arg_ctx.insert("key2", value2.clone());
      let parameters = FeelValue::Context(Rc::new(arg_ctx));
      let actual_value = Builtins::get_entries(parameters, &ctx);

      let expected_ctx_1 = Context::new();
      expected_ctx_1.insert("key", key1.clone());
      expected_ctx_1.insert("value", value1.clone());
      let expected_ctx_2 = Context::new();
      expected_ctx_2.insert("key", key2.clone());
      expected_ctx_2.insert("value", value2.clone());
      let expected = FeelValue::new_list(vec![
        FeelValue::Context(Rc::new(expected_ctx_1)),
        FeelValue::Context(Rc::new(expected_ctx_2))
      ]);
      assert_eq!(expected, actual_value);
    }
    
}
