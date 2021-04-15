use std::hash::Hash;
use std::collections::HashMap;

/*

The definitional formula for Standard Deviation requires two passes
and is prone to overflow. Some alternatives are prone to cancellation.
The algorithm in this class will use Welford's well known online algorithm,
which permits us to generate the sample or population variance and
standard deviations.

Wikipedia Python listing for Welford's Algorithm

# For a new value newValue, compute the new count, new mean, the new M2.
# mean accumulates the mean of the entire dataset
# M2 aggregates the squared distance from the mean
# count aggregates the number of samples seen so far
def update(existingAggregate, newValue):
    (count, mean, M2) = existingAggregate
    count += 1
    delta = newValue - mean
    mean += delta / count
    delta2 = newValue - mean
    M2 += delta * delta2
    return (count, mean, M2)

# Retrieve the mean, variance and sample variance from an aggregate
def finalize(existingAggregate):
    (count, mean, M2) = existingAggregate
    if count < 2:
        return float("nan")
    else:
        (mean, variance, sampleVariance) = (mean, M2 / count, M2 / (count - 1))
        return (mean, variance, sampleVariance)

*/

/// Welford's online algorithm for computing the mean, population and sample variance 
/// and population and sample standard deviation as a running calculation.
/// This algorithm is robustly resistant to overflow and catastophic cancellation.
#[derive(Debug)]
pub struct Welford {
  count: usize,
  mean: f64,
  m2: f64
}

impl Welford {

  pub fn new() -> Welford {
      Welford {
          count: 0,
          mean: 0.0,
          m2: 0.0
      }
  }

  /// Add another point to the set included in the variance calculations. 
  pub fn update(&mut self, new_value: f64) {
    self.count += 1;
    let delta = new_value - self.mean;
    self.mean += delta / (self.count as f64);
    let delta2 = new_value - self.mean;
    self.m2 += delta * delta2;
  }

  /// Running count of values of the series that have been accumulated so far.
  pub fn count(&self) -> usize {
    self.count
  }

  /// Running mean of values of the series that have been accumulated so far,
  /// or None if no values have been accumulated.
  pub fn mean(&self) -> Option<f64> {
      if self.count == 0 { None }
      else { Some(self.mean) }
  }

  /// Running population variance or None, if fewer than two values have 
  /// been accumulated so far.
  pub fn variance(&self) -> Option<f64> {
    if self.count <= 1 { None }
    else { Some(self.m2 / self.count as f64) }
  }

  /// Running sample variance or None, if fewer than two values have 
  /// been accumulated so far.
  pub fn sample_variance(&self) -> Option<f64> {
    if self.count <= 1 { None }
    else { Some(self.m2 / (self.count as f64 - 1.0)) }
  }

  /// Running population standard deviation or None, if fewer than two values have 
  /// been accumulated so far.
  pub fn standard_deviation(&self) -> Option<f64> {
      match self.variance() {
          Some(v) => Some(v.sqrt()),
          None => None
      }
  }

  /// Running sample standard deviation or None, if fewer than two values have 
  /// been accumulated so far.
  pub fn sample_standard_deviation(&self) -> Option<f64> {
    match self.sample_variance() {
        Some(v) => Some(v.sqrt()),
        None => None
    }
  }
}

/// Compute the population standard deviation for a stream of numbers.
pub fn standard_deviation<'a, I>(points: I) -> Option<f64>
where I: IntoIterator<Item = &'a f64>, {
 
    let mut w = Welford::new();
    for point in points.into_iter() {
        w.update(*point);
    }
    w.standard_deviation()
}

/// Compute the sample standard deviation for a stream of numbers.
pub fn sample_standard_deviation<'a, I>(points: I) -> Option<f64>
where I: IntoIterator<Item = &'a f64>, {

    let mut w = Welford::new();
    for point in points.into_iter() {
        w.update(*point);
    }
    w.sample_standard_deviation()
}

/// Compute the mode of a collection of items. 
/// If several items are tied for the most frequent,
/// only one is returned. 
/// Panics if the iterator yields no values. 
pub fn mode<'a, I, T>(items: I) -> &'a T 
where I: IntoIterator<Item = &'a T>, 
      T: Hash + Eq, {
  let mut occurrences: HashMap<&T, usize> = HashMap::new();

  for value in items.into_iter() {
      *occurrences.entry(value).or_insert(0) += 1;
  }

  occurrences
      .into_iter()
      .max_by_key(|&(_, count)| count)
      .map(|(val, _)| val)
      .expect("Cannot compute the mode of zero items")
}

/// Compute the mode (most frequently occurring) item or items of a collection, returning a list of items. 
/// If several items are tied for the most frequent, all are returned in the list. 
/// If no items are in the collection, an empty list is returned. 
pub fn mode_with_ties<T>(items: &Vec<T>) -> Vec<T> 
where T: Hash + Eq + Ord + Clone, {
  // Increment a reference and return the new count.
  fn incr(n: &mut usize) -> usize {
    *n += 1_usize;
    *n
  }

  // Updates the tallies, etc. after each item is visited.
  fn update<'a, T>(value: &'a T, tallies: &mut HashMap<&'a T, usize>, common: &mut Vec<&'a T>, max_ct: &mut usize) 
  where T: Hash + Eq + Ord + Clone {
    let new_count = incr(tallies.entry(value).or_insert(0));
    if new_count > *max_ct {
      common.clear();
      common.push(value);
      *max_ct = new_count;
    }
    else if new_count == *max_ct {
      common.push(value);
    }
  }  

  let mut occurrences = HashMap::new();
  let mut most_common_items: Vec<&T> = Vec::new();
  let mut max_count = 1_usize;

  for value in items.into_iter() {
      update(value, &mut occurrences, &mut most_common_items, &mut max_count);
  }
  // The benefit of this approach is that only the mode items are cloned.
  let mut mode_result: Vec<T> = most_common_items.into_iter().cloned().collect();
  mode_result.sort();
  return mode_result;
}


#[cfg(test)]
mod tests {
  use super::sample_standard_deviation;
  use super::mode;
  use super::mode_with_ties;

  #[test]
  fn test_sample_standard_deviation() {
    let points = &[2.0, 4.0, 7.0, 5.0];
    let actual = sample_standard_deviation(points).unwrap();
    let expected = 2.081665999466133_f64;
    // println!("actual   = {}\nexpected = {}", actual, expected);
    assert!((actual-expected).abs() < 0.000000000000001);
  }

  #[test]
  fn test_mode() {
    let points = &["duck", "duck", "duck", "goose"];
    let actual = *mode(points);
    let expected = "duck";
    // println!("actual   = {}\nexpected = {}", actual, expected);
    assert_eq!(actual, expected);
  }

  #[test]
  fn test_mode_with_ties() {
    let points = vec!["duck", "duck", "duck", "goose", "Dit", "Dit", "Dit", "Dah"];
    let actual = mode_with_ties(&points);
    let expected = vec!["Dit", "duck"]; // mode_with_ties sorts the values.
    // println!("actual   = {:?}\nexpected = {:?}", actual, expected);
    assert_eq!(actual, expected);
  }

  #[test]
  fn test_mode_with_ties_empty() {
    let points: Vec<&str> = Vec::new();
    let actual = mode_with_ties(&points);
    let expected: Vec<&str> = Vec::new();
    // println!("actual   = {:?}\nexpected = {:?}", actual, expected);
    assert_eq!(actual, expected);
  }
}


