use std::hash::Hash;
use std::collections::HashMap;
use std::cmp::{Ord,Ordering};
use pdqselect::{select, select_by};

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

  /// Updates the tallies, etc. after each item is visited.
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

/// Result for the median function, which holds the index or idices into the data where
/// the median value or values may be found.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum MedianIndex {
  /// The population has an odd number of values, hence a single median.
  Single(usize),
  /// The population has an even number of values, hence a double median.
  Dual(usize,usize),
  /// The population is empty, hence has no median.
  None
}

/// Find the median of a collection, which may be one or two values,
/// depending on whether the collection has an even or odd number of elements. 
/// The input array is partially sorted as a result. 
/// The median value or values will end up in the middle of the array. 
pub fn median_mut<T>(v: &mut [T]) -> MedianIndex
where T: Ord {
  match (v.len(), v.len() % 2 == 0) {
    (0, _) => MedianIndex::None,
    (1, _) => MedianIndex::Single(0),
    (2, _) => if v[0].cmp(&v[1]) == Ordering::Greater {
      v.swap(0,1);
      MedianIndex::Dual(0,1)
    }
    else {
      MedianIndex::Dual(0,1)
    },
    (_, true) => {
      // Length is even, so there are two median values.
      let k = (v.len() / 2) - 1;
      select(v, k);
      // The select method does a partial sort. 
      // If the low median has duplicate values, 
      // it is highly likely that select will have already
      // shifted one of those duplicates into the high median's position. 
      // Othewise, do a second search, this time over the upper half of the list. 
      if v[k].cmp(&v[k+1]) != Ordering::Equal {
        let index_of_min: usize = v[k+1..]
          .iter()
          .enumerate()
          .filter(|(_, item)| (*item).cmp(&v[k]) != Ordering::Less)
          .min_by(|(_, a), (_, b)| a.cmp(b))
          .map(|(index, _)| index).unwrap();
        v.swap(k+1, index_of_min);
      }
      MedianIndex::Dual(k, k+1)
    },
    (_, false) => {
      // Length is odd, so there is a single median value.
      let k = v.len() / 2;
      select(v, k);
      MedianIndex::Single(k)
    }
  }
}

pub fn median<T>(v: &[T]) -> MedianIndex 
where T: Ord {
  let population = v.len();
  match (population, population % 2 == 0) {
    (0, _) => MedianIndex::None,
    (1, _) => MedianIndex::Single(0),
    (2, _) => match v[0].cmp(&v[1]) {
        Ordering::Greater => MedianIndex::Dual(1,0),
        _ => MedianIndex::Dual(0,1)
    },
    (_, true) => {
      // Length is even, so there are two median values.
      let mut indices: Vec<usize> = (0..population).collect();
      let sorter = |a: &usize,b: &usize| v[*a].cmp(&v[*b]);

      let k = (population / 2) - 1;
      select_by(&mut indices, k, sorter);
      let i_low_median = indices[k];
      let mut i_high_median = indices[k+1];

      // The select method does a partial sort. 
      // If the low median has duplicate values, 
      // it is highly likely that select will have already
      // shifted one of those duplicates into the high median's position. 
      // Othewise, do a second search, this time over the upper half of the list. 
      if v[i_low_median].cmp(&v[i_high_median]) != Ordering::Equal {
        let found = indices[k+1..]
          .iter()
          .copied()
          .filter(|i| v[*i].cmp(&v[i_low_median]) != Ordering::Less)
          .min_by(|a: &usize, b: &usize| v[*a].cmp(&v[*b]))
          .unwrap();
        i_high_median = found;
      }
      MedianIndex::Dual(i_low_median, i_high_median)
    },
    (_, false) => {
      // Length is odd, so only a single median. 
      let mut indices: Vec<usize> = (0..population).collect();
      let sorter = |a: &usize,b: &usize| v[*a].cmp(&v[*b]);
      let k = population / 2;
      select_by(&mut indices, k, sorter);
      MedianIndex::Single(indices[k])
    }
  }
}


#[cfg(test)]
mod tests {
  use super::sample_standard_deviation;
  use super::mode;
  use super::mode_with_ties;
  use super::{median_mut, median};
  use super::MedianIndex;

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

  #[test]
  fn test_median_mut(){
    let mut data: Vec<i32> = vec![20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1];
    let actual_median = median_mut(&mut data);
    // Values get sorted into place, so their positions changed. 
    assert_eq!(MedianIndex::Dual(9,10), actual_median);
    assert_eq!(data[9], 10);
  }

  #[test]
  fn test_median_even_length(){
    let data: Vec<i32> = vec![2,7,10,4,9,12,15,13,1,20,19,3,5,6,8,11,14,16,17,18];
    let actual_median = median(&data);
    // Values do not get sorted into place, so their positions do not change. 
    assert_eq!(MedianIndex::Dual(2,15), actual_median);
    match actual_median {
      MedianIndex::Dual(low, high) => {
        assert_eq!(data[low], 10);
        assert_eq!(data[high], 11)
      },
      _ => { assert!(false); }
    }
  }

  #[test]
  fn test_median_odd_length(){
    let data: Vec<i32> = vec![2,7,10,4,9,12,15,13,1,19,3,5,6,8,11,14,16,17,18];
    let actual_median = median(&data);
    // Values do not get sorted into place, so their positions do not change. 
    assert_eq!(MedianIndex::Single(2), actual_median);
    match actual_median {
      MedianIndex::Single(low) => {
        assert_eq!(data[low], 10);
      },
      _ => { assert!(false); }
    }
  }
}


