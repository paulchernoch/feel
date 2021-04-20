use lazy_static;
lazy_static! {
  /// The Golden Ratio, Phi
  pub static ref PHI: f64 = (1.0 + 5.0_f64.sqrt()) / 2.0;
}

/*
The usefulness of the Golden Ratio for Generating a Pseudo-random sequence
was reported in this paper:

Colas Schretter, Leif Kobbelt, Paul-Olivier Dehaye. 
"Golden Ratio Sequences For Low-Discrepancy Sampling".
Journal of Graphic Tools (2011).

See https://www.graphics.rwth-aachen.de/publication/2/jgt.pdf

Adapting that sequence to generate a complete sequence for [0,N) without duplication
for any value of N was my idea. - Paul A. Chernoch
*/

/// Generates a low-discrepancy pseudo-random sequence of length N that yields every number 
/// from 0 to N-1 exactly once, without repeating itself, based on the Golden Ratio Phi. 
/// Such a sequence is useful for randomizing access to data in Monte Carlo algorithms,
/// in place of randomly shuffling the data array. 
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GoldenSequence {
  /// Length of the sequence before it repeats. Defines a half open range of [0..length)
  sequence_length: u64,

  /// Smallest fibonacci number greater than or equal to length. 
  fibonacci: u64,

  /// Current index into the sequence of length sequence_length, which is modulo length. 
  index: u64,

  /// Next position to yiled in the sequence of length fibonacci
  next_position: u64,

  /// Most recent value yielded by the iterator. 
  yielded: Option<u64>
}

impl GoldenSequence {
  /// Create a new GoldenSequence for the numbers 0 to len - 1. 
  pub fn new(len: u64) -> GoldenSequence {
    // Find the smallest fibonacci number that is greater than or equal to len. 
    // The sequence is only non-repeating if the arithmetic is performed modulo a
    // fibonacci number. We will just throw away numbers >= len when we iterate. 
    // The worst case is that 38.2% of the computed numbers must be discarded. 
    // If len is a fibonacci number, no computed values need to be discarded. 
    // (Fibonacci numbers are involved because they are related to Phi. 
    //  As the Fibonacci sequence increases, the ratio between each Fibonacci number
    //  and the previous one approaches Phi in the limit.)
    let mut fib_prev = 0_u64;
    let mut fib_curr = 1_u64;
    while fib_curr < len {
      let temp = fib_prev + fib_curr;
      fib_prev = fib_curr;
      fib_curr = temp;
    }
    GoldenSequence {
      sequence_length: len, 
      fibonacci: fib_curr,
      index: 0,
      next_position: 0,
      yielded: None
    }
  }

  /// Compute the nth number in the larger sequence of length fibonacci. 
  fn nth_item(&self, n: u64) -> u64 {
    let n_mod_fibonacci = (n % self.fibonacci) as f64;
    // We are doing two things here: 
    //   1) Generating a pseudo-random floating point number in the range [0,1)
    //      using Phi, the Golden Ratio. 
    //   2) Multiplying by fibonacci and flooring the value to get an integer
    //      in the range [0,fibonacci)
    (((*PHI * (n_mod_fibonacci + 1.0)) % 1.0) * (self.fibonacci as f64)).floor() as u64
  }

  /// Most recently yielded value, which is None before the first call to next, and
  /// None after the iterator completes. 
  pub fn current(&self) -> Option<u64> {
    self.yielded
  }
}

impl Iterator for GoldenSequence {
  type Item = u64;
  
  /// Yield the next number in the low discrepancy, pseudo-random sequence.
  /// Every number from zero to N - 1 will be yielded exactly once,
  /// but in a pseudo-random order. 
  fn next(&mut self) -> Option<u64> {
    if self.index >= self.sequence_length {
      self.yielded = None;
      self.yielded
    }
    else {
      for pos in self.next_position .. self.fibonacci {
        let next_index = self.nth_item(pos);
        // We discard numbers not in the range [0,sequence_length)
        if next_index < self.sequence_length {
          self.index += 1_u64;
          self.next_position = pos + 1;
          self.yielded = Some(next_index);
          return self.yielded
        }
      }
      self.yielded = None;
      self.yielded
    }
  }
}


/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
  use super::GoldenSequence;

  /// Test of a sequence whose length of 13 is a Fibonacci number,
  /// hence need not discard any values.
  #[test]
  fn test_13() {
    let iter = GoldenSequence::new(13_u64);
    let actual_seq: Vec<u64> = iter.collect();
    let expected_seq: Vec<u64> = vec![
      8, 3, 11, 6, 1, 9, 4, 12, 7, 2, 10, 5, 0
    ];
    assert!(actual_seq == expected_seq);
  }

  /// Test of a sequence whose length of 24 is NOT a Fibonacci number,
  /// hence needs to discard values.
  #[test]
  fn test_28() {
    let iter = GoldenSequence::new(28);
    let actual_seq: Vec<u64> = iter.collect();
    let expected_seq: Vec<u64> = vec![
      21,8,16,3,24,11,19,6,27,14,1,22,9,17,4,25,12,20,7,15,2,23,10,18,5,26,13,0
    ];
    assert!(actual_seq == expected_seq);
  }

}
