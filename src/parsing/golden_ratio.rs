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
  yielded: Option<u64>,

  /// Should the sequence repeat endlessly or stop after sequence_length items have been yielded?
  repeat: bool
}

impl GoldenSequence {
  /// Create a new GoldenSequence for the numbers 0 to len - 1 that does not repeat. 
  pub fn new(len: u64) -> GoldenSequence {
    GoldenSequence {
      sequence_length: len, 
      fibonacci: GoldenSequence::smallest_fibonacci(len),
      index: 0,
      next_position: 0,
      yielded: None,
      repeat: false
    }
  }

  /// Create a new GoldenSequence for the numbers 0 to len - 1 that starts over after every number has been yielded
  /// once. This sequence is infinite in length. 
  pub fn new_infinite(len: u64) -> GoldenSequence {
    GoldenSequence {
      sequence_length: len, 
      fibonacci: GoldenSequence::smallest_fibonacci(len),
      index: 0,
      next_position: 0,
      yielded: None,
      repeat: true
    }
  }

  /// For an infinite sequence, this restarts the sequence. 
  fn restart(&mut self) {
    self.index = 0;
    self.next_position = 0;
    self.yielded = None;
  }

  /// Find the smallest fibonacci number that is greater than or equal to n. 
  fn smallest_fibonacci(n: u64) -> u64 {
    // The sequence is only non-repeating if the arithmetic is performed modulo a
    // fibonacci number. We will just throw away numbers >= len when we iterate. 
    // The worst case is that 38.2% of the computed numbers must be discarded. 
    // If len is a fibonacci number, no computed values need to be discarded. 
    // (Fibonacci numbers are involved because they are related to Phi. 
    //  As the Fibonacci sequence increases, the ratio between each Fibonacci number
    //  and the previous one approaches Phi in the limit.)
    let mut fib_prev = 0_u64;
    let mut fib_curr = 1_u64;
    while fib_curr < n {
      let temp = fib_prev + fib_curr;
      fib_prev = fib_curr;
      fib_curr = temp;
    }
    fib_curr
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

    // NOTE: We could replace the above by using ratios of integers and an integer ratio 
    //       approximation for PHI if it becomes a performance issue. 
  }

  /// Most recently yielded value, which is None before the first call to next, and
  /// None after the iterator completes. 
  pub fn current(&self) -> Option<u64> {
    self.yielded
  }

  fn advance(&mut self) -> Option<u64> {
    // Warning: Assumes caller performs bounds check on self.index. 
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

impl Iterator for GoldenSequence {
  type Item = u64;
  
  /// Yield the next number in the low discrepancy, pseudo-random sequence.
  ///   - For a nonrepeating sequence, every number from zero to N - 1 
  ///     will be yielded exactly once, but in a pseudo-random order. 
  ///   - For a repeating sequence, when the sequence finishes, it restarts
  ///     and yields the values in the same order as before. 
  fn next(&mut self) -> Option<u64> {
    match (self.index >= self.sequence_length, self.repeat) {
      (true, true) => { 
        self.restart();
        self.advance()
      },
      (true, false) => {
        self.yielded = None;
        self.yielded
      },
      (false, _) => {
        self.advance()
      }
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

  /// Test a repeatable sequence to make sure it repeats.
  #[test]
  fn test_repeat() {
    let iter = GoldenSequence::new_infinite(13_u64);
    let actual_seq: Vec<u64> = iter.take(26).collect();
    let expected_seq: Vec<u64> = vec![
      8, 3, 11, 6, 1, 9, 4, 12, 7, 2, 10, 5, 0, 8, 3, 11, 6, 1, 9, 4, 12, 7, 2, 10, 5, 0
    ];
    assert!(actual_seq == expected_seq);
  }

}
