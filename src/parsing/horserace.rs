
use std::cmp::Ordering;
use std::fmt::{Debug, Result, Formatter};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum MedianRelation {
    PossiblyLesser,

    /// Definitely below the median
    Lesser,

    /// Definitely equals the lower of the two values in the middle of a collection with an even 
    /// number of samples
    LowMedian, 

    PossiblyMedian,

    /// Definitely equals the median value, either because it is in the middle of a collection
    /// with an odd number of items, or because the number of equal values at the middle
    /// means that the low and high median values are equal. 
    Median, 

    /// Definitely equals the higher of the two values in the middle of a collection with an even 
    /// number of samples
    HighMedian,

    /// Definitely above the median
    Higher,

    PossiblyHigher,

    /// Not enough is known to determine conclusively where the value is
    /// relative to the median. 
    Unknown
}

impl MedianRelation {
    pub fn has_finished_race(self) -> bool {
        match self {
            MedianRelation::Unknown => false,
            MedianRelation::PossiblyLesser => false,
            MedianRelation::PossiblyMedian => false,
            MedianRelation::PossiblyHigher => false,
            _ => true
        }
    }
}

pub struct Horse {
    /// Count of how many horses are known to be slower than this horse.
    lesser: u32,
    /// Count of how many horses are known to match the speed of this horse. 
    equal: u32,
    /// Count of how many horses are known to be faster than this horse.
    greater: u32,
    /// Zero-based index into the field of all horses for this horse
    start_index: u32,
    /// Total number of horses. 
    track_length: u32,
    /// Most recent assessment of this horse's position relative to the median. 
    relative_rank: MedianRelation
}

impl Horse {
    pub fn new(start_index: u32, track_length: u32) -> Horse {
        Horse {
            lesser: 0,
            equal: 0,
            greater: 0,
            start_index: start_index,
            track_length: track_length,
            relative_rank: MedianRelation::Unknown
        }
    }

    pub fn get_start_index(&self) -> u32 {
        self.start_index
    }

    pub fn distance_run(&self) -> u32 {
        self.lesser + self.equal + self.greater + 1 // Add one to count this horse, too!
    }    
    pub fn distance_remaining(&self) -> u32 {
        // Add one to count this horse, too!
        (self.track_length as i64 - (self.lesser + self.equal + self.greater + 1) as i64) as u32
    }
    pub fn has_finished_race(&self) -> bool {
        self.relative_rank.has_finished_race()
    }

    pub fn get_relative_rank(&self) -> MedianRelation {
        self.relative_rank
    }

    /// Get the zero-based ranks that the low and high median should have.
    /// If the track_length is odd, we have a single center and the two numbers are equal,
    /// if even, a double center, and the numbers differ by one. 
    pub fn get_center(&self) -> (u32,u32) {
        let half: u32 = self.track_length / 2;
        if self.track_length % 2 == 1 {
            (half, half)
        }
        else {
            (half - 1, half)
        }
    }

    /// Get the zero-based rank if all remaining horses are higher. 
    pub fn lowest_possible_rank(&self) -> u32 {
        // Do not add one for this horse, else it can never have rank zero. 
        self.lesser
    }

    /// Get the zero-based rank if all remaining horses are lower. 
    pub fn highest_possible_rank(&self) -> u32 {
        self.lesser + self.distance_remaining()
    }

    /// Deduce where this Horse stands relative to the median. 
    /// It is sometimes possible to know this precisely before
    /// comparing it to every other Horse. 
    pub fn compute_relative_rank(&self) -> MedianRelation {
        // NOTE: This is lots of conversion between zero and one-based numbers.
        //       Lots of places for fencepost errors. 
        //       This is one of the most crucial functions. 

        // Note: Remember that we do not compare a Horse to itself, but it counts as one Horse. 
        let (center_low, center_high) = self.get_center();

        // Add one for one-based to zero-based comparison
        if self.lesser > center_high + 1 { 
            // Even if all remaining horses were adverse to this, it could not be made into 
            // a median or a Lesser. 
            MedianRelation::Higher
        }
        else if self.track_length - self.greater - 1 < center_low {
            // Even if all remaining horses were adverse to this, it could not be made into 
            // a median or a Higher. 
            MedianRelation::Lesser
        }
        else {
            let remaining = self.distance_remaining();
            if remaining == 0 {
                // All horses have been compared to this horse,
                // so we know definitively whether we are the median or not. 
                let ranks_contained = self.lesser ..= (self.lesser + self.equal);
                let could_be_low_median = ranks_contained.contains(&center_low);
                let could_be_high_median = ranks_contained.contains(&center_high);
                // println!("could be lo/hi ({}/{}). center ({},{}) => {:?}", 
                //     could_be_low_median, could_be_high_median, center_low, center_high, self
                // );
                match (could_be_low_median, could_be_high_median) {
                    (true, true) => MedianRelation::Median,
                    (true, false) => MedianRelation::LowMedian,
                    (false, true) => MedianRelation::HighMedian,
                    (false, false) => {
                        if self.lesser > self.greater {
                            MedianRelation::Higher
                        }
                        else {
                            // If lesser equaled greater, then we would be at the median,
                            // already taken care of above. 
                            MedianRelation::Lesser
                        }
                    }
                }
            }
            else {
                if self.lesser > self.greater {
                    MedianRelation::PossiblyHigher
                }
                else if self.lesser + self.equal < self.greater {
                    MedianRelation::PossiblyLesser
                }
                else {
                    MedianRelation::PossiblyMedian
                }
            }
        }

    }

    /// Return the absolute zero-based rank only when we are certain. 
    pub fn rank(&self) -> Option<u32> {
        if self.has_finished_race() {
            Some(self.lowest_possible_rank())
        }
        else {
            None
        }
    }

    /// When the horse takes another step, it is compared to another horse
    /// and the result of the comparison is used to update the statistics. 
    /// Only if update_rank is true do we recompute the relative rank. 
    pub fn step(&mut self, relation: Ordering, update_rank: bool) -> bool {
        if self.has_finished_race() {
            return false;
        }
        match relation{
            Ordering::Less => { self.lesser += 1; },
            Ordering::Greater => { self.greater += 1; },
            Ordering::Equal => { self.equal += 1; }
        }
        // compute_relative_rank is an expensive calculation. 
        // We will only start updating a horse after it hits its stride. 
        // Since most horses will be rejected immediately upon hitting their stride,
        // this will only be computed for them once.  
        if update_rank {
            self.relative_rank = self.compute_relative_rank();
        }
        true
    }
}


impl Debug for Horse {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "Horse {} of {}: <{} ={}= {}> is {:?}", 
            self.start_index, self.track_length, self.lesser, self.equal, self.greater, self.relative_rank
        )
    }
}

/////////////// TESTS /////////////////

#[cfg(test)]
mod tests {
    use super::{Horse, MedianRelation};
    use std::cmp::Ordering;

    #[test]
    fn test_get_center() {
        let bob_tailed_nag = Horse::new(0, 20);
        assert_eq!((9_u32,10_u32), bob_tailed_nag.get_center());

        let bay = Horse::new(0, 19);
        assert_eq!((9_u32,9_u32), bay.get_center());
    }

    #[test]
    fn test_horse_compute_relative_rank() {
        let mut bob_tailed_nag = Horse::new(0, 20);
        for _ in 0_usize..3_usize {
            bob_tailed_nag.step(Ordering::Less, true);
            bob_tailed_nag.step(Ordering::Less, true);
            bob_tailed_nag.step(Ordering::Less, true);
            bob_tailed_nag.step(Ordering::Greater, true);
            bob_tailed_nag.step(Ordering::Equal, true);
        }
        assert_eq!(bob_tailed_nag.get_relative_rank(), MedianRelation::PossiblyHigher);
    }

    #[test]
    fn test_horse_step() {
        let mut bob_tailed_nag = Horse::new(0, 19);
        // Note that each Horse in itself counts for one, so if the field has 19 horses,
        // we only take 18 steps to reach the finish line. 
        let step_and_expectation: Vec<(Ordering, MedianRelation)> = vec![
            (Ordering::Less, MedianRelation::PossiblyHigher),
            (Ordering::Less, MedianRelation::PossiblyHigher),
            (Ordering::Less, MedianRelation::PossiblyHigher),
            (Ordering::Greater, MedianRelation::PossiblyHigher),
            (Ordering::Less, MedianRelation::PossiblyHigher),
            (Ordering::Greater, MedianRelation::PossiblyHigher),
            (Ordering::Greater, MedianRelation::PossiblyHigher),
            (Ordering::Less, MedianRelation::PossiblyHigher),
            (Ordering::Less, MedianRelation::PossiblyHigher),
            (Ordering::Less, MedianRelation::PossiblyHigher),
            (Ordering::Greater, MedianRelation::PossiblyHigher),
            (Ordering::Greater, MedianRelation::PossiblyHigher),
            (Ordering::Greater, MedianRelation::PossiblyHigher),
            (Ordering::Greater, MedianRelation::PossiblyMedian),
            (Ordering::Greater, MedianRelation::PossiblyLesser),
            (Ordering::Less, MedianRelation::PossiblyMedian),
            (Ordering::Less, MedianRelation::PossiblyHigher),
            (Ordering::Greater, MedianRelation::Median)
        ];
        let mut i = 0_i32;
        for (comparison, expected_relation) in step_and_expectation.iter() {
            i += 1;
            bob_tailed_nag.step(*comparison, true);
            // println!(
            //     "{}. comparison = {:?}. exp = {:?} act = {:?} => {:?}", 
            //     i, *comparison, *expected_relation, bob_tailed_nag.get_relative_rank(), bob_tailed_nag
            // );
            assert_eq!(*expected_relation, bob_tailed_nag.get_relative_rank());
        }
    }

    #[test]
    fn test_detect_lesser_early() {
        let mut bob_tailed_nag = Horse::new(0, 19);
        // We know the answer early in this case. 
        let step_and_expectation: Vec<(Ordering, MedianRelation)> = vec![
            (Ordering::Greater, MedianRelation::PossiblyLesser),
            (Ordering::Less, MedianRelation::PossiblyMedian),
            (Ordering::Less, MedianRelation::PossiblyHigher),
            (Ordering::Greater, MedianRelation::PossiblyMedian),
            (Ordering::Less, MedianRelation::PossiblyHigher),
            (Ordering::Greater, MedianRelation::PossiblyMedian),
            (Ordering::Greater, MedianRelation::PossiblyLesser),
            (Ordering::Less, MedianRelation::PossiblyMedian),
            (Ordering::Greater, MedianRelation::PossiblyLesser),
            (Ordering::Less, MedianRelation::PossiblyMedian),
            (Ordering::Greater, MedianRelation::PossiblyLesser),
            (Ordering::Greater, MedianRelation::PossiblyLesser),
            (Ordering::Greater, MedianRelation::PossiblyLesser),
            (Ordering::Greater, MedianRelation::PossiblyLesser),
            (Ordering::Greater, MedianRelation::Lesser)
        ];
        for (comparison, expected_relation) in step_and_expectation.iter() {
            bob_tailed_nag.step(*comparison, true);
            assert_eq!(*expected_relation, bob_tailed_nag.get_relative_rank());
        }
    }

    #[test]
    fn test_double_center_median() {
        let mut bob_tailed_nag = Horse::new(0, 20);
        let step_and_expectation: Vec<(Ordering, MedianRelation)> = vec![
            (Ordering::Less, MedianRelation::PossiblyHigher),
            (Ordering::Less, MedianRelation::PossiblyHigher),
            (Ordering::Less, MedianRelation::PossiblyHigher),
            (Ordering::Greater, MedianRelation::PossiblyHigher),
            (Ordering::Less, MedianRelation::PossiblyHigher),
            (Ordering::Greater, MedianRelation::PossiblyHigher),
            (Ordering::Greater, MedianRelation::PossiblyHigher),
            (Ordering::Less, MedianRelation::PossiblyHigher),
            (Ordering::Less, MedianRelation::PossiblyHigher),
            (Ordering::Less, MedianRelation::PossiblyHigher),
            (Ordering::Greater, MedianRelation::PossiblyHigher),
            (Ordering::Greater, MedianRelation::PossiblyHigher),
            (Ordering::Greater, MedianRelation::PossiblyHigher),
            (Ordering::Greater, MedianRelation::PossiblyMedian),
            (Ordering::Greater, MedianRelation::PossiblyLesser),
            (Ordering::Less, MedianRelation::PossiblyMedian),
            (Ordering::Less, MedianRelation::PossiblyHigher),
            (Ordering::Less, MedianRelation::PossiblyHigher),
            (Ordering::Greater, MedianRelation::HighMedian)
        ];
        let mut i = 0_i32;
        for (comparison, expected_relation) in step_and_expectation.iter() {
            i += 1;
            bob_tailed_nag.step(*comparison, true);
            assert_eq!(*expected_relation, bob_tailed_nag.get_relative_rank());
        }
    }

}

