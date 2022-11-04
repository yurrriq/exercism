use crate::error::Error;
use crate::pitch::Pitch;

pub struct Interval(usize);

pub struct Scale {
    tonic: Pitch,
    pitches: Vec<Pitch>,
    intervals: Vec<Interval>,
}

impl Scale {
    pub fn new(tonic: &str, intervals: &str) -> Result<Scale, Error> {
        unimplemented!(
            "Construct a new scale with tonic {} and intervals {}",
            tonic,
            intervals
        )
    }

    pub fn chromatic(tonic: &str) -> Result<Scale, Error> {
        unimplemented!("Construct a new chromatic scale with tonic {}", tonic)
    }

    pub fn enumerate(&self) -> Vec<String> {
        unimplemented!()
    }
}
