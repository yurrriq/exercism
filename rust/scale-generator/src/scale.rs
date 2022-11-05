use crate::accidental::Accidental;
use crate::error::Error;
use crate::pitch::Pitch;
use crate::pitch_class::PitchClass;

use enum_iterator::next;
use int_enum::IntEnum;
use std::str::FromStr;

#[derive(Debug)]
pub struct Interval(pub isize);

#[derive(Debug)]
pub struct Scale {
    tonic: Pitch,
    pitches: Vec<Pitch>,
    intervals: Vec<Interval>,
}

impl Scale {
    pub fn new(tonic: &str, intervals: &str) -> Result<Scale, Error> {
        let the_tonic = Pitch::from_str(tonic)?;
        let intervals = vec![2, 2, 1, 2, 2, 2, 1]
            .into_iter()
            .map(|n| Interval(n))
            .collect::<Vec<Interval>>();
        Ok(Scale {
            tonic: the_tonic,
            pitches: vec![the_tonic],
            intervals,
        })
    }

    pub fn chromatic(tonic: &str) -> Result<Scale, Error> {
        unimplemented!("Construct a new chromatic scale with tonic {}", tonic)
    }

    pub fn enumerate(&self) -> Vec<String> {
        unimplemented!()
    }
}

impl Iterator for Scale {
    type Item = Pitch;

    fn next(&mut self) -> Option<Self::Item> {
        let last_pitch = self.pitches.last().unwrap_or(&self.tonic);
        let next_pitch_class = next(&last_pitch.pitch_class)?;
        let next_step = self.intervals.pop()?;
        let increment: isize =
            if next_pitch_class == PitchClass::C || next_pitch_class == PitchClass::F {
                1
            } else {
                2
            };
        let next_accidental =
            Accidental::from_int(last_pitch.accidental.int_value() + next_step.0 - increment)
                .ok()?;
        let next_pitch = &Pitch {
            pitch_class: next_pitch_class,
            accidental: next_accidental,
        };

        if next_pitch == &self.tonic {
            None
        } else {
            self.pitches.push(*next_pitch);
            Some(*next_pitch)
        }
    }
}
