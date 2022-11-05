use crate::accidental::Accidental;
use crate::error::Error;
use crate::pitch_class::PitchClass;

use std::fmt;
use std::str::FromStr;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Pitch {
    pub pitch_class: PitchClass,
    pub accidental: Accidental,
}

impl fmt::Display for Pitch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.pitch_class, self.accidental)
    }
}

impl FromStr for Pitch {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (pitchclass_str, accidental_str) = s.split_at(1);
        let pitch_class = PitchClass::from_str(pitchclass_str)?;
        let accidental = Accidental::from_str(accidental_str)?;

        Ok(Pitch {
            pitch_class,
            accidental,
        })
    }
}
