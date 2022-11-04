use crate::error::Error;
use crate::pitch_class::PitchClass;
use crate::accidental::Accidental;

use std::fmt;
use std::str::FromStr;

#[derive(Debug, Eq, PartialEq)]
pub struct Pitch {
    pitch_class: PitchClass,
    accidental: Accidental,
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

        Ok(Pitch{pitch_class, accidental})
    }
}
