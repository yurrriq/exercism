extern crate int_enum;

use enum_iterator::IntoEnumIterator;
use int_enum::IntEnum;

use std::fmt;
use std::str::FromStr;

use crate::error::Error;

#[repr(usize)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, IntEnum, IntoEnumIterator)]
pub enum PitchClass {
    C = 0,
    D = 1,
    E = 2,
    F = 3,
    G = 4,
    A = 5,
    B = 6,
}

impl fmt::Display for PitchClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl FromStr for PitchClass {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pc = match s {
            "C" => PitchClass::C,
            "D" => PitchClass::D,
            "E" => PitchClass::E,
            "F" => PitchClass::F,
            "G" => PitchClass::G,
            "A" => PitchClass::A,
            "B" => PitchClass::B,
            _ => return Err(Error::ParsePitchClassError),
        };

        Ok(pc)
    }
}
