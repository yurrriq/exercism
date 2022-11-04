extern crate int_enum;

use enum_iterator::IntoEnumIterator;
use int_enum::IntEnum;

use std::fmt;
use std::str::FromStr;

use crate::error::Error;

#[repr(isize)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, IntEnum, IntoEnumIterator)]
pub enum Accidental {
    DoubleFlat = -2,
    Flat = -1,
    Natural = 0,
    Sharp = 1,
    DoubleSharp = 2,
}

impl fmt::Display for Accidental {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Accidental::DoubleFlat => "bb",
            Accidental::Flat => "b",
            Accidental::Natural => "",
            Accidental::Sharp => "#",
            Accidental::DoubleSharp => "##",
        };
        write!(f, "{}", s)
    }
}

impl FromStr for Accidental {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let accidental = match s {
            "b" | "es" => Accidental::Flat,
            "bb" | "eses" => Accidental::DoubleFlat,
            "#" | "is" => Accidental::Sharp,
            "##" | "isis" => Accidental::DoubleSharp,
            "" => Accidental::Natural,
            _ => return Err(Error::ParseAccidentalError),
        };

        Ok(accidental)
    }
}
