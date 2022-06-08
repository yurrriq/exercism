extern crate int_enum;

use std::fmt;

use enum_iterator::IntoEnumIterator;
use int_enum::IntEnum;

#[repr(usize)]
#[derive(Clone, Copy, Debug, Eq, PartialEq, IntEnum, IntoEnumIterator)]
pub enum ResistorColor {
    Black = 0,
    Brown = 1,
    Red = 2,
    Orange = 3,
    Yellow = 4,
    Green = 5,
    Blue = 6,
    Violet = 7,
    Grey = 8,
    White = 9,
}

impl fmt::Display for ResistorColor {
    fn fmt(&self, f : &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", format!("{:?}", self))
    }
}

pub fn color_to_value(color : ResistorColor) -> usize {
    IntEnum::int_value(color)
}

pub fn value_to_color_string(value : usize) -> String {
    ResistorColor::from_int(value)
        .map_or("value out of range".to_string(), |r| r.to_string())
}

pub fn colors() -> Vec<ResistorColor> {
    ResistorColor::into_enum_iter().collect()
}
