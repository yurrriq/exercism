use std::fmt::{Display, Formatter, Result};

static ARABIC_TO_ROMAN: [(usize, &str); 13] = [
    (1000, "M"),
    (900, "CM"),
    (500, "D"),
    (400, "CD"),
    (100, "C"),
    (90, "XC"),
    (50, "L"),
    (40, "XL"),
    (10, "X"),
    (9, "IX"),
    (5, "V"),
    (4, "IV"),
    (1, "I"),
];

pub struct Roman(usize);

impl Display for Roman {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let mut to_convert = self.0;
        let mut to_display = String::new();

        for (arabic, roman) in ARABIC_TO_ROMAN {
            if arabic <= to_convert {
                to_display.push_str(&roman.repeat(to_convert / arabic));
                to_convert %= arabic;
            }
        }

        write!(f, "{}", to_display)
    }
}

impl From<usize> for Roman {
    fn from(num: usize) -> Self {
        Self(num)
    }
}
