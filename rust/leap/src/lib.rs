pub trait Divisible {
    fn divides(self, numerator: Self) -> bool;
}

impl Divisible for u64 {
    fn divides(self, numerator: Self) -> bool {
        numerator % self == 0
    }
}

pub fn is_leap_year(year: u64) -> bool {
    400.divides(year) || (4.divides(year) && !100.divides(year))
}
