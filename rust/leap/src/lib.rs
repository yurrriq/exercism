extern crate num_traits;

use num_traits::Zero;

use std::ops::Rem;

pub trait Divisible: Rem + Sized {
    fn divides(self, numerator: Self) -> bool
    where
        <Self as Rem>::Output: Zero,
    {
        (numerator % self).is_zero()
    }
}

impl Divisible for u64 {}

pub fn is_leap_year(year: u64) -> bool {
    400.divides(year) || (4.divides(year) && !100.divides(year))
}
