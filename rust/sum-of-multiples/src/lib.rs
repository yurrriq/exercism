extern crate num;
use num::integer::Integer;

pub fn sum_of_multiples(limit : u32, factors : &[u32]) -> u32 {
    (1..limit)
        .filter(|n| factors.iter().any(|d| n.is_multiple_of(d)))
        .sum()
}
