use crate::Comparison::*;
use std::cmp::Ordering;

#[derive(Debug, PartialEq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

pub fn sublist<T: PartialEq>(needle: &[T], haystack: &[T]) -> Comparison {
    match needle.len().cmp(&haystack.len()) {
        Ordering::Less if is_infix_of(needle, haystack) => Sublist,
        Ordering::Equal if needle == haystack => Equal,
        Ordering::Greater if is_infix_of(haystack, needle) => Superlist,
        _ => Unequal,
    }
}

// Basically Haskell's Data.List.isInfixOf
fn is_infix_of<T: PartialEq>(needle: &[T], haystack: &[T]) -> bool {
    needle.is_empty()
        || haystack
            .windows(needle.len())
            .any(|window| window == needle)
}
