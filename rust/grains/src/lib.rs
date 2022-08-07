const MAX_SQUARE: u32 = 64;
const ALL_SQUARES: std::ops::RangeInclusive<u32> = 1..=MAX_SQUARE;

pub fn square(s: u32) -> u64 {
    assert!(ALL_SQUARES.contains(&s), "Square must be between 1 and {}", MAX_SQUARE);
    1 << (s - 1)
}

pub fn total() -> u64 {
    ALL_SQUARES.map(square).sum()
}
