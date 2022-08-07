pub fn square(s: u32) -> u64 {
    assert!((1..=64).contains(&s), "Square must be between 1 and 64");

    match s {
        1 => 1,
        s => 2_u64 * square(s - 1),
    }
}

pub fn total() -> u64 {
    (1..=64).map(square).sum()
}
