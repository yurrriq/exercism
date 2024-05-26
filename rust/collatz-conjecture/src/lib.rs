pub fn collatz(n: u64) -> Option<u64> {
    let mut n = n;
    for steps in 0.. {
        match n {
            0 => return None,
            1 => return Some(steps),
            _ if 0 == n & 1 => n = n.checked_div(2)?,
            _ => n = n.checked_mul(3)?.checked_add(1)?,
        }
    }

    None
}
