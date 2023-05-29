use std::iter;

pub fn factors(n: u64) -> Vec<u64> {
    let mut n = n;
    let mut factors = Vec::new();

    let mut divisors = iter::once(2).chain((3..).step_by(2));
    while n != 1 {
        let d = divisors.next().unwrap();
        while n % d == 0 {
            n /= d;
            factors.push(d);
        }
    }

    factors
}
