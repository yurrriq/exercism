pub fn factors(n: u64) -> Vec<u64> {
    let mut n = n;
    let mut factors = Vec::new();
    let mut divisors = (3..).step_by(2);
    let mut d = 2;
    while n != 1 {
        while n % d == 0 {
            n /= d;
            factors.push(d);
        }

        d = divisors.next().unwrap();
    }

    factors
}
