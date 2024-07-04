/// Find the `n`th prime.
pub fn nth(n: u32) -> u32 {
    let n = n as usize;
    let upper_bound = rosser_schoenfeld_3_13(n + 1);
    primes(upper_bound).nth(n).unwrap() as u32
}

/// A prime iterator not exceeding `upper_bound`.
fn primes(upper_bound: usize) -> impl Iterator<Item = usize> {
    const TWO: usize = 2;
    if upper_bound < TWO {
        Vec::new()
    } else {
        let mut is_prime = vec![true; upper_bound - 1];
        for candidate in TWO..((upper_bound as f64).sqrt() as usize + 1) {
            let mut multiples =
                is_prime[candidate - TWO..].iter_mut().step_by(candidate);
            if let Some(true) = multiples.next() {
                multiples.for_each(|multiple| *multiple = false);
            }
        }
        is_prime
    }
    .into_iter()
    .enumerate()
    .filter_map(|(e, b)| if b { Some(e + TWO) } else { None })
}

/// Corollary 3.13 from [Rosser and Schoenfeld 1962], i.e., the upper limit of
/// the `n`th prime.
///
/// J. Barkley Rosser, Lowell Schoenfeld "Approximate formulas for some
/// functions of prime numbers," Illinois Journal of Mathematics, Illinois
/// J. Math. 6(1), 64-94, (March 1962)
///
/// [Rosser and Schoenfeld 1962]: https://projecteuclid.org/euclid.ijm/1255631807
fn rosser_schoenfeld_3_13(n: usize) -> usize {
    if 6 <= n {
        let n = n as f64;
        (n * (n.ln() + n.ln().ln())) as usize
    } else {
        12
    }
}
