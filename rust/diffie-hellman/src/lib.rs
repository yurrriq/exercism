use rand::Rng;

pub fn private_key(p: u64) -> u64 {
    let mut rng = rand::thread_rng();
    rng.gen_range(2..p)
}

pub fn public_key(p: u64, g: u64, a: u64) -> u64 {
    pow_mod(g, a, p)
}

pub fn secret(p: u64, b_pub: u64, a: u64) -> u64 {
    pow_mod(b_pub, a, p)
}

fn pow_mod(base: u64, exponent: u64, modulus: u64) -> u64 {
    let mut result = 1;
    let mut base = base % modulus;
    let mut exponent = exponent;

    while exponent > 0 {
        if exponent & 1 == 1 {
            result *= base;
            result %= modulus;
        }

        exponent >>= 1;
        base *= base;
        base %= modulus;
    }

    result
}
