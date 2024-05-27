#![feature(test)]
extern crate diffie_hellman;
extern crate test;

use test::Bencher;

#[bench]
fn bench_secret_key(b: &mut Bencher) {
    let p: u64 = 11;

    let private_key_a = 7;
    let public_key_b = 8;

    b.iter(|| diffie_hellman::secret(p, public_key_b, private_key_a));
}
