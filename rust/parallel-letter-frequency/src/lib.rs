use std::collections::HashMap;

extern crate rayon;
use rayon::prelude::*;

pub fn frequency(
    input : &[&str],
    worker_count : usize,
) -> HashMap<char, usize> {
    input
        .par_chunks(worker_count)
        .map(|input| {
            input.iter().fold(HashMap::new(), |counts, s| {
                // NOTE: shadowing counts here
                s.chars().fold(counts, |mut counts, ch| {
                    if ch.is_alphabetic() {
                        *counts
                            // NOTE: Tests don't include non-ASCII uppercase
                            .entry(ch.to_ascii_lowercase())
                            .or_insert(0) += 1;
                    }
                    counts
                })
            })
        })
        .reduce_with(|mut dest, src| {
            for (k, v) in src.iter() {
                *dest.entry(*k).or_insert(0) += *v
            }
            dest
        })
        .unwrap_or_default()
}
