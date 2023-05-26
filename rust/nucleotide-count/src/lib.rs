use std::collections::HashMap;

use itertools::Itertools;

pub fn count(nucleotide: char, dna: &str) -> Result<usize, char> {
    validate_nucleotide(nucleotide).and(
        dna.chars().map(validate_nucleotide).fold_ok(0, |acc, x| {
            if x == nucleotide {
                acc + 1
            } else {
                acc
            }
        }),
    )
}

pub fn nucleotide_counts(dna: &str) -> Result<HashMap<char, usize>, char> {
    dna.chars().map(validate_nucleotide).fold_ok(
        HashMap::from([('A', 0), ('C', 0), ('G', 0), ('T', 0)]),
        |mut acc, x| {
            acc.entry(x).and_modify(|counter| *counter += 1);
            acc
        },
    )
}

fn validate_nucleotide(x: char) -> Result<char, char> {
    ['A', 'C', 'G', 'T'].contains(&x).then_some(x).ok_or(x)
}
