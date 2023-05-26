use std::collections::HashMap;

use itertools::Itertools;

pub fn count(nucleotide: char, dna: &str) -> Result<usize, char> {
    validate_nucleotide(nucleotide).and(
        dna.chars()
            .map(validate_nucleotide)
            .collect::<Result<Vec<char>, _>>()
            .map(|xs| xs.into_iter().filter(|&x| x == nucleotide).count()),
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
    "ACGT".contains(x).then_some(x).ok_or(x)
}
