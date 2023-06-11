use std::iter::once;

const SEPARATORS: &str = " -_";

pub fn abbreviate(phrase: &str) -> String {
    once(' ')
        .chain(phrase.chars())
        .collect::<Vec<char>>()
        .windows(2)
        .filter_map(|window| match *window {
            [_, next_char] if !next_char.is_alphabetic() => None,
            [prev_char, next_char]
                if SEPARATORS.contains(prev_char)
                    || (prev_char.is_lowercase()
                        && next_char.is_uppercase()) =>
            {
                Some(next_char)
            },
            _ => None,
        })
        .collect::<String>()
        .to_uppercase()
}
