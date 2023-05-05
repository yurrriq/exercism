use std::collections::HashSet;

pub fn anagrams_for<'a>(
    word: &str,
    possible_anagrams: &[&'a str],
) -> HashSet<&'a str> {
    let lowercased_word = word.to_lowercase();
    let normalized_word = normalize(&lowercased_word);

    possible_anagrams
        .iter()
        .fold(HashSet::new(), |mut anagrams, candidate| {
            if is_anagram(candidate, &lowercased_word, &normalized_word) {
                anagrams.insert(candidate);
            }
            anagrams
        })
}

fn is_anagram(
    candidate: &str,
    lowercased_word: &str,
    normalized_word: &str,
) -> bool {
    let lowercased_candidate = candidate.to_lowercase();
    lowercased_candidate.ne(&lowercased_word)
        && normalize(&lowercased_candidate).eq(&normalized_word)
}

fn normalize(lowercased_word: &str) -> String {
    let mut normalized_word: Vec<char> = lowercased_word.chars().collect();
    normalized_word.sort_unstable();
    normalized_word.into_iter().collect()
}
