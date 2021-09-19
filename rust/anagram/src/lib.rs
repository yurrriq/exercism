use std::collections::HashSet;

pub fn anagrams_for<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    let normalized_word = normalize(word);
    let lowercased_word = word.to_lowercase();

    possible_anagrams
        .iter()
        .fold(HashSet::new(), |mut anagrams, candidate| {
            if candidate.to_lowercase().ne(&lowercased_word)
                && normalize(candidate).eq(&normalized_word)
            {
                anagrams.insert(candidate);
            }
            anagrams
        })
}

fn normalize(word: &str) -> String {
    let mut normalized_word: Vec<char> = word.to_lowercase().chars().collect();
    normalized_word.sort_unstable();
    normalized_word.into_iter().collect()
}
