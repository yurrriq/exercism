use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub fn can_construct_note(magazine: &[&str], note: &[&str]) -> bool {
    let mut available_words = HashMap::new();
    for word in magazine.iter() {
        *available_words.entry(word).or_insert(0) += 1
    }

    for word in note.iter() {
        match available_words.entry(word) {
            Entry::Occupied(o) if *o.get() > 1 => {
                *o.into_mut() -= 1;
            }
            Entry::Occupied(o) => {
                o.remove_entry();
            }
            _ => return false,
        }
    }
    true
}
