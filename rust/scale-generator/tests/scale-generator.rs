//! Tests for scale-generator
//!
//! Generated by [script][script] using [canonical data][canonical-data]
//!
//! [script]: https://github.com/exercism/rust/blob/main/bin/init_exercise.py
//! [canonical-data]: https://raw.githubusercontent.com/exercism/problem-specifications/main/exercises/scale-generator/canonical_data.json

use scale_generator::*;

/// Process a single test case for the property `chromatic`
///
/// All cases for the `chromatic` property are implemented
/// in terms of this function.
fn process_chromatic_case(tonic: &str, expected: &[&str]) {
    let s = Scale::chromatic(tonic).unwrap();
    assert_eq!(s.enumerate(), expected);
}

/// Process a single test case for the property `interval`
///
/// All cases for the `interval` property are implemented
/// in terms of this function.
fn process_interval_case(tonic: &str, intervals: &str, expected: &[&str]) {
    let s = Scale::new(tonic, intervals).unwrap();
    assert_eq!(s.enumerate(), expected);
}

// Chromatic scales
// These tests have no interval.
// The chromatic scale is considered the default scale

#[test]
/// Chromatic scale with sharps
fn test_chromatic_scale_with_sharps() {
    process_chromatic_case(
        "C",
        &[
            "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B",
            "C",
        ],
    );
}

#[test]
#[ignore]
/// Chromatic scale with flats
fn test_chromatic_scale_with_flats() {
    process_chromatic_case(
        "F",
        &[
            "F", "Gb", "G", "Ab", "A", "Bb", "B", "C", "Db", "D", "Eb", "E",
            "F",
        ],
    );
}

// Scales with specified intervals
// These tests all have intervals and are explorations of different
// traversals of the scale.

#[test]
#[ignore]
/// Simple major scale
///
/// The simplest major scale, with no sharps or flats.
fn test_simple_major_scale() {
    process_interval_case(
        "C",
        "MMmMMMm",
        &["C", "D", "E", "F", "G", "A", "B", "C"],
    );
}

#[test]
#[ignore]
/// Major scale with sharps
fn test_major_scale_with_sharps() {
    process_interval_case(
        "G",
        "MMmMMMm",
        &["G", "A", "B", "C", "D", "E", "F#", "G"],
    );
}

#[test]
#[ignore]
/// Major scale with flats
fn test_major_scale_with_flats() {
    process_interval_case(
        "F",
        "MMmMMMm",
        &["F", "G", "A", "Bb", "C", "D", "E", "F"],
    );
}

#[test]
#[ignore]
/// Minor scale with sharps
fn test_minor_scale_with_sharps() {
    process_interval_case(
        "f#",
        "MmMMmMM",
        &["F#", "G#", "A", "B", "C#", "D", "E", "F#"],
    );
}

#[test]
#[ignore]
/// Minor scale with flats
fn test_minor_scale_with_flats() {
    process_interval_case(
        "bb",
        "MmMMmMM",
        &["Bb", "C", "Db", "Eb", "F", "Gb", "Ab", "Bb"],
    );
}

#[test]
#[ignore]
/// Dorian mode
fn test_dorian_mode() {
    process_interval_case(
        "d",
        "MmMMMmM",
        &["D", "E", "F", "G", "A", "B", "C", "D"],
    );
}

#[test]
#[ignore]
/// Mixolydian mode
fn test_mixolydian_mode() {
    process_interval_case(
        "Eb",
        "MMmMMmM",
        &["Eb", "F", "G", "Ab", "Bb", "C", "Db", "Eb"],
    );
}

#[test]
#[ignore]
/// Lydian mode
fn test_lydian_mode() {
    process_interval_case(
        "a",
        "MMMmMMm",
        &["A", "B", "C#", "D#", "E", "F#", "G#", "A"],
    );
}

#[test]
#[ignore]
/// Phrygian mode
fn test_phrygian_mode() {
    process_interval_case(
        "e",
        "mMMMmMM",
        &["E", "F", "G", "A", "B", "C", "D", "E"],
    );
}

#[test]
#[ignore]
/// Locrian mode
fn test_locrian_mode() {
    process_interval_case(
        "g",
        "mMMmMMM",
        &["G", "Ab", "Bb", "C", "Db", "Eb", "F", "G"],
    );
}

#[test]
#[ignore]
/// Harmonic minor
///
/// Note that this case introduces the augmented second interval (A)
fn test_harmonic_minor() {
    process_interval_case(
        "d",
        "MmMMmAm",
        &["D", "E", "F", "G", "A", "Bb", "Db", "D"],
    );
}

#[test]
#[ignore]
/// Octatonic
fn test_octatonic() {
    process_interval_case(
        "C",
        "MmMmMmMm",
        &["C", "D", "D#", "F", "F#", "G#", "A", "B", "C"],
    );
}

#[test]
#[ignore]
/// Hexatonic
fn test_hexatonic() {
    process_interval_case(
        "Db",
        "MMMMMM",
        &["Db", "Eb", "F", "G", "A", "B", "Db"],
    );
}

#[test]
#[ignore]
/// Pentatonic
fn test_pentatonic() {
    process_interval_case("A", "MMAMA", &["A", "B", "C#", "E", "F#", "A"]);
}

#[test]
#[ignore]
/// Enigmatic
fn test_enigmatic() {
    process_interval_case(
        "G",
        "mAMMMmm",
        &["G", "G#", "B", "C#", "D#", "F", "F#", "G"],
    );
}
