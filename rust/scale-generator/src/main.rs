use scale_generator::pitch::Pitch;
use std::str::FromStr;

fn main() {
    let pitch = Pitch::from_str("Gbb").unwrap();
    println!("{}\n{:?}", pitch, pitch)
}
