use scale_generator::pitch::Pitch;
use scale_generator::scale::Scale;
use std::str::FromStr;

fn main() {
    let tonic = Pitch::from_str("C").unwrap();
    println!("{}\n{:?}", tonic, tonic);

    let scale = Scale::new("C", "").expect("scale");
    println!("{:?}", scale);
    println!("{:?}", scale.into_iter().collect::<Vec<Pitch>>());
}
