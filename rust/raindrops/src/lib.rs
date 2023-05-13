use phf::phf_ordered_map;

static RAINDROPS: phf::OrderedMap<u32, &'static str> = phf_ordered_map! {
    3_u32 => "Pling",
    5_u32 => "Plang",
    7_u32 => "Plong",
};

pub fn raindrops(number: u32) -> String {
    let mut result = String::from("");

    for (divisor, word) in RAINDROPS.entries() {
        if number % divisor == 0 {
            result.push_str(word);
        }
    }

    if result.is_empty() {
        result = number.to_string();
    }

    result
}
