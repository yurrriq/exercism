pub fn verse(n: u32) -> String {
    format!(
        "{} on the wall, {}.\n{}, {} on the wall.\n",
        bottles(n),
        bottles(n).to_lowercase(),
        pass_or_buy(n),
        bottles((100 + n - 1) % 100).to_lowercase()
    )
}

pub fn sing(start: u32, end: u32) -> String {
    (end..start + 1)
        .rev()
        .map(verse)
        .collect::<Vec<String>>()
        .join("\n")
}

fn bottles(n: u32) -> String {
    match n {
        0 => String::from("No more bottles of beer"),
        1 => String::from("1 bottle of beer"),
        n => format!("{} bottles of beer", n),
    }
}

fn pass_or_buy(n: u32) -> &'static str {
    match n {
        0 => "Go to the store and buy some more",
        1 => "Take it down and pass it around",
        _ => "Take one down and pass it around",
    }
}
