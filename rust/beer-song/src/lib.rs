pub fn verse(n: u32) -> String {
    format!(
        "{}, {}.\n{}.\n",
        match n {
            0 => on_the_wall(&of_beer("No more bottles")),
            n => on_the_wall(&of_beer(&bottles(n))),
        },
        of_beer(&bottles(n)),
        match n {
            0 => {
                format!(
                    "Go to the store and buy some more, {}",
                    on_the_wall(&of_beer(&bottles(99)))
                )
            }
            1 => {
                format!(
                    "Take it down and pass it around, {}",
                    on_the_wall(&of_beer(&bottles(n - 1)))
                )
            }
            n => {
                format!(
                    "Take one down and pass it around, {}",
                    on_the_wall(&of_beer(&bottles(n - 1)))
                )
            }
        }
    )
}

pub fn sing(start: u32, end: u32) -> String {
    (end..start + 1)
        .rev()
        .map(&verse)
        .collect::<Vec<String>>()
        .join("\n")
}

fn bottles(n: u32) -> String {
    match n {
        0 => String::from("no more bottles"),
        1 => String::from("1 bottle"),
        n => format!("{} bottles", n),
    }
}

fn of_beer(suffix: &str) -> String {
    format!("{} of beer", suffix)
}

fn on_the_wall(prefix: &str) -> String {
    format!("{} on the wall", prefix)
}
