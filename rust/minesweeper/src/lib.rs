use std::convert::TryFrom;

static ADJACENCIES: &[(i8, i8)] = &[
    (-1, -1),
    (0, -1),
    (1, -1),
    (-1, 0),
    // (0, 0),
    (1, 0),
    (-1, 1),
    (0, 1),
    (1, 1),
];

pub fn annotate(minefield: &[&str]) -> Vec<String> {
    if minefield.is_empty() {
        return vec![];
    }

    let height = i8::try_from(minefield.len()).expect("Too tall!");
    let width = i8::try_from(minefield[0].len()).expect("Too wide!");

    minefield
        .iter()
        .enumerate()
        .map(|(y, row)| {
            row.chars()
                .enumerate()
                .map(|(x, square)| match square {
                    '*' => '*',
                    _ => match ADJACENCIES
                        .iter()
                        .map(|&(dx, dy)| (x as i8 + dx, y as i8 + dy))
                        .filter(|&(x, y)| {
                            0 <= x && x < width && 0 <= y && y < height
                        })
                        .filter(|&(x, y)| {
                            minefield[y as usize].as_bytes()[x as usize] == b'*'
                        })
                        .count()
                    {
                        0 => ' ',
                        n => char::from_digit(n as u32, 10_u32)
                            .expect("Can't count the mines!"),
                    },
                })
                .collect()
        })
        .collect()
}
