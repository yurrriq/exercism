#[derive(Debug)]
pub enum Category {
    Ones,
    Twos,
    Threes,
    Fours,
    Fives,
    Sixes,
    FullHouse,
    FourOfAKind,
    LittleStraight,
    BigStraight,
    Choice,
    Yacht,
}

type Dice = [u8; 5];

pub fn score(dice: Dice, category: Category) -> u8 {
    let mut dice = dice;
    match category {
        Category::Ones => pips(1, dice),
        Category::Twos => pips(2, dice),
        Category::Threes => pips(3, dice),
        Category::Fours => pips(4, dice),
        Category::Fives => pips(5, dice),
        Category::Sixes => pips(6, dice),
        Category::FullHouse => {
            dice.sort();
            match dice {
                [a, b, c, d, e]
                    if a == b && d == e && a != e && (b == c || c == d) =>
                {
                    dice.into_iter().sum()
                },
                _ => 0,
            }
        },
        Category::FourOfAKind => {
            dice.sort();
            match dice {
                [a, b, _, d, e] if a == d || b == e => 4 * d,
                _ => 0,
            }
        },
        Category::LittleStraight => {
            dice.sort();
            if dice == [1, 2, 3, 4, 5] {
                30
            } else {
                0
            }
        },
        Category::BigStraight => {
            dice.sort();
            if dice == [2, 3, 4, 5, 6] {
                30
            } else {
                0
            }
        },
        Category::Choice => dice.into_iter().sum(),
        Category::Yacht => {
            let mut dice_iter = dice.into_iter();
            match dice.first() {
                None => 0,
                Some(&first_die) => {
                    if dice_iter.all(|die| die == first_die) {
                        50
                    } else {
                        0
                    }
                },
            }
        },
    }
}

fn pips(n: u8, dice: Dice) -> u8 {
    dice.into_iter().filter(|&d| d == n).sum()
}
