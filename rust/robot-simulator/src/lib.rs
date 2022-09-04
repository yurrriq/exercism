extern crate int_enum;

use int_enum::IntEnum;

#[repr(usize)]
#[derive(PartialEq, Eq, Debug, Clone, Copy, IntEnum)]
pub enum Direction {
    North = 0,
    East = 1,
    South = 2,
    West = 3,
}

pub struct Robot {
    x: i32,
    y: i32,
    direction: Direction,
}

impl Robot {
    pub fn new(x: i32, y: i32, direction: Direction) -> Self {
        Robot { x, y, direction }
    }

    pub fn turn_right(self) -> Self {
        Self {
            direction: Direction::from_int((self.direction.int_value() + 1) % 4).unwrap(),
            ..self
        }
    }

    pub fn turn_left(self) -> Self {
        Self {
            direction: Direction::from_int((self.direction.int_value() + 3) % 4).unwrap(),
            ..self
        }
    }

    pub fn advance(self) -> Self {
        use Direction::*;
        match self.direction {
            North => Self {
                y: self.y + 1,
                ..self
            },
            East => Self {
                x: self.x + 1,
                ..self
            },
            South => Self {
                y: self.y - 1,
                ..self
            },
            West => Self {
                x: self.x - 1,
                ..self
            },
        }
    }

    pub fn instructions(self, instructions: &str) -> Self {
        instructions
            .chars()
            .fold(self, |robot, instruction| match instruction {
                'A' => robot.advance(),
                'L' => robot.turn_left(),
                'R' => robot.turn_right(),
                _ => unimplemented!("Unknown instruction"),
            })
    }

    pub fn position(&self) -> (i32, i32) {
        (self.x, self.y)
    }

    pub fn direction(&self) -> &Direction {
        &self.direction
    }
}
