extern crate int_enum;

use int_enum::IntEnum;

// The code below is a stub. Just enough to satisfy the compiler.
// In order to pass the tests you can add-to or change any of this code.

#[repr(usize)]
#[derive(PartialEq, Eq, Debug, Clone, Copy, IntEnum)]
pub enum Direction {
    North = 0,
    East = 1,
    South = 2,
    West = 3,
}

pub struct Robot {
    x : i32,
    y : i32,
    d : Direction,
}

impl Robot {
    pub fn new(x : i32, y : i32, d : Direction) -> Self {
        Robot { x, y, d }
    }

    #[must_use]
    pub fn turn_right(mut self) -> Self {
        self.d = Direction::from_int((self.d.int_value() + 1) % 4).unwrap();
        self
    }

    #[must_use]
    pub fn turn_left(mut self) -> Self {
        self.d = Direction::from_int((self.d.int_value() + 3) % 4).unwrap();
        self
    }

    #[must_use]
    pub fn advance(mut self) -> Self {
        match self.d {
            Direction::North => self.y += 1,
            Direction::East => self.x += 1,
            Direction::South => self.y -= 1,
            Direction::West => self.x -= 1,
        };
        self
    }

    #[must_use]
    pub fn instructions(self, instructions : &str) -> Self {
        instructions.chars().fold(
            self,
            |robot, instruction| match instruction {
                'A' => robot.advance(),
                'L' => robot.turn_left(),
                'R' => robot.turn_right(),
                _ => unimplemented!("Unknown instruction"),
            },
        )
    }

    pub fn position(&self) -> (i32, i32) {
        (self.x, self.y)
    }

    pub fn direction(&self) -> &Direction {
        &self.d
    }
}
