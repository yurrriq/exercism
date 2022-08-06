extern crate int_enum;
use int_enum::IntEnum;

extern crate enum_iterator;
use enum_iterator::IntoEnumIterator;

pub struct Allergies {
    score: u32,
}

#[repr(u32)]
#[derive(Clone, Copy, Debug, PartialEq, IntEnum, IntoEnumIterator)]
pub enum Allergen {
    Eggs = 1,
    Peanuts = 2,
    Shellfish = 4,
    Strawberries = 8,
    Tomatoes = 16,
    Chocolate = 32,
    Pollen = 64,
    Cats = 128,
}

impl Allergies {
    pub fn new(score: u32) -> Self {
        Allergies { score }
    }

    pub fn is_allergic_to(&self, allergen: &Allergen) -> bool {
        test_bit(self.score, *allergen as u32)
    }

    pub fn allergies(&self) -> Vec<Allergen> {
        Allergen::into_enum_iter()
            .filter(|allergen| self.is_allergic_to(allergen))
            .collect()
    }
}

fn test_bit(n: u32, k: u32) -> bool {
    n & k != 0
}
