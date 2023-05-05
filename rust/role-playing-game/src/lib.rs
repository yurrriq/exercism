use std::cmp::min;

pub struct Player {
    pub health: u32,
    pub mana: Option<u32>,
    pub level: u32,
}

impl Player {
    pub fn revive(&self) -> Option<Player> {
        match *self {
            Player {
                health: 0, level, ..
            } if level >= 10 => Some(Player {
                health: 100,
                mana: Some(100),
                ..*self
            }),
            Player { health: 0, .. } => Some(Player {
                health: 100,
                mana: None,
                ..*self
            }),
            _ => None,
        }
    }

    pub fn cast_spell(&mut self, mana_cost: u32) -> u32 {
        match self.mana {
            Some(mana) if mana >= mana_cost => {
                self.mana = Some(mana - mana_cost);
                2 * mana_cost
            },
            Some(_) => 0,
            None => {
                self.health -= min(self.health, mana_cost);
                0
            },
        }
    }
}
