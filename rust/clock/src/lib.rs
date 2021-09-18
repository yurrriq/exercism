#[derive(Debug, PartialEq)]
pub struct Clock {
    hours: i32,
    minutes: i32,
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        Self::default().add_minutes(hours * 60 + minutes)
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        Self {
            hours: (self.hours + (self.minutes + minutes).div_euclid(60)).rem_euclid(24),
            minutes: (self.minutes + minutes).rem_euclid(60),
        }
    }
}

impl Default for Clock {
    fn default() -> Self {
        Self {
            hours: 0,
            minutes: 0,
        }
    }
}

impl ToString for Clock {
    fn to_string(&self) -> String {
        format!("{:02}:{:02}", self.hours, self.minutes)
    }
}
