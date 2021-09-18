#[derive(Debug, PartialEq)]
pub struct Clock {
    hours: i32,
    minutes: i32,
}

impl ToString for Clock {
    fn to_string(&self) -> String {
        format!("{:02}:{:02}", self.hours, self.minutes)
    }
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        Self {
            hours: 0,
            minutes: 0,
        }
        .add_minutes(hours * 60 + minutes)
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        let new_time = self.hours * 60 + self.minutes + minutes;

        let mut new_hours = (new_time / 60) % 24;
        let mut new_minutes = new_time % 60;

        while new_minutes < 0 {
            new_hours -= 1;
            new_minutes += 60;
        }

        while new_hours < 0 {
            new_hours += 24;
        }

        Self {
            hours: new_hours,
            minutes: new_minutes,
        }
    }
}
