extern crate chrono;

use chrono::{DateTime, NaiveDateTime, Utc};

const GIGASECOND: i64 = 1_000_000_000;

/// Return a [`DateTime<Utc>`] one billion seconds after `start`.
pub fn after(start: DateTime<Utc>) -> DateTime<Utc> {
    DateTime::<Utc>::from_utc(
        NaiveDateTime::from_timestamp(start.timestamp() + GIGASECOND, 0),
        Utc,
    )
}
