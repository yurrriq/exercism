fn process_rate_per_hour(speed: u8, expected_rate: f64) {
    assert!(
        (assembly_line::production_rate_per_hour(speed) - expected_rate).abs()
            < f64::EPSILON
    );
}

fn process_rate_per_minute(speed: u8, expected_rate: u32) {
    assert_eq!(
        assembly_line::working_items_per_minute(speed),
        expected_rate
    );
}

#[test]
fn production_rate_per_hour_at_speed_zero() {
    process_rate_per_hour(0, 0.0);
}

#[test]
fn production_rate_per_hour_at_speed_one() {
    process_rate_per_hour(1, 221.0);
}

#[test]
fn production_rate_per_hour_at_speed_four() {
    process_rate_per_hour(4, 884.0);
}

#[test]
fn production_rate_per_hour_at_speed_seven() {
    process_rate_per_hour(7, 1392.3);
}

#[test]
fn production_rate_per_hour_at_speed_nine() {
    process_rate_per_hour(9, 1531.53);
}

#[test]
fn production_rate_per_minute_at_speed_zero() {
    process_rate_per_minute(0, 0);
}

#[test]
fn production_rate_per_minute_at_speed_one() {
    process_rate_per_minute(1, 3);
}

#[test]
fn production_rate_per_minute_at_speed_five() {
    process_rate_per_minute(5, 16);
}

#[test]
fn production_rate_per_minute_at_speed_eight() {
    process_rate_per_minute(8, 26);
}

#[test]
fn production_rate_per_minute_at_speed_ten() {
    process_rate_per_minute(10, 28);
}
