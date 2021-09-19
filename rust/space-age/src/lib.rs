#[derive(Debug)]
pub struct Duration(f64);

const EARTH_YEAR: f64 = 31_557_600_f64;

impl From<u64> for Duration {
    fn from(seconds: u64) -> Self {
        Self(seconds as f64)
    }
}

pub trait Planet {
    const ORBITAL_PERIOD: f64 = 1_f64;

    fn years_during(duration: &Duration) -> f64 {
        duration.0 / Self::ORBITAL_PERIOD / EARTH_YEAR
    }
}

pub struct Mercury;
impl Planet for Mercury {
    const ORBITAL_PERIOD: f64 = 0.2408467;
}

pub struct Venus;
impl Planet for Venus {
    const ORBITAL_PERIOD: f64 = 0.61519726;
}

pub struct Earth;
impl Planet for Earth {}

pub struct Mars;
impl Planet for Mars {
    const ORBITAL_PERIOD: f64 = 1.8808158;
}

pub struct Jupiter;
impl Planet for Jupiter {
    const ORBITAL_PERIOD: f64 = 11.862615;
}

pub struct Saturn;
impl Planet for Saturn {
    const ORBITAL_PERIOD: f64 = 29.447498;
}

pub struct Uranus;
impl Planet for Uranus {
    const ORBITAL_PERIOD: f64 = 84.016846;
}

pub struct Neptune;
impl Planet for Neptune {
    const ORBITAL_PERIOD: f64 = 164.79132;
}
