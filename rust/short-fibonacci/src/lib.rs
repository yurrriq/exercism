use std::vec;

/// Create an empty vector
pub fn create_empty() -> Vec<u8> {
    vec![]
}

/// Create a buffer of `count` zeroes.
///
/// Applications often use buffers when serializing data to send over the network.
pub fn create_buffer(count: usize) -> Vec<u8> {
    vec![0; count]
}

/// Create a vector containing the first five elements of the Fibonacci sequence.
///
/// Fibonacci's sequence is the list of numbers where the next number is a sum of the previous two.
/// Its first five elements are `1, 1, 2, 3, 5`.
pub fn fibonacci() -> Vec<u8> {
    Fib::new().take(5).collect()
}

/// A structure to compute the Fibonacci sequence iteratively.
///
/// ```rust
/// # use short_fibonacci::Fib;
/// let fibs: Vec<u8> = Fib::new().take(5).collect();
/// assert_eq!(fibs, vec![1, 1, 2, 3, 5]);
/// ```
pub struct Fib(u8, u8);

impl Fib {
    pub fn new() -> Self {
        Fib(0, 1)
    }
}

impl Default for Fib {
    fn default() -> Self {
        Self::new()
    }
}

impl Iterator for Fib {
    type Item = u8;
    fn next(&mut self) -> Option<Self::Item> {
        *self = Fib(self.1, self.0 + self.1);
        Some(self.0)
    }
}
