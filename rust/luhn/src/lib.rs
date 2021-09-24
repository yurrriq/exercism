/// Check a Luhn checksum.
pub fn is_valid(code : &str) -> bool {
    let code = code.chars().filter(|&c| c != ' ');

    if code.clone().collect::<String>() == "0" {
        return false;
    }

    let mut sum = 0;

    for (i, c) in code.rev().enumerate() {
        if let Some(mut n) = c.to_digit(10) {
            if i % 2 != 0 {
                n *= 2;
                if n > 9 {
                    n -= 9;
                }
            }

            sum += n;
        } else {
            return false;
        }
    }

    sum % 10 == 0
}
