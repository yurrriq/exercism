/// Check a Luhn checksum.
pub fn is_valid(code : &str) -> bool {
    code.chars()
        .rev()
        .filter(|character| !character.is_whitespace())
        .try_fold((0, 0), |(index, sum), character| {
            character
                .to_digit(10)
                .map(|digit| if index % 2 == 1 { digit * 2 } else { digit })
                .map(|digit| if digit > 9 { digit - 9 } else { digit })
                .map(|digit| (index + 1, sum + digit))
        })
        .map_or(false, |(length, sum)| length > 1 && sum % 10 == 0)
}
