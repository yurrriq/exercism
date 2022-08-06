pub fn is_armstrong_number(num: u32) -> bool {
    let string = num.to_string();
    let k = string.len() as u32;
    let sum = string.chars().fold(0, |sum, character| {
        sum + character.to_digit(10).expect("Invalid digit!").pow(k)
    });
    sum == num
}
