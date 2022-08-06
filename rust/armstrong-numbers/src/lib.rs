pub fn is_armstrong_number(num: u32) -> bool {
    is_armstrong_number_base(num, 10)
}

fn is_armstrong_number_base(num: u32, base: u32) -> bool {
    if num == 0 {
        return true
    }

    let string = num.to_string();
    let k = (num as f32).log(base as f32).floor() as u32 + 1;
    let sum = string.chars().fold(0, |sum, character| {
        sum + character.to_digit(base).expect("Invalid digit!").pow(k)
    });

    sum == num
}
