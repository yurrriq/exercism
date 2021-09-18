#[derive(Debug)]
pub enum CalculatorInput {
    Add,
    Subtract,
    Multiply,
    Divide,
    Value(i32),
}

pub fn evaluate(inputs: &[CalculatorInput]) -> Option<i32> {
    let mut stack: Vec<i32> = vec![];

    for input in inputs {
        match input {
            CalculatorInput::Add => {
                if let (Some(y), Some(x)) = (stack.pop(), stack.pop()) {
                    stack.push(x + y)
                } else {
                    return None;
                }
            }
            CalculatorInput::Subtract => {
                if let (Some(y), Some(x)) = (stack.pop(), stack.pop()) {
                    stack.push(x - y)
                } else {
                    return None;
                }
            }
            CalculatorInput::Multiply => {
                if let (Some(y), Some(x)) = (stack.pop(), stack.pop()) {
                    stack.push(x * y)
                } else {
                    return None;
                }
            }
            CalculatorInput::Divide => {
                if let (Some(y), Some(x)) = (stack.pop(), stack.pop()) {
                    stack.push(x / y)
                } else {
                    return None;
                }
            }
            CalculatorInput::Value(value) => stack.push(*value),
        }
    }

    if stack.len() == 1 {
        stack.pop()
    } else {
        None
    }
}
