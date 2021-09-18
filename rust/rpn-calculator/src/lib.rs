use std::ops::{Add, Div, Mul, Sub};

#[derive(Debug)]
pub enum CalculatorInput {
    Add,
    Subtract,
    Multiply,
    Divide,
    Value(i32),
}

pub fn evaluate(inputs: &[CalculatorInput]) -> Option<i32> {
    inputs
        .iter()
        .try_fold(vec![], |mut stack, input| {
            match *input {
                CalculatorInput::Add => binop(i32::add, &mut stack),
                CalculatorInput::Subtract => binop(i32::sub, &mut stack),
                CalculatorInput::Multiply => binop(i32::mul, &mut stack),
                CalculatorInput::Divide => binop(i32::div, &mut stack),
                CalculatorInput::Value(value) => Some(value),
            }
            .map(|result| {
                stack.push(result);
                stack
            })
        })
        .and_then(|mut stack| match stack.pop() {
            Some(result) if stack.is_empty() => Some(result),
            _ => None,
        })
}

fn binop(f: impl Fn(i32, i32) -> i32, stack: &mut Vec<i32>) -> Option<i32> {
    stack.pop().and_then(|y| stack.pop().map(|x| f(x, y)))
}
