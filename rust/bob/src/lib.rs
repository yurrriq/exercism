pub fn reply(message: &str) -> &str {
    match message.trim() {
        "" => "Fine. Be that way!",
        m if is_forceful_question(m) => "Calm down, I know what I'm doing!",
        m if is_question(m) => "Sure.",
        m if is_yelled(m) => "Whoa, chill out!",
        _ => "Whatever.",
    }
}

fn is_forceful_question(message: &str) -> bool {
    is_question(message) && is_yelled(message)
}

fn is_question(message: &str) -> bool {
    message.ends_with("?")
}

fn is_yelled(message: &str) -> bool {
    message.chars().any(|c| c.is_alphabetic())
        && message == message.to_uppercase()
}
