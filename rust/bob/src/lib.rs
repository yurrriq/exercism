pub fn reply(message : &str) -> &str {
    let message_trimmed = message.trim();

    if message_trimmed == "" {
        return "Fine. Be that way!";
    }

    let is_yelled = message_trimmed.chars().any(|c| c.is_alphabetic())
        && message_trimmed == message_trimmed.to_uppercase();

    if message_trimmed.ends_with("?") {
        if is_yelled {
            return "Calm down, I know what I'm doing!";
        }

        return "Sure.";
    }

    if is_yelled {
        return "Whoa, chill out!";
    }

    return "Whatever.";
}
