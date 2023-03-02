import gleam/string

fn is_forceful_question(message: String) -> Bool {
  is_question(message) && is_yelled(message)
}

fn is_question(message: String) -> Bool {
  string.ends_with(message, "?")
}

fn is_yelled(message: String) -> Bool {
  message == string.uppercase(message) && message != string.lowercase(message)
}

pub fn hey(remark: String) -> String {
  let trimmed = string.trim(remark)
  case "" == trimmed {
    True -> "Fine. Be that way!"
    False ->
      case is_forceful_question(trimmed) {
        True -> "Calm down, I know what I'm doing!"
        False ->
          case is_question(trimmed) {
            True -> "Sure."
            False ->
              case is_yelled(trimmed) {
                True -> "Whoa, chill out!"
                False -> "Whatever."
              }
          }
      }
  }
}
