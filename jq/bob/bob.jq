def is_question:
  test("\\?\\s*$");

def is_silent:
  test("^\\s*$");

def is_yelled:
  test("[[:upper:]]") and (test("[[:lower:]]") | not);

.heyBob |
if is_silent then
  "Fine. Be that way!"
elif is_question and is_yelled then
  "Calm down, I know what I'm doing!"
elif is_question then
  "Sure."
elif is_yelled then
  "Whoa, chill out!"
else
  "Whatever."
end