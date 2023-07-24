def trim: sub("^\\s+"; "") | sub("\\s+$"; "");

def is_question:
  endswith("?");

def is_yelled:
  test("[[:alpha:]]") and (test("[[:lower:]]") | not);

.heyBob |
trim |
if . == "" then
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