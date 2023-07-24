# Given a numeric score between 0 and 100, output a letter grade
# - "A" is 90% - 100%
# - "B" is 80% - 89%
# - "C" is 70% - 79%
# - "D" is 60% - 69%
# - "F" is  0% - 59%

def letter_grade:
  if 90 <= . and . <= 100 then
    "A"
  elif 80 <= . and . < 90 then
    "B"
  elif 70 <= . and . < 80 then
    "C"
  elif 60 <= . and . < 70 then
    "D"
  else
    "F"
  end
;

# Given an object that maps a student's name to their grade,
# generate an object that maps the letter grade to the number of
# students with that grade

def count_letter_grades:
  reduce (to_entries | map(.value)[]) as $score ({A: 0, B: 0, C: 0, D: 0, F: 0};
    .[$score | letter_grade] += 1
  )
;
