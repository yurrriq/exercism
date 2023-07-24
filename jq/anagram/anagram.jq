def normalize:
  ascii_downcase |
  explode |
  sort |
  implode;

(.subject | ascii_downcase) as $downcased_subject |
(.subject | normalize) as $normalized_subject |
.candidates |
map(
  select(
    ascii_downcase != $downcased_subject and
    normalize == $normalized_subject
  )
)
