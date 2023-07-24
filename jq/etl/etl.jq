.legacy |
to_entries |
map({key: (.value[] | ascii_downcase), value: (.key | tonumber)}) |
flatten |
sort_by(.key) |
from_entries
