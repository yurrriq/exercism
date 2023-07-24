.legacy |
to_entries |
map({key: (.value | map(ascii_downcase))[], value: (.key | tonumber)}) |
flatten |
sort_by(.key) |
from_entries