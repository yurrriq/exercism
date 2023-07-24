def date_only_format:
  "%Y-%m-%d"
;

def full_time_format:
  "%Y-%m-%dT%H:%M:%S"
;

.moment |
(try strptime(date_only_format)) // (strptime(full_time_format)) |
mktime + 1e9 |
strftime(full_time_format)
