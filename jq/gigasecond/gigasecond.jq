def gigasecond:
  1e9
;

def full_time_format:
  "%Y-%m-%dT%H:%M:%S"
;

.moment |
(try strptime("%Y-%m-%d")) // (strptime(full_time_format)) |
mktime + gigasecond |
gmtime |
strftime(full_time_format)
