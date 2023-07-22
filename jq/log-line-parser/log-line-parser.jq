def trim: sub("^\\s+"; "") | sub("\\s+$"; "");

def log_format: capture("\\[(?<level>[[:alpha:]]+)\\]: (?<message>.+)");

def message: log_format | .message | trim;

def log_level: log_format | .level | ascii_downcase;

def reformat: . as $line | "\($line | message) (\($line | log_level))";
