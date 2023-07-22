def trim: sub("^\\s+"; "") | sub("\\s+$"; "");

def parse_line: capture("\\[(?<level>[[:upper:]]+)\\]: (?<message>.+)");

def message: parse_line | .message | trim;

def log_level: parse_line | .level | ascii_downcase;

def reformat: "\(message) (\(log_level))";
