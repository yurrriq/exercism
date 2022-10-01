package logs

var knownApplications = map[rune]string{
	'❗': "recommendation",
	'🔍': "search",
	'☀': "weather",
}

// Application identifies the application emitting the given log.
func Application(log string) string {
	for _, roon := range log {
		if application, isKnown := knownApplications[roon]; isKnown {
			return application
		}
	}

	return "default"
}

// Replace replaces all occurrences of old with new, returning the modified log
// to the caller.
func Replace(log string, oldRune, newRune rune) string {
	newLog := make([]rune, 0, len(log))
	for _, roon := range log {
		if roon == oldRune {
			newLog = append(newLog, newRune)
		} else {
			newLog = append(newLog, roon)
		}
	}

	return string(newLog)
}

// WithinLimit determines whether or not the number of characters in log is
// within the limit.
func WithinLimit(log string, limit int) bool {
	for i, _ := range log {
		if i+1 > limit {
			return false
		}
	}

	return true
}
