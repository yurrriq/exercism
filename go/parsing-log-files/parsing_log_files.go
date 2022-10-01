package parsinglogfiles

import "regexp"

var validPrefixesPattern = regexp.MustCompile(`^\[(?:TRC|DBG|INF|WRN|ERR|FTL)\].+$`)

func IsValidLine(text string) bool {
	return validPrefixesPattern.MatchString(text)
}

var separatorPattern = regexp.MustCompile(`<[~*=-]*>`)

func SplitLogLine(text string) []string {
	return separatorPattern.Split(text, -1)
}

var quotedPasswordPattern = regexp.MustCompile(`(?i)"[^"]*password"`)

func CountQuotedPasswords(lines []string) (count int) {
	for _, line := range lines {
		count += len(quotedPasswordPattern.FindAllString(line, -1))
	}

	return count
}

var endOfLinePattern = regexp.MustCompile(`end-of-line\d+`)

func RemoveEndOfLineText(text string) string {
	return endOfLinePattern.ReplaceAllLiteralString(text, "")
}

var usernamePattern = regexp.MustCompile(`User\s+(\w+)`)

func TagWithUserName(lines []string) []string {
	taggedLines := make([]string, 0, len(lines))
	for _, line := range lines {
		matches := usernamePattern.FindStringSubmatch(line)
		if len(matches) == 2 {
			username := matches[1]
			taggedLines = append(taggedLines, "[USR] "+username+" "+line)
		} else {
			taggedLines = append(taggedLines, line)
		}
	}

	return taggedLines
}
