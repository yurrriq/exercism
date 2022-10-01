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

var quotedPasswordPattern = regexp.MustCompile(`"[^"]*(?:(?i)password)"`)

func CountQuotedPasswords(lines []string) int {
	count := 0
	for _, line := range lines {
		count += len(quotedPasswordPattern.FindAllString(line, -1))
	}

	return count
}

var endOfLinePattern = regexp.MustCompile(`end-of-line[0-9]+`)

func RemoveEndOfLineText(text string) string {
	return endOfLinePattern.ReplaceAllString(text, "")
}

var usernamePattern = regexp.MustCompile(`User\s+(\S+)`)

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
