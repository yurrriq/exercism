package techpalace

import (
	"fmt"
	"strings"
)

// Return a welcome message for the customer.
func WelcomeMessage(customer string) string {
	return fmt.Sprintf("Welcome to the Tech Palace, %s", strings.ToUpper(customer))
}

// Add a border to a welcome message.
func AddBorder(welcomeMsg string, numStarsPerLine int) string {
	starsLine := strings.Repeat("*", numStarsPerLine)
	return strings.Join([]string{
		starsLine,
		welcomeMsg,
		starsLine,
	}, "\n")
}

// Clean up an old marketing message.
func CleanupMessage(oldMsg string) string {
	return strings.TrimSpace(strings.ReplaceAll(oldMsg, "*", ""))
}

func Unlines(lines []string) string {
	return strings.Join(lines, "\n")
}
