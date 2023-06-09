package secret

import (
	"sort"
)

func Handshake(code uint) []string {
	actions := make([]string, 0, 5)

	if 1&code == 1 {
		actions = append(actions, "wink")
	}

	if 2&code == 2 {
		actions = append(actions, "double blink")
	}

	if 4&code == 4 {
		actions = append(actions, "close your eyes")
	}

	if 8&code == 8 {
		actions = append(actions, "jump")
	}

	if 16&code == 16 {
		reverseSlice(actions)
	}

	return actions
}

func reverseSlice[T comparable](s []T) {
	sort.SliceStable(s, func(i, j int) bool {
		return i > j
	})
}
