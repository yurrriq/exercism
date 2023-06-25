// Package isbn implements the ISBN Verifier exercise.
package isbn

// IsValidISBN validates a book identification number.
func IsValidISBN(isbn string) bool {
	checksum, position := 0, 10

	for i, char := range []byte(isbn) {
		if position < 1 {
			return false
		} else if digit, ok := digitToInt(char); ok {
			checksum += digit * position
			position--
		} else if char == 'X' && position == 1 {
			checksum += 10 * position
			position--
		} else if !(char == '-' && (i == 1 || i == 5 || i == 11)) {
			return false
		}
	}

	return (position == 0) && checksum%11 == 0
}

func digitToInt(char byte) (digit int, ok bool) {
	if char < '0' || char > '9' {
		return digit, ok
	}
	return int(char - '0'), true
}
