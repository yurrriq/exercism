// Package rotationalcipher implements the Rotational Cipher exercise.
package rotationalcipher

// RotationalCipher rotates each letter in a given string by a given number of
// letters.
func RotationalCipher(plain string, shiftKey int) string {
	if shiftKey%26 == 0 {
		return plain
	}

	cipherText := make([]rune, len(plain))
	for i, char := range plain {
		cipherText[i] = shift(char, shiftKey)
	}

	return string(cipherText)
}

func shift(char rune, shiftKey int) rune {
	switch {
	case 'a' <= char && char <= 'z':
		return shiftWrapped(char, shiftKey, 'a')
	case 'A' <= char && char <= 'Z':
		return shiftWrapped(char, shiftKey, 'A')
	default:
		return char
	}
}

func shiftWrapped(char rune, shiftKey int, wrapAround int) rune {
	return rune(wrapAround + ((int(char) - wrapAround + shiftKey) % 26))
}
