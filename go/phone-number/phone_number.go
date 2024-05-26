// Package phonenumber implements the Phone Number exercise from Exercism.
package phonenumber

import (
	"errors"
	"fmt"
	"regexp"
)

// Number attempts to sanitizes a phone number.
func Number(phoneNumber string) (sanitizedNumber string, err error) {
	re := regexp.MustCompile(`\D`)
	digits := re.ReplaceAllString(phoneNumber, "")

	switch len(digits) {
	case 10:
		sanitizedNumber = digits
	case 11:
		if digits[0] != '1' {
			return "", errors.New("Unknown country code")
		}

		sanitizedNumber, err = Number(digits[1:])
		if err != nil {
			return "", err
		}
	default:
		return "", errors.New("Invalid length")
	}

	switch sanitizedNumber[0] {
	case '0', '1':
		return "", errors.New("Invalid area code")
	}

	switch sanitizedNumber[3] {
	case '0', '1':
		return "", errors.New("Invalid exchange code")
	}

	return sanitizedNumber, nil
}

// AreaCode attempts to extract the area code of a phone number.
func AreaCode(phoneNumber string) (string, error) {
	sanitizedNumber, err := Number(phoneNumber)
	if err != nil {
		return "", err
	}

	return sanitizedNumber[:3], nil
}

// Format attempts to normlize a phone number.
func Format(phoneNumber string) (string, error) {
	sanitizedNumber, err := Number(phoneNumber)
	if err != nil {
		return "", err
	}

	sanitizedNumber = fmt.Sprintf("(%s) %s-%s",
		sanitizedNumber[:3],
		sanitizedNumber[3:6],
		sanitizedNumber[6:],
	)
	return sanitizedNumber, err
}
