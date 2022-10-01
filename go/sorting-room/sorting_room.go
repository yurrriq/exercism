package sorting

import "fmt"

// DescribeNumber returns a string describing the number.
func DescribeNumber(f float64) string {
	return fmt.Sprintf("This is the number %.1f", f)
}

type NumberBox interface {
	Number() int
}

// DescribeNumberBox returns a string describing the NumberBox.
func DescribeNumberBox(nb NumberBox) string {
	return fmt.Sprintf("This is a box containing the number %.1f", float64(nb.Number()))
}

type FancyNumber struct {
	n string
}

func (i FancyNumber) Value() string {
	return i.n
}

type FancyNumberBox interface {
	Value() string
}

// ExtractFancyNumber returns the integer value for a FancyNumber and 0 if any
// other FancyNumberBox is supplied.
func ExtractFancyNumber(fnb FancyNumberBox) int {
	_, ok := fnb.(FancyNumber)
	if !ok {
		return 0
	}

	var i int
	_, err := fmt.Sscan(fnb.Value(), &i)
	if err != nil {
		return 0
	}

	return i
}

// DescribeFancyNumberBox returns a string describing the FancyNumberBox.
func DescribeFancyNumberBox(fnb FancyNumberBox) string {
	i := ExtractFancyNumber(fnb)
	return fmt.Sprintf("This is a fancy box containing the number %.1f", float64(i))
}

// DescribeAnything returns a string describing whatever it contains.
func DescribeAnything(i interface{}) string {
	switch x := i.(type) {
	case int:
		return DescribeNumber(float64(x))
	case float64:
		return DescribeNumber(x)
	case NumberBox:
		return DescribeNumberBox(x)
	case FancyNumberBox:
		return DescribeFancyNumberBox(x)
	default:
		return "Return to sender"
	}
}
