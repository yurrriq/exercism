// Package triangle classifies triangles.
package triangle

// Kind represents the classifications of a triangle.
type Kind int

const (
	// NaT is not a triangle.
	NaT Kind = iota
	// Equ is an equilateral triangle.
	Equ
	// Iso is an isosceles triangle.
	Iso
	// Sca is a scalene triangle.
	Sca
)

// KindFromSides classifies a triangle by its sides.
func KindFromSides(a, b, c float64) Kind {
	if a <= 0 || b <= 0 || c <= 0 {
		return NaT
	}

	if a >= b+c || b >= c+a || c >= a+b {
		return NaT
	}

	if a == b {
		if b == c {
			return Equ
		}
		return Iso
	}

	if a == c || b == c {
		return Iso
	}

	return Sca
}
