// Package dna implements the Nucleotide Count exercise.
package dna

// Histogram is a mapping from nucleotide to its count in given DNA.
type Histogram map[Nucleotide]uint

// A Nucleotide is a rune.
type Nucleotide rune

// 'A' for adenine, 'C' for cytosine, 'G' for guanine, and 'T' for thymine.
const (
	Adenine  Nucleotide = 'A'
	Cytosine            = 'C'
	Guanine             = 'G'
	Thymine             = 'T'
)

// NewNucleotide is a smart constructor to convert a rune to Nucleotide.
func NewNucleotide(roon rune) (nucleotide *Nucleotide, err error) {
	switch roon {
	case 'A', 'C', 'G', 'T':
		nucleotide := Nucleotide(roon)
		return &nucleotide, nil
	default:
		return nil, ErrInvalidNucleotide
	}
}

// DNA is a list of nucleotides. Choose a suitable data type.
type DNA []rune

// ErrInvalidNucleotide is thrown when an invalid nucleotide is encountered.
const ErrInvalidNucleotide NucleotideError = "Invalid nucleotide"

// A NucleotideError is a string.
type NucleotideError string

// Error describes a DNSError as a string.
func (err NucleotideError) Error() string {
	return string(err)
}

// Counts generates a histogram of valid nucleotides in the given DNA.
// Returns an error if dna contains an invalid nucleotide.
func (dna DNA) Counts() (Histogram, error) {
	counts := Histogram{Adenine: 0, Cytosine: 0, Guanine: 0, Thymine: 0}
	for _, roon := range dna {
		nucleotide, err := NewNucleotide(roon)
		if err != nil {
			return nil, err
		}
		counts[*nucleotide]++
	}

	return counts, nil
}
