// Package strand implements the RNA Transcription exercise.
package strand

// ToRNA converts a given DNA string into its RNA complement.
func ToRNA(dna string) string {
	length := len(dna)
	rna := make([]byte, length)
	for i := 0; i < length; i++ {
		rna[i] = rnaComplement(dna[i])
	}

	return string(rna)
}

func rnaComplement(nucleotide byte) byte {
	switch nucleotide {
	case 'G':
		return 'C'
	case 'C':
		return 'G'
	case 'T':
		return 'A'
	case 'A':
		return 'U'
	default:
		panic("Invalid nucleotide: " + string(nucleotide))
	}
}
