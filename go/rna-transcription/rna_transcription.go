// Package strand implements the RNA Transcription exercise.
package strand

// ToRNA converts a given DNA string into its RNA complement.
func ToRNA(dna string) string {
	runes := []rune(dna)
	rna := make([]rune, 0, len(runes))
	for _, nucleotide := range runes {
		switch nucleotide {
		case 'G':
			rna = append(rna, 'C')
		case 'C':
			rna = append(rna, 'G')
		case 'T':
			rna = append(rna, 'A')
		case 'A':
			rna = append(rna, 'U')
		default:
			panic("Invalid nucleotide")
		}
	}

	return string(rna)
}
