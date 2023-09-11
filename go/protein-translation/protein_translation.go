// Package protein has functionality to translate RNA sequences into proteins.
package protein

import (
	"errors"
	"sort"
)

var (
	// ErrInvalidBase indicates the codon was invalid.
	ErrInvalidBase = errors.New("invalid base")

	// ErrStop indicates the translation was stopped.
	ErrStop = errors.New("STOP")
)

// FromRNA maps RNA codons to their matching proteins and returns either the
// list of proteins or returns an error for an invalid codon.
func FromRNA(rna string) ([]string, error) {
	proteins := make([]string, 0)
	for {
		if len(rna) < 3 {
			break
		}

		protein, err := FromCodon(rna[:3])
		if err == ErrInvalidBase {
			return []string{}, err
		}
		if err == ErrStop {
			rna = ""
			break
		}

		proteins = append(proteins, protein)
		rna = rna[3:]
	}

	if rna != "" {
		return []string{}, ErrInvalidBase
	}

	return proteins, nil
}

// FromCodon either translates a codon to a protein or returns an error for a
// stop codon or invalid codon.
func FromCodon(codon string) (protein string, err error) {
	switch codon {
	case "AUG":
		protein = "Methionine"
	case "UUU", "UUC":
		protein = "Phenylalanine"
	case "UUA", "UUG":
		protein = "Leucine"
	case "UCU", "UCC", "UCA", "UCG":
		protein = "Serine"
	case "UAU", "UAC":
		protein = "Tyrosine"
	case "UGU", "UGC":
		protein = "Cysteine"
	case "UGG":
		protein = "Tryptophan"
	case "UAA", "UAG", "UGA":
		err = ErrStop
	default:
		err = ErrInvalidBase
	}

	return protein, err
}

func reverse[T comparable](s []T) {
	sort.SliceStable(s, func(i, j int) bool {
		return i > j
	})
}
