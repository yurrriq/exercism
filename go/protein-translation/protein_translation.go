package protein

import (
	"errors"
	"sort"
)

var ErrStop = errors.New("STOP")

var ErrInvalidBase = errors.New("invalid base")

func FromRNA(rna string) ([]string, error) {
	codons := make([]string, 0)
	for {
		if len(rna) < 3 {
			break
		}

		aminoAcid, err := FromCodon(rna[:3])
		if err == ErrInvalidBase {
			return []string{}, err
		}
		if err == ErrStop {
			rna = ""
			break
		}

		codons = append(codons, aminoAcid)
		rna = rna[3:]
	}

	if rna != "" {
		return []string{}, ErrInvalidBase
	}

	return codons, nil
}

func FromCodon(codon string) (string, error) {
	protein, ok := aminoAcids()[codon]
	if !ok {
		return "", ErrInvalidBase
	}

	if protein == "STOP" {
		return "", ErrStop
	}

	return protein, nil
}

func aminoAcids() map[string]string {
	codons := make(map[string]string)
	codons["AUG"] = "Methionine"
	codons["UUU"] = "Phenylalanine"
	codons["UUC"] = codons["UUU"]
	codons["UUA"] = "Leucine"
	codons["UUG"] = codons["UUA"]
	codons["UCU"] = "Serine"
	codons["UCC"] = codons["UCU"]
	codons["UCA"] = codons["UCU"]
	codons["UCG"] = codons["UCU"]
	codons["UAU"] = "Tyrosine"
	codons["UAC"] = codons["UAU"]
	codons["UGU"] = "Cysteine"
	codons["UGC"] = codons["UGU"]
	codons["UGG"] = "Tryptophan"
	codons["UAA"] = "STOP"
	codons["UAG"] = codons["UAA"]
	codons["UGA"] = codons["UAA"]

	return codons
}

func reverse[T comparable](s []T) {
	sort.SliceStable(s, func(i, j int) bool {
		return i > j
	})
}
