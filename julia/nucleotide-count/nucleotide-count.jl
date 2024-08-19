"""
    count_nucleotides(strand)

The count of each nucleotide within `strand` as a dictionary.

Invalid strands raise a `DomainError`.
"""
function count_nucleotides(strand::String)::Dict{Char,Integer}
    counts = Dict(nucleotide => 0 for nucleotide in "ACGT")
    for nucleotide in strand
        if nucleotide ∉ "ACGT"
            throw(DomainError(nucleotide, "invalid nucleotide"))
        end
        counts[nucleotide] += 1
    end
    counts
end
