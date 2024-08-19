module NucleotideCount

using ThreadsX

export count_nucleotides

"""
    count_nucleotides(strand)

The count of each nucleotide within `strand` as a dictionary.

Invalid strands raise a `DomainError`.
"""
function count_nucleotides(strand::String)::Dict{Char,Integer}
    if any(∉("ACGT"), strand)
        throw(DomainError(strand, "strand contains invalid nucleotide"))
    end

    Dict(
        nucleotide => ThreadsX.count(==(nucleotide), strand)
        for nucleotide in "ACGT"
    )
end

end
