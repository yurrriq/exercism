@enum Classification begin
    deficient
    perfect
    abundant
end

aliquotsum(n) =
    sum(d for d ∈ 1:(n-1) if iszero(n % d))

function classify(n)
    n ≤ 0 && throw(DomainError(n, "must be positive"))
    asum = aliquotsum(n)
    asum < n && return deficient::Classification
    asum ≡ n ? perfect::Classification : abundant::Classification
end

isdeficient(n) =
    classify(n) ≡ deficient::Classification

isperfect(n) =
    classify(n) ≡ perfect::Classification

isabundant(n) =
    classify(n) ≡ abundant::Classification
