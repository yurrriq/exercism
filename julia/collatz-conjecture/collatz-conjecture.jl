import Base: iterate, eltype, IteratorSize, IteratorEltype

struct CollatzIterator{T<:Integer}
    n::T
end

function iterate(ci::CollatzIterator, state=ci.n)
    next = iseven(state) ? state ÷ 2 : 3state + 1
    (next, next)
end

function collatz_steps(n)
    n < 1 && throw(DomainError(n, "must be positive"))
    isone(n) && return zero(n)
    1 + sum(1 for _ ∈ Iterators.takewhile(≠(1), CollatzIterator(n)))
end
