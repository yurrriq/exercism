function collatz_steps(n; steps=0)
    n < 1 && throw(DomainError(n, "must be positive"))
    isone(n) && return steps
    collatz_steps(iseven(n) ? n ÷ 2 : 3n + 1; steps=steps + 1)
end
