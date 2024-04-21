import Combinatorics: partitions

function combinations_in_cage(n, k)
    collect(filtermap(sort, allunique, partitions(n, k)))
end

function combinations_in_cage(n, k, restrictions)
    filter(not_any_in(restrictions), combinations_in_cage(n, k))
end

filtermap(f, pred, xs) = Iterators.map(f, Iterators.filter(pred, xs))

not_any_in(xs) = ys -> not_any_in(xs, ys)
not_any_in(xs, ys) = !any(in(xs), ys)
