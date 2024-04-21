import Combinatorics: partitions

function combinations_in_cage(n, k)
    collect(Iterators.map(sort, Iterators.filter(allunique, partitions(n, k))))
end

function combinations_in_cage(n, k, restrictions)
    filter(ys -> not_any_in(restrictions, ys), combinations_in_cage(n, k))
end

function not_any_in(xs, ys)
    !any(x -> x in ys, xs)
end
