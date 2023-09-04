@include "assert"

NR == 1 {
    lhs = $0
    length_lhs = length(lhs)
}

NR == 2 {
    rhs = $0
    assert(length_lhs == length(rhs), "strands must be of equal length")

    distance = 0
    for (i = 1; i <= length_lhs; i++) {
        if (substr(lhs, i, 1) != substr(rhs, i, 1))
            distance++
    }

    print distance
}
