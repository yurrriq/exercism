@include "assert"

BEGIN {
    RS = ""
    distance = 0
}

{
    lhs = $1
    length_lhs = length(lhs)
    rhs = $2
    assert(length_lhs == length(rhs), "strands must be of equal length")
}

END {
    for (i = 1; i <= length_lhs; i++) {
        if (substr(lhs, i, 1) != substr(rhs, i, 1))
            distance++
    }

    print distance
}
