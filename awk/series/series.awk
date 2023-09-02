@include "assert"

{
    assert(NF, "series cannot be empty")
    assert(len > 0 && len <= length(), "invalid length")

    for (i = 0; i <= length() - len; i++) {
        if (i > 0)
            printf " "
        printf substr($0, i + 1, len)
    }
}
