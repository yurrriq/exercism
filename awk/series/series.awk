BEGIN {
    if (len <= 0) {
        print "invalid length"
        exit 1
    }

    getline input

    if (length(input) == 0) {
        print "series cannot be empty"
        exit 1
    } else if (length(input) < len) {
        print "invalid length"
        exit 1
    } else if (length(input) == len) {
        print input
    } else {
        for (i = 1; i <= length(input) - len + 1; i++) {
            if (i > 1)
                printf " "
            printf substr(input, i, len)
        }
    }
}
