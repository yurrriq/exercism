@load "ordchr"

{
    input = $0

    for (i = length(input); i >= 1; i--) {
        char = substr(input, i, 1)
        switch (char) {
        case " ":
            continue
        case /[^0-9]/:
            print "false"
            exit
        default:
            digit = int(char)
            if (and(len, 1) == 1)
                 digit *= 2
            if (digit > 9)
                digit -= 9
            sum += digit
            len++
        }
    }

    if (len > 1 && sum % 10 == 0)
        print "true"
    else
        print "false"
}
