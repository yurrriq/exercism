# These variables are initialized on the command line (using '-v'):
# -direction

BEGIN {
    uppers = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    lowers = "abcdefghijklmnopqrstuvwxyz"
}

function cipher(char) {
    switch (char) {
    case /[[:upper:]]/:
        return substr(lowers, 27 - index(uppers, char), 1)
    case /[[:lower:]]/:
        return substr(lowers, 27 - index(lowers, char), 1)
    case /[[:digit:]]/:
        return char
    default:
        return ""
    }
}

function transcode(str) {
    result = ""
    for (i = 1; i <= length(str); i++)
        result = result cipher(substr(str, i, 1))
    return result
}

{
    switch (direction) {
    case "encode":
        encoded = transcode($0)
        for (i = 1; i <= length(encoded); i += 5) {
            if (i > 1)
                printf " "
            printf substr(encoded, i, 5)
        }
        break
    case "decode":
        print transcode($0)
        break
    default:
        print "unknown direction"
        exit 1
    }
}
