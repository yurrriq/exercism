@load "ordchr"

BEGIN {
    a = ord("a")
    z = ord("z")
}

function cipher(char) {
    switch (char) {
    case /[[:lower:]]/:
        return chr(a + (z - ord(char)))
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
    $0 = tolower($0)
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
