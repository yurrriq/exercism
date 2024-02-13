using OrderedCollections, Printf

const conversions = OrderedDict(
    1000 => "M",
    900 => "CM",
    500 => "D",
    400 => "CD",
    100 => "C",
    90 => "XC",
    50 => "L",
    40 => "XL",
    10 => "X",
    9 => "IX",
    5 => "V",
    4 => "IV",
    1 => "I"
)

function to_roman(number)
    if number < 1 || number > 3999
        error(@sprintf("%d is not within 1 and 3999, inclusive", number))
    end

    converted, toconvert = "", number
    for (arabic, roman) in conversions
        if arabic <= toconvert
            q, toconvert = divrem(toconvert, arabic)
            converted *= roman ^ q
        end
    end

    converted
end
