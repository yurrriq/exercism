import Printf: @sprintf

const conversions = (
    (1000, "M"),
    (900,  "CM"),
    (500, "D"),
    (400,  "CD"),
    (100, "C"),
    (90, "XC"),
    (50, "L"),
    (40, "XL"),
    (10, "X"),
    (9, "IX"),
    (5, "V"),
    (4, "IV"),
    (1, "I")
)

function to_roman(number)
    if number ∉ 1:3999
        error(@sprintf("%d ∉ 1:3999", number))
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
