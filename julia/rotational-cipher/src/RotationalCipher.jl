module RotationalCipher

export rotate


for i in range(0, 26)
    @eval export $(Symbol("@R$(i)_str"))
    @eval macro $(Symbol("R$(i)_str"))(s)
        rotate($i, s)
    end
end


"""
    rotate(n::Integer, input::AbstractString)

Rotate a string `input` by `n`.

```jldoctest
julia> R5"omg"
"trl"

julia> R0"c"
"c"

julia> R26"Cool"
"Cool"

julia> R13"The quick brown fox jumps over the lazy dog."
"Gur dhvpx oebja sbk whzcf bire gur ynml qbt."

julia> R13"Gur dhvpx oebja sbk whzcf bire gur ynml qbt."
"The quick brown fox jumps over the lazy dog."
```
"""
function rotate(n::Integer, input::AbstractString)::String
    if n % 26 ≡ 0
        return input
    end

    map(char -> rotate(n, char), input)
end


"""
    rotate(n::Integer, char::AbstractChar)::Char

Rotate an ASCII letter by `n`.
"""
function rotate(n::Integer, char::AbstractChar)::Char
    if !(isascii(char) && isletter(char))
        return char
    end

    pivot = islowercase(char) ? 'a' : 'A'
    pivot + (char - pivot + n) % 26
end

end # module RotationalCipher
