module RotationalCipher

export rotate, shiftwrapped


for i in range(1, 26)
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

julia> rotate(0, 'c')
'c': ASCII/Unicode U+0063 (category Ll: Letter, lowercase)

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

    io = IOBuffer()
    foreach(char -> write(io, rotate(n, char)), input)
    String(take!(io))
end


"""
    rotate(n::Integer, char::AbstractChar)::Char

Rotate a character by `n`.
"""
function rotate(n::Integer, char::AbstractChar)::Char
    if islowercase(char)
        shiftwrapped(n, 'a', char)
    elseif isuppercase(char)
        shiftwrapped(n, 'A', char)
    else
        char
    end
end


"""
    shiftwrapped(n::Integer, pivot::AbstractChar, char::AbstractChar)::Char

Shift a letter by `n % 26`, wrapping around `pivot`.

```jldoctest
julia> shiftwrapped(42, 'A', 'C')
'S': ASCII/Unicode U+0053 (category Lu: Letter, uppercase)
```
"""
function shiftwrapped(n::Integer, pivot::AbstractChar, char::AbstractChar)::Char
    shiftwrapped(n, Int(pivot), Int(char))
end


function shiftwrapped(n::Integer, pivot::Integer, char::Integer)::Char
    Char(pivot + (char - pivot + n) % 26)
end

end # module RotationalCipher
