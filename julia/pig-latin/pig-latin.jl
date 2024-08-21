"""
    translate(phrase::AbstractString)::String

Translate a `phrase` from English to Pig Latin.
"""
function translate(phrase::AbstractString)::String
    join(map(translate_word, split(phrase, " ")), " ")
end


"""
    translate_word(word::AbstractString)::String

Translate a `word` from English to Pig Latin.
"""
function translate_word(word::AbstractString)::String
    if occursin(r"^(?:xr|yt|[aeiou]+)\w+$", word)
        return word * "ay"
    end

    m = match(r"^([^aeiouy]*q)u(\w+)$", word)
    if m ≠ nothing
        return join(reverse(m.captures), "") * "uay"
    end

    m = match(r"^y(\w+)$", word)
    if m ≠ nothing
        return m.captures[1] * "yay"
    end

    m = match(r"^([^aeiouy]+)(y?\w+)$", word)
    if m ≠ nothing
        return join(reverse(m.captures), "") * "ay"
    end

    throw(DomainError(word, "word of unknown pattern"))
end
