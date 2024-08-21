const rule_1 = r"^(?:[aeiou]|xr|yt)"
const rules_342 = r"^(?<prefix>[^aeiou]*qu|[^aeiou]+(?=y)|[^aeiou]+)(?<suffix>\w+)"


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
    translated = word
    if !occursin(rule_1, word)
        translated = replace(word, rules_342 => s"\g<suffix>\g<prefix>")
    end
    translated * "ay"
end
