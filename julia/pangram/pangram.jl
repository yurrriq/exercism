"""
    ispangram(input::String)::Bool

Return `true` if `input` contains every alphabetic character (case insensitive).
"""
ispangram(input::String)::Bool = 'a':'z' ⊆ lowercase(input)
