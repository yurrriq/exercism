const words = ["Pling", "Plang", "Plong"]

function raindrops(number)
    result = ""
    for (i, word) in enumerate(words)
        if number % (2 * i + 1) ≡ 0
            result *= word
        end
    end
    isempty(result) ? string(number) : result
end
