function score(x, y)
    distance = √(x * x + y * y)
    distance > 10 && return 0
    distance > 5 && return 1
    distance > 1 ? 5 : 10
end
