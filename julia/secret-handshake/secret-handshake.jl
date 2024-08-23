using .Iterators: reverse

const actions = ["wink", "double blink", "close your eyes", "jump"]

function secret_handshake(code)
    handshake = [action for (i, action) in enumerate(actions) if bitset(i - 1, code)]
    bitset(4, code) && reverse!(handshake)
    handshake
end

bitset(k, n) = n & (1 << k) > 0
