module Clocks

using Dates, Mods, Printf

export Clock

struct Clock
    hour::Mod{24}
    minute::Mod{60}
end

Clock() = Clock(Mod{24}(0), Mod{60}(0))

function Clock(hour::Integer, minute::Integer)
    Clock() + Dates.Hour(hour) + Dates.Minute(minute)
end

function Base.show(io::IO, clock::Clock)
    @printf(io, "\"%02d:%02d\"", value(clock.hour), value(clock.minute))
end

function Base.:+(x::Clock, y::Clock)::Clock
    x + Dates.Hour(value(y.hour)) + Dates.Minute(value(y.minute))
end

function Base.:-(x::Clock, y::Clock)::Clock
    x - Dates.Hour(value(y.hour)) - Dates.Minute(value(y.minute))
end

function Base.:+(clock::Clock, hours::Dates.Hour)::Clock
    Clock(clock.hour + hours.value, clock.minute)
end

Base.:-(clock::Clock, hours::Dates.Hour) = clock + -hours

function Base.:+(clock::Clock, minutes::Dates.Minute)::Clock
    h, m = fldmod((value(clock.minute) + minutes.value) % 1440, 60)
    Clock(clock.hour + h, Mod{60}(m))
end

Base.:-(clock::Clock, minutes::Dates.Minute) = clock + -minutes

end # module Clock
