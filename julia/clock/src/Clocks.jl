module Clocks

using Dates, Mods, Printf

export Clock

struct Clock
    hour::Mod{24}
    minute::Mod{60}
end

Clock() = Clock(Mod{24}(0), Mod{60}(0))

function Clock(hour::Integer, minute::Integer)
    h, m = fldmod((hour * 60 + minute) % 1440, 60)
    Clock(Mod{24}(h), Mod{60}(m))
end

Base.show(io::IO, clock::Clock) =
    @printf(io, "\"%02d:%02d\"", value(clock.hour), value(clock.minute))

Base.:+(x::Clock, y::Clock) =
    x + Dates.Hour(value(y.hour)) + Dates.Minute(value(y.minute))

Base.:-(x::Clock, y::Clock) =
    Clock(value(x.hour - y.hour), x.minute - y.minute)

Base.:+(clock::Clock, hours::Dates.Hour) =
    Clock(clock.hour + hours.value, clock.minute)

Base.:-(clock::Clock, hours::Dates.Hour) = clock + -hours

Base.:+(clock::Clock, minutes::Dates.Minute) =
    Clock(value(clock.hour), value(clock.minute) + minutes.value)

Base.:-(clock::Clock, minutes::Dates.Minute) = clock + -minutes

end # module Clock
