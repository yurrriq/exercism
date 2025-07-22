function success_rate(speed)
   iszero(speed) && return zero(speed)
   1 ≤ speed ≤ 4 && return 1
   5 ≤ speed ≤ 8 && return 0.9
   speed == 9 ? 0.8 : 0.77
end

production_rate_per_hour(speed) =
    221speed * success_rate(speed)

working_items_per_minute(speed) =
    Int(production_rate_per_hour(speed) ÷ 60)
