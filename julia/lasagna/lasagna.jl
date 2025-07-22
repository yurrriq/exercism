const expected_bake_time = 60

preparation_time = Base.Fix1(*, 2)

remaining_time = Base.Fix1(-, expected_bake_time) 

total_working_time(layers, time_in_oven) =
    preparation_time(layers) + time_in_oven
