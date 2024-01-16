(ns cars-assemble)

(defn production-rate
  "Given an assembly line's `speed` (an integer between 0 and 10 inclusive),
  compute the hourly production rate, taking into account the success rate."
  [speed]
  {:pre [(<= 0 speed) (<= speed 10)]}
  (-> (cond
        (= 0 speed) 0.0
        (and (<= 1 speed) (<= speed 4)) 1.0
        (and (<= 5 speed) (<= speed 8)) 0.9
        (= 9 speed) 0.8
        (= 10 speed) 0.77)
      (* speed 221)))

(defn working-items
  "Calculate how many working cars are produced per minute."
  [speed]
  {:pre [(int? speed)]}
  (-> speed
      production-rate
      (/ 60)
      int))
