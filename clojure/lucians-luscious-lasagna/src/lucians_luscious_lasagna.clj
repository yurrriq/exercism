(ns lucians-luscious-lasagna)

(def expected-time
  "According to the cooking book, the expected oven time in minutes is 40."
  40)

(defn remaining-time
  "Given the actual time in minutes the lasagna has been in the oven,
   return how many minutes the lasagna still has to remain in the oven."
  [actual-time]
  (- expected-time actual-time))

(defn prep-time
  "Given the number of layers added to the lasagna,
   return how many minutes you spent preparing the lasagna."
  [num-layers]
  (* num-layers 2))

(defn total-time
  "Given the number of layers of lasagna and the actual time in minutes it has been in the oven,
   Returns how many minutes in total you've worked on cooking the lasagna"
  [num-layers actual-time]
  (+ (prep-time num-layers)
     actual-time))
