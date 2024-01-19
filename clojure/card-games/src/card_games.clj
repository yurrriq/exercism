(ns card-games)

(defn rounds
  "Return a list with a given round number and the next two."
  [n]
  (take 3 (iterate inc n)))

(defn concat-rounds
  "Concatenate two lists of rounds."
  [xs ys]
  (concat xs ys))

(defn contains-round?
  "Determine if a given list of rounds contains a given round."
  [rounds round]
  (boolean (some #{round} rounds)))

(defn card-average
  "Compute the average value of a hand."
  [hand]
  (double (/ (apply + hand) (count hand))))

(defn approx-average?
  "Determine if the average value of a hand is equal to either of:
  - the average of the _first_ and _last_ card
  - the median (middle card)"
  [hand]
  (let [actual-average (card-average hand)]
    (or (= actual-average (card-average [(first hand) (last hand)]))
        (= actual-average (double (nth hand (/ (count hand) 2)))))))

(defn average-even-odd?
  "Determine if the average of the even-indexed cards is equal to the
  average of the odd-indexed cards."
  [hand]
  (= (card-average (take-nth 2 hand))
     (card-average (take-nth 2 (rest hand)))))

(defn maybe-double-last
  "If the last card is a Jack (11), double its value."
  [hand]
  (if (= 11 (last hand))
    (concat (butlast hand) [22])
    hand))
