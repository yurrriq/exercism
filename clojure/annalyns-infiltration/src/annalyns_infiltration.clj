(ns annalyns-infiltration
  "The Annalyn's Infiltration exercise")

(def can-fast-attack?
  "Determine if a fast attack can be made."
  not)

(defn can-spy?
  "Determine if the kidnappers can be spied upon."
  [knight-awake? archer-awake? prisoner-awake?]
  (or knight-awake? archer-awake? prisoner-awake?))

(defn can-signal-prisoner?
  "Determine if the prisoner can be signaled."
  [archer-awake? prisoner-awake?]
  (and prisoner-awake? (not archer-awake?)))

(defn can-free-prisoner?
  "Determine if the prisoner can be freed."
  [knight-awake? archer-awake? prisoner-awake? dog-present?]
  (if dog-present?
    (not archer-awake?)
    (and prisoner-awake?
         (not (or knight-awake?
                  archer-awake?)))))
