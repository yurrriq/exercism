(ns elyses-destructured-enchantments)

(defn first-card
  "Return the first card from the deck."
  [[first-card & _]]
  first-card)

(defn second-card
  "Return the second card from the deck."
  [[_ second-card & _]]
  second-card)

(defn swap-top-two-cards
  "Return the deck with first two cards reversed."
  [[first-card second-card & other-cards]]
  (concat [second-card first-card] other-cards))

(defn discard-top-card
  "Return a sequence containing the first card and
   a sequence of the remaining cards in the deck."
  [[card & other-cards]]
  [card other-cards])

(def face-cards
  ["jack" "queen" "king"])

(defn insert-face-cards
  "Returns the deck with face cards between its head and tail."
  [[card & other-cards]]
  (if (nil? card)
    face-cards
    (concat [card] face-cards other-cards)))
