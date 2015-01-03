(ns beer-song
  (:require [clojure.string :refer [capitalize join]]))

(defn verse
  ([n] 
   (str (capitalize (verse n :first)) ", "
        (verse n :second)
        (verse n :next)))
  ([n position]
   (let [s? (if (not= n 1) "s" "")
         n? (if (= n 0) "no more" n)
         one? (if (= n 1) "it" "one")]
     (case position
       :first  (format "%s bottle%s of beer on the wall" n? s?)
       :second (format  "%s bottle%s of beer.\n" n? s?)
       :next   (str
                 (if (> n 0)
                   (format "Take %s down and pass it around, " one?)
                   "Go to the store and buy some more, ")
                 (verse (if (= n 0) 99 (dec n)) :first)
                 ".\n")
       (verse n)))))

(defn sing [start & end]
  (join "\n"
        (for [n (reverse (range (or (first end) 0) (inc start)))]
          (verse n))))
