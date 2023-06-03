(ns flatten-array)

(defn flatten [arr]
  (mapcat (fn [x]
            (cond
              (sequential? x) (flatten x)
              (some? x) [x]
              :else []))
          arr))
