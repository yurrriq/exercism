(ns coordinate-transformation)

(defn translate2d
  "Return a function to translate a coordinate pair."
  [dx dy]
  (fn [x y]
    (mapv + [x y] [dx dy])))

(defn scale2d
  "Return a function to scale a coordinate pair."
  [sx sy]
  (fn [x y]
    (mapv * [x y] [sx sy])))

(defn compose-transform
  "Compose two coordinate pair transformations left to right."
  [f g]
  (fn [x y]
    (apply g (f x y))))

(defn memoize-transform
  "Memoize the last result of a coordinate pair transformation."
  [f]
  (let [transforms (atom {:x nil, :y nil, :result nil})]
    (fn [x y]
      (let [{x' :x, y' :y :as previous} @transforms]
        (:result (if (and (= x x') (= y y'))
                   previous
                   (swap! transforms assoc :x x :y y :result (f x y))))))))
