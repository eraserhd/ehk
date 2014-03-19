(ns clojure-katas.4clojure.111)

(def crossword-puzzle

  (fn [w p]
    (let [p (vec (map #(clojure.string/replace % #"\s+" "") p))

          at (fn [i j]
               (get-in p [i j] \#))

          at-n (fn [[i j] [i-delta j-delta] n]
                 (at (+ i (* n i-delta)) (+ j (* n j-delta))))

          across [0 1]

          down [1 0]

          fits-at (fn [pos dir]
                    (and (every?
                           #(some (partial = (at-n pos dir %)) [\_ (get w %)])
                           (range (count w)))
                         (= \# (at-n pos dir -1))
                         (= \# (at-n pos dir (count w)))))

          all-positions (for [i (range (count p))
                              j (range (count (get p i)))]
                          [i j])

          allowed? (or (some #(fits-at % across) all-positions)
                       (some #(fits-at % down) all-positions)
                       false)]
      allowed?))

  )

(comment
  (= true (crossword-puzzle "the" ["_ # _ _ e"]))

  (= false (crossword-puzzle "the" ["c _ _ _"
                                    "d _ # e"
                                    "r y _ _"])))
