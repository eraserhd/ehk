(ns clojure-katas.4clojure.111)

(def crossword-puzzle

  (fn [w p]
    (let [p (vec (map #(clojure.string/replace % #"\s+" "") p))
          in-bounds (fn [i j]
                      (and (>= i 0)
                           (>= j 0)
                           (< i (count p))
                           (< j (count (get p i)))))

          at (fn [i j]
               (if-not (in-bounds i j)
                 \#
                 (get (get p i) j)))

          w-length (count w)

          check (fn [[i j] [i-delta j-delta]]
                  (and (every?
                         identity
                         (for [n (range (count w))]
                           (let [board-c (at (+ i (* i-delta n)) (+ j (* j-delta n)))
                                 word-c (get w n)]
                             (or (= board-c \_)
                                 (= board-c word-c)))))
                       (= \# (at (- i i-delta) (- j j-delta)))
                       (= \# (at (+ i (* i-delta w-length)) (+ j (* j-delta w-length))))))

          all-positions (apply
                          concat
                          (for [i (range (count p))]
                            (for [j (range (count (get p i)))]
                              [i j])))

          allowed? (or (some identity (map #(check % [1 0]) all-positions))
                       (some identity (map #(check % [0 1]) all-positions))
                       false)]
      allowed?))

  )

(= true (crossword-puzzle "the" ["_ # _ _ e"]))

(= false (crossword-puzzle "the" ["c _ _ _"
                                  "d _ # e"
                                  "r y _ _"]))
