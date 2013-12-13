(ns clojure-katas.4clojure.53)

(def longest-increasing-sub-seq

  (fn [s]
    (reduce
      (fn [a b]
        (if (< (count a) (count b))
          b
          a))
      []
      (apply concat
        (for [i (range (count s))]
          (for [j (range (+ i 1) (count s))
                :while (< (s (dec j)) (s j))]
            (subvec s i (+ j 1)))))))


  )

(= (longest-increasing-sub-seq [1 0 1 2 3 0 4 5]) [0 1 2 3])
(= (longest-increasing-sub-seq [5 6 1 3 2 7]) [5 6])
(= (longest-increasing-sub-seq [2 3 3 4 5]) [3 4 5])
(= (longest-increasing-sub-seq [7 6 5 4]) [])
