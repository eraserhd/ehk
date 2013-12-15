(ns clojure-katas.4clojure.80)

(def perfect?

  (fn [n]
    (->>
      (for [d (range 1 n) :when (= 0 (mod n d))] d)
      (reduce +)
      (= n)))

  )

(= (perfect? 6) true)
(= (perfect? 7) false)
