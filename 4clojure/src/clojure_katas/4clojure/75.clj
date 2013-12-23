(ns clojure-katas.4clojure.75
  (:require [clojure-katas.algo :as algo]))

(def totient

  (fn [n]
    (if (= n 1)
      1
      (count
        (for [i (range 1 n)
              :when (= 1 (algo/gcd i n))]
          i))))

  )

(= 1 (totient 1))
(= 4 (totient 10))
