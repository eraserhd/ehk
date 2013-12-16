(ns clojure-katas.4clojure.75)

(def totient

  (fn [n]
    (letfn [(gcd [a b]
              (if (= b 0)
                a
                (recur b (mod a b))))]
      (if (= n 1)
        1
        (count
          (for [i (range 1 n)
                :when (= 1 (gcd i n))]
            i)))))

  )

(= 1 (totient 1))
(= 4 (totient 10))
