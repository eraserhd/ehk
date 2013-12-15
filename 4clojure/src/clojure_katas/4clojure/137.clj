(ns clojure-katas.4clojure.137)

(def in-base

  (fn [n b]
    (if (= n 0)
      '(0)
      (loop [n n
             r ()]
        (if (= n 0)
          r
          (recur (quot n b)
                 (conj r (mod n b)))))))

  )

(= [1 2 3 4 5 0 1] (in-base 1234501 10))
