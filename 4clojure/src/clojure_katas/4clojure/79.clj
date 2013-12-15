(ns clojure-katas.4clojure.79)

(def triangle-path

  (fn [t]
    (loop [curr (first t)
           more (rest t)]
      (if-not (seq more)
        (apply min curr)
        (recur
          (vec (map-indexed
                 (fn [i e]
                   (+ e (min
                          (get curr (- i 1) Integer/MAX_VALUE)
                          (get curr i Integer/MAX_VALUE))))
                 (first more)))
          (rest more)))))

  )

(= 7 (triangle-path '([1]
          [2 4]
         [5 1 4]
        [2 3 4 5]))) ; 1->2->1->3

(= 20 (triangle-path '([3]
           [2 4]
          [1 9 3]
         [9 9 2 4]
        [4 6 6 7 8]
       [5 7 3 5 1 4]))) ; 3->4->3->2->7->1

