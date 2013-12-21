(ns clojure-katas.4clojure.110
  "Sequence of pronunciations

  Write a function that returns a lazy sequence of \"pronunciations\" of a
  sequence of numbers. A pronunciation of each element in the sequence
  consists of the number of repeating identical numbers and the number itself.
  For example, [1 1] is pronounced as [2 1] (\"two ones\"), which in turn is
  pronounced as [1 2 1 1] (\"one two, one one\").

  Your function should accept an initial sequence of numbers, and return an
  infinite lazy sequence of pronunciations, each element being a pronunciation
  of the previous element.")

(def __

  (fn f [s]
    (letfn [(pronounce [s]
              (loop [s s
                     p []]
                (if-not (seq s)
                  p
                  (let [x (first s)
                        n (count (take-while #(= x %) s))]
                    (recur (drop n s)
                           (conj (conj p n) x))))))]
      (lazy-seq
        (let [p (pronounce s)]
          (cons p (f p))))))

  )

(= [[1 1] [2 1] [1 2 1 1]] (take 3 (__ [1])))
(= [3 1 2 4] (first (__ [1 1 1 4 4])))
(= [1 1 1 3 2 1 3 2 1 1] (nth (__ [1]) 6))
(= 338 (count (nth (__ [3 2]) 15)))
