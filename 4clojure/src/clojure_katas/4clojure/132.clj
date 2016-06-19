(ns clojure-katas.4clojure.132
  "Insert between two items

  Write a function that takes a two-argument predicate, a value, and a
  collection; and returns a new collection where the value is inserted between
  every two items that satisfy the predicate.")

(def __
  (fn __ [f v s]
    (lazy-seq
      (cond
        (not (seq s))
        nil

        (not (seq (rest s)))
        (cons (first s) (__ f v (rest s)))

        (f (first s) (second s))
        (cons (first s) (cons v (__ f v (rest s))))

        :else
        (cons (first s) (__ f v (rest s)))))))

(comment
  (= '(1 :less 6 :less 7 4 3) (__ < :less [1 6 7 4 3]))
  (= '(2) (__ > :more [2]))
  (= [0 1 :x 2 :x 3 :x 4]  (__ #(and (pos? %) (< % %2)) :x (range 5)))
  (empty? (__ > :more ()))
  (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
     (take 12 (->> [0 1]
                   (iterate (fn [[a b]] [b (+ a b)]))
                   (map first) ; fibonacci numbers
                   (__ (fn [a b] ; both even or both odd
                         (= (mod a 2) (mod b 2)))
                       :same)))))
