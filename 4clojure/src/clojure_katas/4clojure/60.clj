(ns clojure-katas.4clojure.60
  "Sequence Reductions

  Write a function which behaves like reduce, but returns each intermediate
  value of the reduction.  Your function must accept either two or three
  arguments, and the return sequence must be lazy.")

(def __

  (fn rs
    ([f s]
     (rs f (first s) (rest s)))
    ([f x s]
     (lazy-seq
       (cons x (if (empty? s)
                 ()
                 (rs f (f x (first s)) (rest s)))))))

  )

(= (take 5 (__ + (range))) [0 1 3 6 10])
(= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
(= (last (__ * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)
